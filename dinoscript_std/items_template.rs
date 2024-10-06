use std::{ops::ControlFlow, sync::Arc};
use dinoscript_core::{
    bytecode::{Command, SourceId}, compilation_scope::{
        self,
        ty::{BuiltinTemplate, GenericSetId, TemplateGenericSpecs, Ty, TyTemplate},
        CompilationScope, NamedItem, NamedType,
    }, dinobj::{DinObject, DinoResult, SourceFnResult, TailCallAvailability}, dinopack::utils::{SetupFunction, SetupFunctionBody, SetupItem, Signature, SignatureGen}, errors::RuntimeError, maybe_owned::MaybeOwned, sequence::Sequence
};
// pragma: skip 2
use std::collections::HashMap;
use dinoscript_core::bytecode::to_in_code;

pub struct Builtins<'s> {
    pub int: Arc<Ty<'s>>,
    pub float: Arc<Ty<'s>>,
    pub bool: Arc<Ty<'s>>,
    pub str: Arc<Ty<'s>>,
    pub Sequence: Arc<TyTemplate<'s>>,
}

impl<'s> compilation_scope::Builtins<'s> for Builtins<'s> {
    fn int(&self) -> Arc<Ty<'s>> {
        self.int.clone()
    }

    fn float(&self) -> Arc<Ty<'s>> {
        self.float.clone()
    }

    fn bool(&self) -> Arc<Ty<'s>> {
        self.bool.clone()
    }

    fn str(&self) -> Arc<Ty<'s>> {
        self.str.clone()
    }

    fn sequence(&self, ty: Arc<Ty<'s>>) -> Arc<Ty<'s>> {
        self.Sequence.instantiate(vec![ty])
    }
}

macro_rules! get_type {
    ($b:ident, $gens:expr, $ty_name:ident) => {
        $b.$ty_name.clone()
    };
    ($b:ident, $gens:expr, $g_id:literal) => {
        $gens[$g_id].clone()
    };
    ($b:ident, $gens:expr, ($ty_name:ident<$($gen_params:tt),*>)) => {
        $b.$ty_name.instantiate(vec![$(get_type!($b, $gens, $gen_params)),*])
    };
}

macro_rules! signature_fn {
    (fn $name:ident <$($gen_name:ident),*> ($($param_name:ident : $type_name:tt),*) -> $ret_type:tt) => {
        {
            |b: &Builtins<'_>| {
                let gen_id = dinoscript_core::compilation_scope::ty::GenericSetId::unique();
                let gen_names = vec![$(std::borrow::Cow::Borrowed(stringify!($gen_name))),*];
                let gens: Vec<Arc<Ty<'_>>> = gen_names.iter().enumerate().map(|(i, _)| Arc::new(dinoscript_core::compilation_scope::ty::Ty::Generic(dinoscript_core::compilation_scope::ty::Generic::new(i, gen_id)))).collect();
                let gen = SignatureGen::new(gen_id, gen_names);
                Signature::new(
                    std::borrow::Cow::Borrowed(stringify!($name)),
                    Some(gen),
                    vec![$(dinoscript_core::dinopack::utils::Arg::new(std::borrow::Cow::Borrowed(stringify!($param_name)), get_type!(b, gens, $type_name))),*],
                    get_type!(b, gens, $ret_type),
                )
            }
        }
    };
    (fn $name:ident ($($param_name:ident : $type_name:tt),*) -> $ret_type:tt) => {
        |b: &Builtins<'_>| Signature::new(
            std::borrow::Cow::Borrowed(stringify!($name)),
            None,
            vec![$(dinoscript_core::dinopack::utils::Arg::new(std::borrow::Cow::Borrowed(stringify!($param_name)), get_type!(b, (), $type_name))),*],
            get_type!(b, (), $ret_type),
        )
    };
}

// pragma:replace-start
pub(crate) struct ItemsBuilder<'p, 's> {
    scope: CompilationScope<'p, 's, Builtins<'s>>,
    source_id: SourceId,
    next_id: usize,
    pub(crate) replacements: HashMap<&'static str, String>
}

fn get_signature_from_code(code: &'static str)->&'static str{
    let mut paren_depth = 0;
    let mut search_start = 0;
    loop {
        let next_open = code[search_start..].find('(').map(|i| i + search_start);
        if paren_depth == 0 {
            // if there is a bracket before the next paren, then we have it
            let next_bracket = code[search_start..].find('{').unwrap() + search_start;
            if next_open.is_none() || next_open.unwrap() > next_bracket {
                return &code[..next_bracket];
            }
        }
        else {
            let next_close = code[search_start..].find(')').map(|i| i + search_start);
            if next_close.is_none() {
                panic!("Unmatched parenthesis in code: {}", code);
            }
            if next_open.is_none() || next_open.unwrap() > next_close.unwrap() {
                paren_depth -= 1;
                search_start = next_close.unwrap() + 1;
                continue;
            }
        }
        paren_depth += 1;
        search_start = next_open.unwrap() + 1;
    }
}

impl<'p, 's> ItemsBuilder<'p, 's> {
    fn add_item<'c>(&'c mut self, item: SetupItem<'s, Builtins<'s>>) -> () {
        item.push_to_compilation(&mut self.scope, self.source_id, || {
            let ret = self.next_id;
            self.next_id += 1;
            ret
        });
    }

    fn build_source<'c>(&'c mut self, replacement_name: &'static str, code: &'static str) -> () {
        let func_stmt = dinoscript_core::grammar::parse_raw_function(code).unwrap();
        let mut sink = Vec::new();
        if let Err(err) =self.scope.feed_statement(&func_stmt, &mut sink){
            panic!("compilation error (building {}): {}", replacement_name, err);
        }
        // the sink should now have two items, the first is the actual function, the second is popping it to some cell (that we don't care about)
        // in the future, the sink might also include default values and the like, but it will never include captures since these are globals functions
        assert!(matches!(sink.pop(), Some(Command::PopToCell(_))));  // remove the pop command
        let Command::MakeFunction(mk_func) = sink.pop().unwrap() else { panic!("Expected a MakeFunction command") };
        assert!(mk_func.n_captures == 0);
        let n_cells = mk_func.n_cells;
        let commands = to_in_code(&mk_func.commands, "                    ");
        // now we need the signature, we could re-create it from the statement, but it's easier (for now at least) to just get it from the code
        let signature = get_signature_from_code(code);
        let replacement = format!("SetupItem::Function(SetupFunction::new(
            signature_fn!({signature}),{{
                static LZ: std::sync::LazyLock<Vec<Command>> = std::sync::LazyLock::new(|| {{
                    {commands}
                }});
                SetupFunctionBody::User(dinoscript_core::dinobj::UserFn::without_capture({n_cells}, &LZ))
            }}
        ))\n");
        assert!(self.replacements.insert(replacement_name, replacement).is_none());
    }
}

fn prepare_build<'p>()->ItemsBuilder<'p, 'static>{
    let mut scope = CompilationScope::root();
    let builtins = pre_items_setup(&mut scope);
    scope.builtins = Some(MaybeOwned::Owned(builtins));
    ItemsBuilder{scope, source_id: SOURCE_ID, next_id: 0, replacements: HashMap::new()}
}
// pragma:replace-with-raw
// pragma:replace-end

pub(crate) const SOURCE_ID: SourceId = "core";
pub(crate) fn pre_items_setup<'p, 's>(scope: &mut CompilationScope<'p, 's, Builtins<'s>>)->Builtins<'s>{
    fn register_type<'p, 's, B>(
        scope: &mut CompilationScope<'p, 's, B>,
        name: &'s str,
        template: TyTemplate<'s>,
    ) -> Arc<TyTemplate<'s>> {
        let template = Arc::new(template);
        scope
        .names
        .insert(name.into(), NamedItem::Type(NamedType::Template(template.clone())));
        template
    }

    let int = register_type(scope, "int", TyTemplate::Builtin(BuiltinTemplate::primitive("int"))).instantiate(vec![]);
    let float = register_type(scope, "float", TyTemplate::Builtin(BuiltinTemplate::primitive("float"))).instantiate(vec![]);
    let bool = register_type(scope, "bool", TyTemplate::Builtin(BuiltinTemplate::primitive("bool"))).instantiate(vec![]);
    let str = register_type(scope, "str", TyTemplate::Builtin(BuiltinTemplate::primitive("str"))).instantiate(vec![]);
    let sequence = register_type(scope, "Sequence", TyTemplate::Builtin(BuiltinTemplate::new(
        "Sequence",
        TemplateGenericSpecs::new(GenericSetId::unique(), 1)
    )));

    Builtins { int, float, bool, str, Sequence: sequence }
}

fn to_return_value<'s>(result: DinoResult<'s>)->SourceFnResult<'s>{
    match result {
        Ok(v) => Ok(ControlFlow::Break(v)),
        Err(e) => Err(e),
    }
}

macro_rules! unwrap_value {
    ($evaled:expr) => {
        match $evaled {
            Ok(v) => v,
            Err(e) => return to_return_value(Ok(Err(e))),
        }
    };
}

macro_rules! as_prim {
    ($ref:expr, $variant:ident) => {
        if let DinObject::$variant(v) = $ref.as_ref() {
            v
        } else {
            let err = format!("Expected {} got {:?}", stringify!($variant), $ref);
            return to_return_value(Ok($ref.runtime().allocate(Err(err.into()))?));
        }
    };
}

macro_rules! as_ext {
    ($ref:expr, $ty:ident) => {
        {
            let ptr = (*as_prim!($ref, Extended)) as *const $ty;
            unsafe{&*ptr}
        }
    };
}

pub(crate)
fn setup_items<'a, 's>()->
// pragma:replace-start
ItemsBuilder<'a, 's>
// pragma:replace-with-raw
/*
 Vec<SetupItem<'s, Builtins<'s>>>
*/
// pragma:replace-end
{
    // pragma:skip 1
    let mut builder = prepare_build();
    vec![
        // region:int
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn add (a: int, b: int) -> int),
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = unwrap_value!(frame.eval_pop()?);
                    let b = unwrap_value!(frame.eval_pop()?);

                    let a = as_prim!(a, Int);
                    let b = as_prim!(b, Int);
                    to_return_value(frame.runtime().allocate(Ok(DinObject::Int(a + b))))
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn div (a: int, b: int) -> float),
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = unwrap_value!(frame.eval_pop()?);
                    let b = unwrap_value!(frame.eval_pop()?);

                    let a = as_prim!(a, Int);
                    let b = as_prim!(b, Int);
                    to_return_value(if *b == 0{
                        frame.runtime().allocate(Err("Division by zero".into()))
                    } else {
                        frame.runtime().allocate(Ok(DinObject::Float((*a as f64) / (*b as f64))))
                    })
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn eq (a: int, b: int) -> bool),
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = unwrap_value!(frame.eval_pop()?);
                    let b = unwrap_value!(frame.eval_pop()?);

                    let a = as_prim!(a, Int);
                    let b = as_prim!(b, Int);
                    to_return_value(frame.runtime().bool(a == b))
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn gte (a: int, b: int) -> bool),
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = unwrap_value!(frame.eval_pop()?);
                    let b = unwrap_value!(frame.eval_pop()?);

                    let a = as_prim!(a, Int);
                    let b = as_prim!(b, Int);
                    to_return_value(frame.runtime().bool(a >= b))
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn mod (a: int, b: int) -> int),
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = unwrap_value!(frame.eval_pop()?);
                    let b = unwrap_value!(frame.eval_pop()?);

                    let a = as_prim!(a, Int);
                    let b = as_prim!(b, Int);
                    to_return_value(if *b == 0{
                        frame.runtime().allocate(Err("Division by zero".into()))
                    } else {
                        frame.runtime().allocate(Ok(DinObject::Int(a % b)))
                    })
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn mul (a: int, b: int) -> int),
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = unwrap_value!(frame.eval_pop()?);
                    let b = unwrap_value!(frame.eval_pop()?);

                    let a = as_prim!(a, Int);
                    let b = as_prim!(b, Int);
                    to_return_value(frame.runtime().allocate(Ok(DinObject::Int(a * b))))
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn neg (a: int) -> int),
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = unwrap_value!(frame.eval_pop()?);

                    let a = as_prim!(a, Int);
                    to_return_value(frame.runtime().allocate(Ok(DinObject::Int(-a))))
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn sub (a: int, b: int) -> int),
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = unwrap_value!(frame.eval_pop()?);
                    let b = unwrap_value!(frame.eval_pop()?);

                    let a = as_prim!(a, Int);
                    let b = as_prim!(b, Int);
                    to_return_value(frame.runtime().allocate(Ok(DinObject::Int(a - b))))
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn to_str (a: int) -> str),
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = unwrap_value!(frame.eval_pop()?);

                    let a = as_prim!(a, Int);
                    to_return_value(frame.runtime().allocate(Ok(DinObject::Str(format!("{}", a).into()))))
                })),
            ))
        )
        ,
        // endregion
        // region:bool
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn and (a: bool, b: bool) -> bool),
                SetupFunctionBody::System(Box::new(|frame| {
                    let a_ref = unwrap_value!(frame.eval_pop()?);

                    let a = as_prim!(a_ref, Bool);
                    if !a{
                        to_return_value(Ok(Ok(a_ref)))
                    }
                    else{
                        frame.eval_pop_tca(TailCallAvailability::Allowed)
                    }
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn assert (a: bool) -> bool),
                SetupFunctionBody::System(Box::new(|frame| {
                    // todo: if the value is a pending, maybe we can add it to the error message?
                    let a_ref = unwrap_value!(frame.eval_pop()?);

                    let a = as_prim!(a_ref, Bool);
                    to_return_value(if *a{
                        Ok(Ok(a_ref))
                    }
                    else{
                        frame.runtime().allocate(Err("Assertion is false".into()))
                    })
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn if <T> (b: bool, t: 0, e: 0) -> 0),
                SetupFunctionBody::System(Box::new(|frame| {
                    let b = unwrap_value!(frame.eval_pop()?);

                    let b = as_prim!(b, Bool);
                    
                    if !b {
                        frame.stack.pop().unwrap();
                    }
                    frame.eval_pop_tca(TailCallAvailability::Allowed)
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn eq (a: bool, b: bool) -> bool),
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = unwrap_value!(frame.eval_pop()?);
                    let b = unwrap_value!(frame.eval_pop()?);

                    let a = as_prim!(a, Bool);
                    let b = as_prim!(b, Bool);

                    to_return_value(frame.runtime().bool(a == b))
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn not (a: bool) -> bool),
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = unwrap_value!(frame.eval_pop()?);

                    let a = as_prim!(a, Bool);
                    to_return_value(frame.runtime().allocate(Ok(DinObject::Bool(!a))))
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn or (a: bool, b: bool) -> bool),
                SetupFunctionBody::System(Box::new(|frame| {
                    let a_ref = unwrap_value!(frame.eval_pop()?);

                    let a = as_prim!(a_ref, Bool);
                    if *a{
                        to_return_value(Ok(Ok(a_ref)))
                    }
                    else{
                        frame.eval_pop_tca(TailCallAvailability::Allowed)
                    }
                })),
            ))
        )
        ,
        // endregion bool
        // region:str
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn eq (a: str, b: str) -> bool),
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = unwrap_value!(frame.eval_pop()?);
                    let b = unwrap_value!(frame.eval_pop()?);

                    let a = as_prim!(a, Str);
                    let b = as_prim!(b, Str);
                    
                    to_return_value(frame.runtime().bool(a == b))
                })),
            ))
        )
        ,
        // endregion str
        // region:float
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn floor (a: float) -> int),
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = unwrap_value!(frame.eval_pop()?);

                    let a = as_prim!(a, Float);
                    to_return_value(frame.runtime().allocate(Ok(DinObject::Int(a.floor() as i64))))
                })),
            ))
        )
        ,
        // endregion float
        // region:sequence
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn lookup<T> (a: (Sequence<0>), idx: int) -> 0),
                SetupFunctionBody::System(Box::new(|frame| {
                    let seq = unwrap_value!(frame.eval_pop()?);
                    let idx = unwrap_value!(frame.eval_pop()?);

                    let seq = as_ext!(seq, Sequence);
                    let idx = as_prim!(idx, Int);
                    let Ok(idx) = usize::try_from(*idx) else {
                        return to_return_value(frame.runtime().allocate(Err("Index out of range".into())));
                    };
                    let Some(item_ref) = seq.get(idx) else {
                        return to_return_value(frame.runtime().allocate(Err("Index out of range".into())));
                    };
                    let ret = frame.runtime().clone_ref(item_ref)?;
                    to_return_value(Ok(Ok(ret)))
                })),
            ))
        )
        ,
        // endregion sequence
        // pragma:replace-start
        builder.build_source(
            // pragma:replace-id
            "abs",
            r#"fn abs(x: int)->int{
                if(x>=0, x, -x)
            }"#
        )
        // pragma:replace-end
    ]
    // pragma:skip 1
    ; builder

}