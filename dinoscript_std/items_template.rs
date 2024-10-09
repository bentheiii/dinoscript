use std::{ops::ControlFlow, sync::Arc};
use dinoscript_core::{
    ast::statement::{FnArgDefault, ResolveOverload, Stmt}, bytecode::{Command, SourceId}, compilation_scope::{
        self, ty::{BuiltinTemplate, Fn, Generic, GenericSetId, TemplateGenericSpecs, Ty, TyTemplate}, CompilationScope, NamedItem, NamedType
    }, dinobj::{DinObject, DinoResult, SourceFnResult, TailCallAvailability}, dinopack::utils::{Arg, SetupFunction, SetupFunctionBody, SetupItem, Signature, SignatureGen}, maybe_owned::MaybeOwned, sequence::Sequence
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

impl<'s> Builtins<'s> {
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
    ($b:ident, $gens:expr, T0) => {
        $gens[0].clone()
    };
    ($b:ident, $gens:expr, T1) => {
        $gens[0].clone()
    };
    ($b:ident, $gens:expr, $ty_name:ident) => {
        $b.$ty_name.clone()
    };
    ($b:ident, $gens:expr, ($ty_name:ident<$($gen_params:tt),*>)) => {
        $b.$ty_name.instantiate(vec![$(get_type!($b, $gens, $gen_params)),*])
    };
    ($b:ident, $gens:expr, ($($params:tt),*)->($out:tt)) => {
        Arc::new(Ty::Fn(Fn::new(vec![$(get_type!($b, $gens, $params)),*], get_type!($b, $gens, $out))))
    }
}

macro_rules! arg_suffix {
    ($b:ident, $gens:expr, $name:ident, ($type_name:tt ~= $resolv:ident)) => {
        dinoscript_core::dinopack::utils::Arg::with_default_resolve(std::borrow::Cow::Borrowed(stringify!($param_name)), get_type!($b, $gens, $type_name), std::borrow::Cow::Borrowed(stringify!($resolv)))
    };

    ($b:ident, $gens:expr, $name:ident, $type_name:tt) => {
        dinoscript_core::dinopack::utils::Arg::new(std::borrow::Cow::Borrowed(stringify!($param_name)), get_type!($b, $gens, $type_name))
    };
}

macro_rules! ty_gen {
    ($b:ident, $g:expr, int) => {
        $b.int()
    };
    ($b:ident, $g:expr, bool) => {
        $b.bool()
    };
    ($b:ident, $g:expr, float) => {
        $b.float()
    };
    ($b:ident, $g:expr, str) => {
        $b.str()
    };
    ($b:ident, $g:expr, Sequence<$ty:tt>) => {
        $b.sequence(ty_gen!($b, $g, $ty))
    };
    ($b:ident, $g:expr, $id:ident) => {
        $g.get_gen(stringify!($id))
    };
    ($b:ident, $g:expr, ($($args:tt),*)->($ret:tt)) => {
        Arc::new(Ty::Fn(Fn::new(vec![$(ty_gen!($b, $g, $args)),*], ty_gen!($b, $g, $ret))))
    };
}

macro_rules! ty {
    ($b:ident, $ty:tt) => {
        ty_gen!($b, (), $ty)
    };
}

macro_rules! arg {
    ($b:ident, $name:ident: $($ty_parts:tt)*) => {
        Arg::new(stringify!($name), ty!($b, $($ty_parts)*))
        
    };
}

macro_rules! arg_gen { 
    ($b:ident, $g:expr, $name:ident: $($ty_parts:tt)*) => {
        Arg::new(stringify!($name), ty_gen!($b, $g, $($ty_parts)*))
    };
    ($b:ident, $g:expr, $name:ident~=$resolv:ident: $($ty_parts:tt)*) => {
        Arg::with_default_resolve(stringify!($name), ty_gen!($b, $g, $($ty_parts)*), stringify!($resolv))
    };
}

macro_rules! signature_fn {
    (fn $name:ident <$($gen_name:ident),*> ($($param_name:ident : $arg_suf:tt),*) -> $ret_type:tt) => {
        {
            |b: &Builtins<'_>| {
                let gen_id = dinoscript_core::compilation_scope::ty::GenericSetId::unique();
                let gen_names = vec![$(std::borrow::Cow::Borrowed(stringify!($gen_name))),*];
                let gens: Vec<Arc<Ty<'_>>> = gen_names.iter().enumerate().map(|(i, _)| Arc::new(dinoscript_core::compilation_scope::ty::Ty::Generic(dinoscript_core::compilation_scope::ty::Generic::new(i, gen_id)))).collect();
                let gen = SignatureGen::new(gen_names);
                Signature::new(
                    std::borrow::Cow::Borrowed(stringify!($name)),
                    Some(gen),
                    vec![$(arg_suffix!(b, gens, $param_name, $arg_suf)),*],
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

fn signature_code_from_statement<'s>(stmt: &Stmt<'s>)->String{
    let Stmt::Fn(func) = stmt else {
        panic!("expected function statement, got {:?}", stmt);
    };
    let name = func.name.as_ref();
    let generic_params = &func.generic_params;
    let args = &func.args;
    let ret = &func.return_ty;

    let gen_params_tokens = generic_params.iter().map(|p| format!("\"{}\"", p)).collect::<Vec<_>>().join(", ");
    let name_token = format!("\"{}\"", name);
    let mut args_tokens = Vec::with_capacity(args.len());
    for arg in args {
        let name = arg.name.as_ref();
        let ty = &arg.ty;
        args_tokens.push(match arg.default.as_ref() {
            Some(FnArgDefault::ResolveOverload(ResolveOverload{name: resolv})) => {
                format!("arg_gen!(bi, gen, {}~={}: {})", name, resolv, ty.inner.to_in_code())
            }
            Some(_) => {panic!()}
            None => {
                format!("arg_gen!(bi, gen, {}: {})", name, ty.inner.to_in_code())
            }
        })
    }
    let args_tokens = args_tokens.join(",\n                ");
    let ret_token = ret.inner.to_in_code();
    format!("
    |bi: &Builtins<'_>| {{
        let gen = SignatureGen::new(vec![{gen_params_tokens}] as Vec<&'static str>);
        Signature::new_generic(
            {name_token},
            vec![
                {args_tokens}
            ],
            ty_gen!(bi, gen, {ret_token}),
            gen,
        )
    }}
    ")
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
        assert!(sink.is_empty());
        assert!(mk_func.n_captures == 0);
        let n_cells = mk_func.n_cells;
        let commands = to_in_code(&mk_func.commands, "                    ");
        // now we need the signature, we could re-create it from the statement, but it's easier (for now at least) to just get it from the code
        let signature = signature_code_from_statement(&func_stmt.inner);
        let replacement = format!("SetupItem::Function(SetupFunction::new(
            {signature},
            {{
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
            let ptr = (*as_prim!($ref, Extended));
            let tn = (unsafe{&*ptr}).type_name();
            if tn != $ty::EXPECTED_TYPE_NAME {
                let err = format!("Expected {} got {:?}", $ty::EXPECTED_TYPE_NAME, tn);
                return to_return_value(Ok($ref.runtime().allocate(Err(err.into()))?));
            } 
            let ptr = ptr as *const $ty;
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
                |bi: &Builtins<'_>| {
                    Signature::new(
                        "add",
                        vec![arg!(bi, a: int), arg!(bi, b: int)],
                        ty!(bi, int),
                    )
                },
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
                |bi: &Builtins<'_>| {
                    Signature::new(
                        "div",
                        vec![arg!(bi, a: int), arg!(bi, b: int)],
                        ty!(bi, float),
                    )
                },
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
                |bi: &Builtins<'_>| {
                    Signature::new(
                        "eq",
                        vec![arg!(bi, a: int), arg!(bi, b: int)],
                        ty!(bi, bool),
                    )
                },
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
                |bi: &Builtins<'_>| {
                    Signature::new(
                        "gte",
                        vec![arg!(bi, a: int), arg!(bi, b: int)],
                        ty!(bi, bool),
                    )
                },
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
                |bi: &Builtins<'_>| {
                    Signature::new(
                        "mod",
                        vec![arg!(bi, a: int), arg!(bi, b: int)],
                        ty!(bi, int),
                    )
                },
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
                |bi: &Builtins<'_>| {
                    Signature::new(
                        "mul",
                        vec![arg!(bi, a: int), arg!(bi, b: int)],
                        ty!(bi, int),
                    )
                },
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
                |bi: &Builtins<'_>| {
                    Signature::new(
                        "neg",
                        vec![arg!(bi, a: int)],
                        ty!(bi, int),
                    )
                },
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
                |bi: &Builtins<'_>| {
                    Signature::new(
                        "sub",
                        vec![arg!(bi, a: int), arg!(bi, b: int)],
                        ty!(bi, int),
                    )
                },
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
                |bi: &Builtins<'_>| {
                    Signature::new(
                        "to_str",
                        vec![arg!(bi, a: int)],
                        ty!(bi, str),
                    )
                },
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
                |bi: &Builtins<'_>| {
                    Signature::new(
                        "and",
                        vec![arg!(bi, a: bool), arg!(bi, b: bool)],
                        ty!(bi, bool),
                    )
                },
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
                |bi: &Builtins<'_>| {
                    Signature::new(
                        "assert",
                        vec![arg!(bi, a: bool)],
                        ty!(bi, bool),
                    )
                },
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
                |bi: &Builtins<'_>| {
                    let gen = SignatureGen::new(vec!["T"]);
                    Signature::new_generic(
                        "if",
                        vec![arg!(bi, b:bool), arg_gen!(bi, gen, t: T), arg_gen!(bi, gen, e: T)],
                        ty_gen!(bi, gen, T),
                        gen,
                    )
                },
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
                |bi: &Builtins<'_>| {
                    Signature::new(
                        "eq",
                        vec![arg!(bi, a: bool), arg!(bi, b: bool)],
                        ty!(bi, bool),
                    )
                },
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
                |bi: &Builtins<'_>| {
                    Signature::new(
                        "not",
                        vec![arg!(bi, a: bool)],
                        ty!(bi, bool),
                    )
                },
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
                |bi: &Builtins<'_>| {
                    Signature::new(
                        "or",
                        vec![arg!(bi, a: bool), arg!(bi, b: bool)],
                        ty!(bi, bool),
                    )
                },
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
                |bi: &Builtins<'_>| {
                    Signature::new(
                        "eq",
                        vec![arg!(bi, a: str), arg!(bi, b: str)],
                        ty!(bi, bool),
                    )
                },
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
                |bi: &Builtins<'_>| {
                    Signature::new(
                        "floor",
                        vec![arg!(bi, a: float)],
                        ty!(bi, int),
                    )
                },
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
                |bi: &Builtins<'_>| {
                    let gen = SignatureGen::new(vec!["T0", "T1"]);
                    Signature::new_generic(
                        "eq",
                        vec![
                            arg_gen!(bi, gen, a: Sequence<T0>), 
                            arg_gen!(bi, gen, b: Sequence<T1>), 
                            arg_gen!(bi, gen, fn_eq~=eq: (T0,T1)->(bool))
                        ],
                        ty!(bi, bool),
                        gen,
                    )
                },
                //signature_fn!(fn eq<T0, T1> (a: Sequence<T0>, b: Sequence<T1>, fn_eq: (T0,T1)->(bool)~=eq) -> bool),
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = unwrap_value!(frame.eval_pop()?);
                    let b = unwrap_value!(frame.eval_pop()?);
                    let fn_eq = unwrap_value!(frame.eval_pop()?);

                    let a = as_ext!(a, Sequence);
                    let b = as_ext!(b, Sequence);
                    if a.len() != b.len(){
                        return to_return_value(frame.runtime().bool(false));
                    }
                    for (a_it, b_it) in a.iter().zip(b.iter()){
                        let a_it = frame.runtime().clone_ref(a_it)?;
                        let b_it = frame.runtime().clone_ref(b_it)?;
                        let res = frame.call(&fn_eq, vec![a_it, b_it])?;
                        let res = unwrap_value!(res);
                        let res = as_prim!(res, Bool);
                        if !*res{
                            return to_return_value(frame.runtime().bool(false));
                        }
                    }
                    to_return_value(frame.runtime().bool(true))

                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                |bi: &Builtins<'_>| {
                    let gen = SignatureGen::new(vec!["T"]);
                    Signature::new_generic(
                        "lookup",
                        vec![
                            arg_gen!(bi, gen, seq: Sequence<T>), 
                            arg!(bi, idx: int), 
                        ],
                        ty_gen!(bi, gen, T),
                        gen,
                    )
                },
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
        // region:int-1
        // pragma:replace-start
        builder.build_source(
            // pragma:replace-id
            "abs",
            r#"fn abs(x: int)->int{
                if(x>=0, x, -x)
            }"#
        )
        // pragma:replace-end
        ,
        // endregion
        // region:generic-1
        // pragma:replace-start
        builder.build_source(
            // pragma:replace-id
            "neq",
            r#"fn neq<T0, T1>(x: T0, y: T1, fn_eq: (T0, T1)->(bool) ~= eq)->bool{
                !fn_eq(x, y)
            }"#
        )
        // pragma:replace-end
        ,
        // endregion

    ]
    // pragma:skip 1
    ; builder

}