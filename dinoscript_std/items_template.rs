use std::sync::Arc;
use dinoscript_core::{
    bytecode::{Command, SourceId},
    compilation_scope::{
        self,
        ty::{BuiltinTemplate, Ty, TyTemplate},
        CompilationScope, NamedItem, NamedType,
    },
    dinobj::{DinObject},
    dinopack::utils::{SetupFunction, SetupFunctionBody, SetupItem, Signature, SignatureGen},
};
// pragma: skip 2
use std::collections::HashMap;
use dinoscript_core::bytecode::to_in_code;

pub struct Builtins<'s> {
    pub int: Arc<Ty<'s>>,
    pub float: Arc<Ty<'s>>,
    pub bool: Arc<Ty<'s>>,
    pub str: Arc<Ty<'s>>,
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
}

macro_rules! get_type {
    ($b:ident, $gens:expr, $ty_name:ident) => {
        $b.$ty_name.clone()
    };
    ($b:ident, $gens:expr, $g_id:literal) => {
        $gens[$g_id].clone()
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
    builtins: Builtins<'s>,
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
    fn add_item<'c>(&'c mut self, item: SetupItem<'s, &'c Builtins<'s>>) -> () {
        let builtins = &self.builtins;
        item.push_to_compilation(&mut self.scope, builtins, self.source_id, || {
            let ret = self.next_id;
            self.next_id += 1;
            ret
        });
    }

    fn build_source<'c>(&'c mut self, replacement_name: &'static str, code: &'static str) -> () {
        let func_stmt = dinoscript_core::grammar::parse_raw_function(code).unwrap();
        let mut sink = Vec::new();
        self.scope.feed_statement(&func_stmt, &mut sink).unwrap();
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
    ItemsBuilder{scope, builtins, source_id: SOURCE_ID, next_id: 0, replacements: HashMap::new()}
}
// pragma:replace-with-raw
// pragma:replace-end

pub(crate) const SOURCE_ID: SourceId = "core";
pub(crate) fn pre_items_setup<'p, 's>(scope: &mut CompilationScope<'p, 's, Builtins<'s>>)->Builtins<'s>{
    fn register_type<'p, 's, B>(
        scope: &mut CompilationScope<'p, 's, B>,
        name: &'s str,
        template: TyTemplate<'s>,
    ) -> Arc<Ty<'s>> {
        let template = Arc::new(template);
        let ret = template.instantiate(vec![]);
        scope
            .names
            .insert(name.into(), NamedItem::Type(NamedType::Template(template)));
        ret
    }

    let int = register_type(scope, "int", TyTemplate::Builtin(BuiltinTemplate::primitive("int")));
    let float = register_type(scope, "float", TyTemplate::Builtin(BuiltinTemplate::primitive("float")));
    let bool = register_type(scope, "bool", TyTemplate::Builtin(BuiltinTemplate::primitive("bool")));
    let str = register_type(scope, "str", TyTemplate::Builtin(BuiltinTemplate::primitive("str")));

    Builtins { int, float, bool, str }
}

pub(crate)
fn setup_items<'a, 's>()->
// pragma:replace-start
ItemsBuilder<'a, 's>
// pragma:replace-with-raw
/*
 Vec<SetupItem<'s, &'a Builtins<'s>>>
*/
// pragma:replace-end
{
    // pragma:skip 1
    let mut builder = prepare_build();
    vec![
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn add (a: int, b: int) -> int),
                SetupFunctionBody::System(Box::new(|frame| {
                    let Ok(a) = frame.eval_pop()? else {
                        todo!()
                    };
                    let Ok(b) = frame.eval_pop()? else {
                        todo!()
                    };
                    match (a.as_ref(), b.as_ref()) {
                        (DinObject::Int(a), DinObject::Int(b)) => frame.runtime().allocate(Ok(DinObject::Int(a + b))),
                        _ => Ok(Err(())),
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
                    let Ok(a) = frame.eval_pop()? else {
                        todo!()
                    };
                    match a.as_ref() {
                        DinObject::Bool(true) => Ok(Ok(frame.runtime().bool(true)?)),
                        _ => Ok(Err(())),
                    }
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn eq (a: int, b: int) -> bool),
                SetupFunctionBody::System(Box::new(|frame| {
                    let Ok(a) = frame.eval_pop()? else {
                        todo!()
                    };
                    let Ok(b) = frame.eval_pop()? else {
                        todo!()
                    };
                    match (a.as_ref(), b.as_ref()) {
                        (DinObject::Int(a), DinObject::Int(b)) => Ok(Ok(frame.runtime().bool(a==b)?)),
                        _ => Ok(Err(())),
                    }
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn mul (a: int, b: int) -> int),
                SetupFunctionBody::System(Box::new(|frame| {
                    let Ok(a) = frame.eval_pop()? else {
                        todo!()
                    };
                    let Ok(b) = frame.eval_pop()? else {
                        todo!()
                    };
                    match (a.as_ref(), b.as_ref()) {
                        (DinObject::Int(a), DinObject::Int(b)) => frame.runtime().allocate(Ok(DinObject::Int(a * b))),
                        _ => Ok(Err(())),
                    }
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn mod (a: int, b: int) -> int),
                SetupFunctionBody::System(Box::new(|frame| {
                    let Ok(a) = frame.eval_pop()? else {
                        todo!()
                    };
                    let Ok(b) = frame.eval_pop()? else {
                        todo!()
                    };
                    match (a.as_ref(), b.as_ref()) {
                        (DinObject::Int(a), DinObject::Int(b)) => {
                            if *b == 0{
                                return Ok(Err(()));
                            }
                            frame.runtime().allocate(Ok(DinObject::Int(a % b)))
                        },
                        _ => Ok(Err(())),
                    }
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn to_str (a: int) -> str),
                SetupFunctionBody::System(Box::new(|frame| {
                    let Ok(a) = frame.eval_pop()? else {
                        todo!()
                    };
                    match a.as_ref() {
                        DinObject::Int(a) => frame.runtime().allocate(Ok(DinObject::Str(format!("{}", a).into()))),
                        _ => Ok(Err(())),
                    }
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn if <T> (b: bool, t: 0, e: 0) -> 0),
                SetupFunctionBody::System(Box::new(|frame| {
                    let Ok(b) = frame.eval_pop()? else {
                        todo!()
                    };
                    if let DinObject::Bool(false) = b.as_ref() {
                        let popped = frame.stack.pop().unwrap();
                        println!("popping {:?}", popped);
                    }
                    frame.eval_pop()
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn and (a: bool, b: bool) -> bool),
                SetupFunctionBody::System(Box::new(|frame| {
                    let Ok(a) = frame.eval_pop()? else {
                        todo!()
                    };
                    match a.as_ref() {
                        DinObject::Bool(false) => {
                            frame.stack.pop().unwrap();
                            Ok(Ok(frame.runtime().bool(false)?))
                        },
                        DinObject::Bool(true) => frame.eval_pop(), // todo tail call
                        _ => Ok(Err(())),
                    }
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn eq (a: bool, b: bool) -> bool),
                SetupFunctionBody::System(Box::new(|frame| {
                    let Ok(a) = frame.eval_pop()? else {
                        todo!()
                    };
                    let Ok(b) = frame.eval_pop()? else {
                        todo!()
                    };
                    match (a.as_ref(), b.as_ref()) {
                        (DinObject::Bool(a), DinObject::Bool(b)) => Ok(Ok(frame.runtime().bool(a==b)?)),
                        _ => Ok(Err(())),
                    }
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                signature_fn!(fn eq (a: str, b: str) -> bool),
                SetupFunctionBody::System(Box::new(|frame| {
                    let Ok(a) = frame.eval_pop()? else {
                        todo!()
                    };
                    let Ok(b) = frame.eval_pop()? else {
                        todo!()
                    };
                    match (a.as_ref(), b.as_ref()) {
                        (DinObject::Str(a), DinObject::Str(b)) => Ok(Ok(frame.runtime().bool(a==b)?)),
                        _ => Ok(Err(())),
                    }
                })),
            ))
        )
        ,
        // pragma:replace-start
        builder.build_source(
            // pragma:replace-id
            "abs",
            r#"fn abs(x: int)->int{
                x
            }"#
        )
        // pragma:replace-end
    ]
    // pragma:skip 1
    ; builder

}