use std::{ops::ControlFlow, sync::Arc};
use dinoscript_core::{
    bytecode::{Command, SourceId}, compilation_scope::{
        self, ty::{BuiltinTemplate, Fn, GenericSetId, TemplateGenericSpecs, Ty, TyTemplate}, CompilationScope, NamedItem, NamedType
    }, dinobj::{DinObject, DinoResult, SourceFnResult, TailCallAvailability}, dinopack::utils::{Arg, SetupFunction, SetupFunctionBody, SetupItem, Signature, SignatureGen}, sequence::{NormalizedIdx, Sequence}, stack::Stack
};
// pragma: skip 2
use std::collections::HashMap;
use dinoscript_core::{bytecode::to_in_code, ast::statement::{FnArgDefault, ResolveOverload, Stmt}, maybe_owned::MaybeOwned};

pub struct Builtins<'s> {
    pub int: Arc<Ty<'s>>,
    pub float: Arc<Ty<'s>>,
    pub bool: Arc<Ty<'s>>,
    pub str: Arc<Ty<'s>>,

    pub sequence: Arc<TyTemplate<'s>>,
    pub stack: Arc<TyTemplate<'s>>,
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
        self.sequence.instantiate(vec![ty])
    }

    fn stack(&self, ty: Arc<Ty<'s>>) -> Arc<Ty<'s>> {
        self.stack.instantiate(vec![ty])
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
        self.sequence.instantiate(vec![ty])
    }
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
    ($b:ident, $g:expr, Stack<$ty:tt>) => {
        $b.stack(ty_gen!($b, $g, $ty))
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

macro_rules! rt_unwrap_value {
    ($evaled:expr) => {
        match $evaled {
            Ok(v) => v,
            Err(e) => return to_return_value(Ok(Err(e))),
        }
    };
}

macro_rules! rt_as_prim {
    ($ref:expr, $variant:ident) => {
        if let DinObject::$variant(v) = $ref.as_ref() {
            v
        } else {
            let err = format!("Expected {} got {:?} (ln {})", stringify!($variant), $ref, line!());
            return to_return_value(Ok($ref.runtime().allocate(Err(err.into()))?));
        }
    };
}

macro_rules! rt_as_ext {
    ($ref:expr, $ty:ident) => {
        {
            let ptr = (*rt_as_prim!($ref, Extended));
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

// pragma:replace-start
pub(crate) struct ItemsBuilder<'p, 's> {
    scope: CompilationScope<'p, 's, Builtins<'s>>,
    source_id: SourceId,
    next_id: usize,
    pub(crate) replacements: HashMap<&'static str, String>
}

fn signature_code_from_statement(stmt: &Stmt<'_>)->String{
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
    fn add_item<'c>(&'c mut self, item: SetupItem<'s, Builtins<'s>>) {
        item.push_to_compilation(&mut self.scope, self.source_id, || {
            let ret = self.next_id;
            self.next_id += 1;
            ret
        });
    }

    fn build_source(&mut self, replacement_name: &'static str, code: &'static str) {
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

pub(crate) const SOURCE_ID: SourceId = "std";
pub(crate) fn pre_items_setup<'s>(scope: &mut CompilationScope<'_, 's, Builtins<'s>>)->Builtins<'s>{
    fn register_type<'s, B>(
        scope: &mut CompilationScope<'_, 's, B>,
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
    let stack = register_type(scope, "Stack", TyTemplate::Builtin(BuiltinTemplate::new(
        "Stack",
        TemplateGenericSpecs::new(GenericSetId::unique(), 1)
    )));

    Builtins { int, float, bool, str, sequence, stack }
}

fn to_return_value(result: DinoResult<'_>)->SourceFnResult<'_>{
    match result {
        Ok(v) => Ok(ControlFlow::Break(v)),
        Err(e) => Err(e),
    }
}



pub(crate)
// pragma:replace-start
fn setup_items<'a, 's>()->
ItemsBuilder<'a, 's>
// pragma:replace-with-raw
/*
fn setup_items<'s>()->
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
                    let a = rt_unwrap_value!(frame.eval_pop()?);
                    let b = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a, Int);
                    let b = rt_as_prim!(b, Int);
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
                    let a = rt_unwrap_value!(frame.eval_pop()?);
                    let b = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a, Int);
                    let b = rt_as_prim!(b, Int);
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
                    let a = rt_unwrap_value!(frame.eval_pop()?);
                    let b = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a, Int);
                    let b = rt_as_prim!(b, Int);
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
                        "gt",
                        vec![arg!(bi, a: int), arg!(bi, b: int)],
                        ty!(bi, bool),
                    )
                },
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = rt_unwrap_value!(frame.eval_pop()?);
                    let b = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a, Int);
                    let b = rt_as_prim!(b, Int);
                    to_return_value(frame.runtime().bool(a > b))
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
                    let a = rt_unwrap_value!(frame.eval_pop()?);
                    let b = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a, Int);
                    let b = rt_as_prim!(b, Int);
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
                        "lt",
                        vec![arg!(bi, a: int), arg!(bi, b: int)],
                        ty!(bi, bool),
                    )
                },
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = rt_unwrap_value!(frame.eval_pop()?);
                    let b = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a, Int);
                    let b = rt_as_prim!(b, Int);
                    to_return_value(frame.runtime().bool(a < b))
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                |bi: &Builtins<'_>| {
                    Signature::new(
                        "lte",
                        vec![arg!(bi, a: int), arg!(bi, b: int)],
                        ty!(bi, bool),
                    )
                },
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = rt_unwrap_value!(frame.eval_pop()?);
                    let b = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a, Int);
                    let b = rt_as_prim!(b, Int);
                    to_return_value(frame.runtime().bool(a <= b))
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
                    let a = rt_unwrap_value!(frame.eval_pop()?);
                    let b = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a, Int);
                    let b = rt_as_prim!(b, Int);
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
                    let a = rt_unwrap_value!(frame.eval_pop()?);
                    let b = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a, Int);
                    let b = rt_as_prim!(b, Int);
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
                    let a = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a, Int);
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
                    let a = rt_unwrap_value!(frame.eval_pop()?);
                    let b = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a, Int);
                    let b = rt_as_prim!(b, Int);
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
                    let a = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a, Int);
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
                    let a_ref = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a_ref, Bool);
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
                    let a_ref = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a_ref, Bool);
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
                    let b = rt_unwrap_value!(frame.eval_pop()?);

                    let b = rt_as_prim!(b, Bool);
                    
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
                    let a = rt_unwrap_value!(frame.eval_pop()?);
                    let b = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a, Bool);
                    let b = rt_as_prim!(b, Bool);

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
                    let a = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a, Bool);
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
                    let a_ref = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a_ref, Bool);
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
                    let a = rt_unwrap_value!(frame.eval_pop()?);
                    let b = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a, Str);
                    let b = rt_as_prim!(b, Str);
                    
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
                        "eq",
                        vec![arg!(bi, a: float), arg!(bi, b: float)],
                        ty!(bi, bool),
                    )
                },
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = rt_unwrap_value!(frame.eval_pop()?);
                    let b = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a, Float);
                    let b = rt_as_prim!(b, Float);

                    to_return_value(frame.runtime().bool(a==b))
                })),
            ))
        )
        ,
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
                    let a = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a, Float);
                    to_return_value(frame.runtime().allocate(Ok(DinObject::Int(a.floor() as i64))))
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
                        vec![arg!(bi, a: float), arg!(bi, b: float)],
                        ty!(bi, float),
                    )
                },
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = rt_unwrap_value!(frame.eval_pop()?);
                    let b = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_prim!(a, Float);
                    let b = rt_as_prim!(b, Float);

                    to_return_value(
                        frame.runtime().allocate(Ok(DinObject::Float(a * b)))
                    )
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
                    let gen = SignatureGen::new(vec!["T"]);
                    Signature::new_generic(
                        "add",
                        vec![
                            arg_gen!(bi, gen, a: Sequence<T>), 
                            arg_gen!(bi, gen, b: Sequence<T>),
                        ],
                        ty_gen!(bi, gen, Sequence<T>),
                        gen,
                    )
                },
                //signature_fn!(fn eq<T0, T1> (a: Sequence<T0>, b: Sequence<T1>, fn_eq: (T0,T1)->(bool)~=eq) -> bool),
                SetupFunctionBody::System(Box::new(|frame| {
                    let a_ref = rt_unwrap_value!(frame.eval_pop()?);
                    let b_ref = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_ext!(a_ref, Sequence);
                    let b = rt_as_ext!(b_ref, Sequence);

                    if a.is_empty(){
                        return to_return_value(Ok(Ok(frame.runtime().clone_ref(&b_ref)?)));
                    } else if b.is_empty(){
                        return to_return_value(Ok(Ok(frame.runtime().clone_ref(&a_ref)?)));
                    }
                    
                    let ret = Sequence::new_concat(frame.runtime(), vec![a_ref, b_ref])?;

                    to_return_value(frame.runtime().allocate_ext(ret))

                })),
            ))
        )
        ,
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
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = rt_unwrap_value!(frame.eval_pop()?);
                    let b = rt_unwrap_value!(frame.eval_pop()?);
                    let fn_eq = rt_unwrap_value!(frame.eval_pop()?);

                    let a = rt_as_ext!(a, Sequence);
                    let b = rt_as_ext!(b, Sequence);
                    if a.len() != b.len(){
                        return to_return_value(frame.runtime().bool(false));
                    }
                    for (a_it, b_it) in a.iter().zip(b.iter()){
                        let a_it = frame.runtime().clone_ref(a_it)?;
                        let b_it = frame.runtime().clone_ref(b_it)?;
                        let res = frame.call(&fn_eq, vec![a_it, b_it])?;
                        let res = rt_unwrap_value!(res);
                        let res = rt_as_prim!(res, Bool);
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
                        "len",
                        vec![
                            arg_gen!(bi, gen, a: Sequence<T>), 
                        ],
                        ty!(bi, int),
                        gen,
                    )
                },
                SetupFunctionBody::System(Box::new(|frame| {
                    let a = rt_unwrap_value!(frame.eval_pop()?);
                    
                    let a = rt_as_ext!(a, Sequence);
                    let l = a.len() as i64;
                    to_return_value(frame.runtime().allocate(Ok(DinObject::Int(l))))
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
                    let seq = rt_unwrap_value!(frame.eval_pop()?);
                    let idx = rt_unwrap_value!(frame.eval_pop()?);

                    let seq = rt_as_ext!(seq, Sequence);
                    let idx = rt_as_prim!(idx, Int);
                    let NormalizedIdx::Positive(idx) = seq.idx_from_int(*idx) else {
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
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                |bi: &Builtins<'_>| {
                    let gen = SignatureGen::new(vec!["T"]);
                    Signature::new_generic(
                        "slice",
                        vec![
                            arg_gen!(bi, gen, seq: Sequence<T>), 
                            arg!(bi, start_idx: int), 
                            arg!(bi, end_idx: int), // todo make this an optional?
                        ],
                        ty_gen!(bi, gen, Sequence<T>),
                        gen,
                    )
                },
                SetupFunctionBody::System(Box::new(|frame| {
                    let seq_ref = rt_unwrap_value!(frame.eval_pop()?);
                    let start_idx = rt_unwrap_value!(frame.eval_pop()?);
                    let end_idx = rt_unwrap_value!(frame.eval_pop()?);

                    let seq = rt_as_ext!(seq_ref, Sequence);
                    
                    let start_idx = rt_as_prim!(start_idx, Int);
                    let end_idx = rt_as_prim!(end_idx, Int);
                    
                    let seq_len = seq.len();
                    let start_idx = match seq.idx_from_int(*start_idx){
                        NormalizedIdx::Positive(idx) => idx,
                        NormalizedIdx::Negative => 0,
                        NormalizedIdx::Overflow => seq_len,
                    };
                    let end_idx = match seq.idx_from_int(*end_idx){
                        NormalizedIdx::Positive(idx) => idx,
                        NormalizedIdx::Negative => 0,
                        NormalizedIdx::Overflow => seq_len,
                    };
                    
                    if start_idx == 0 && end_idx >= seq_len{
                        return to_return_value(Ok(Ok(frame.runtime().clone_ref(&seq_ref)?)));
                    }
                    let ret = Sequence::new_slice(frame.runtime(), seq_ref, start_idx, end_idx)?;
                    to_return_value(frame.runtime().allocate_ext(ret))
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
                        "to_array",
                        vec![
                            arg_gen!(bi, gen, seq: Sequence<T>),
                        ],
                        ty_gen!(bi, gen, Sequence<T>),
                        gen,
                    )
                },
                SetupFunctionBody::System(Box::new(|frame| {
                    let seq_ref = rt_unwrap_value!(frame.eval_pop()?);

                    let seq = rt_as_ext!(seq_ref, Sequence);
                    
                    if seq.is_array(){
                        return to_return_value(Ok(Ok(frame.runtime().clone_ref(&seq_ref)?)));
                    }
                    
                    let seq_len = seq.len();
                    let ret = {
                        let mut v = Vec::with_capacity(seq_len);
                        for item_ref in seq.iter(){
                            let item_ref = frame.runtime().clone_ref(item_ref)?;
                            v.push(item_ref);
                        }
                        v
                    };

                    let ret = Sequence::new_array(ret);
                    to_return_value(frame.runtime().allocate_ext(ret))
                })),
            ))
        )
        ,
        // endregion sequence
        // region:stack
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                |bi: &Builtins<'_>| {
                    // todo is this ok? should we make an unknown explicit?
                    let gen = SignatureGen::new(vec!["T"]);
                    Signature::new_generic(
                        "push",
                        vec![arg_gen!(bi, gen, stack: Stack<T>), arg_gen!(bi, gen, item: T)],
                        ty_gen!(bi, gen, Stack<T>),
                        gen,
                    )
                },
                SetupFunctionBody::System(Box::new(|frame| {
                    let stack_ref = rt_unwrap_value!(frame.eval_pop()?);
                    let item_ref = rt_unwrap_value!(frame.eval_pop()?);

                    let ret = Stack::populated(stack_ref, item_ref);
                    to_return_value(frame.runtime().allocate_ext(ret))
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                |bi: &Builtins<'_>| {
                    // todo is this ok? should we make an unknown explicit?
                    let gen = SignatureGen::new(vec!["T"]);
                    Signature::new_generic(
                        "stack",
                        vec![],
                        ty_gen!(bi, gen, Stack<T>),
                        gen,
                    )
                },
                SetupFunctionBody::System(Box::new(|frame| {
                    let ret = Stack::empty();
                    to_return_value(frame.runtime().allocate_ext(ret))
                })),
            ))
        )
        ,
        // pragma:unwrap
        builder.add_item(
            SetupItem::Function(SetupFunction::new(
                |bi: &Builtins<'_>| {
                    // todo is this ok? should we make an unknown explicit?
                    let gen = SignatureGen::new(vec!["T"]);
                    Signature::new_generic(
                        "to_array",
                        vec![arg_gen!(bi, gen, stack: Stack<T>)],
                        ty_gen!(bi, gen, Sequence<T>),
                        gen,
                    )
                },
                SetupFunctionBody::System(Box::new(|frame| {
                    let stack = rt_unwrap_value!(frame.eval_pop()?);

                    let stack = rt_as_ext!(stack, Stack);
                    let mut arr = Vec::with_capacity(stack.len());
                    
                    for item_ref in stack.iter(){
                        let item_ref = frame.runtime().clone_ref(item_ref)?;
                        arr.push(item_ref);
                    }

                    let ret = Sequence::new_array(arr);
                    to_return_value(frame.runtime().allocate_ext(ret))
                })),
            ))
        )
        ,
        // endregion stack
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
        // region:seq-1
        // pragma:replace-start
        builder.build_source(
            // pragma:replace-id
            "pop",
            r#"fn pop<T>(seq: Sequence<T>, idx:int)->Sequence<T>{
                let seq_len = seq.len();
                if(idx==0 || -idx==seq_len,
                    slice(seq, 1, seq_len),
                    if(idx==-1 || idx==seq_len-1,
                        slice(seq, 0, -1),
                        slice(seq, 0, idx) + slice(seq, idx+1, seq_len)
                    )
                )
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