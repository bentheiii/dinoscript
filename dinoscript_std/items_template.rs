use dinoscript_core::{
    bytecode::{Command, SourceId},
    compilation_scope::{
        self,
        ty::{
            BuiltinTemplate, CompoundKind, CompoundTemplate, Field, Fn, Generic, GenericSetId, TemplateGenericSpecs,
            Ty, TyTemplate,
        },
        CompilationScope, Location, NamedItem, NamedType, Overloads, SystemLoc,
    },
    dinobj::{DinObject, DinoResult, SourceFnResult, TailCallAvailability, VariantObject},
    dinopack::utils::{Arg, SetupFunction, SetupFunctionBody, SetupItem, SetupValue, Signature, SignatureGen},
    errors::RuntimeError,
    lib_objects::{optional::{self}, sequence::{NormalizedIdx, Sequence}, stack::{self}},
    runtime::Runtime,
};
use std::{ops::ControlFlow, sync::Arc};
// pragma: skip 6
use dinoscript_core::{
    ast::statement::{FnArgDefault, ResolveOverload, Stmt},
    bytecode::to_in_code,
    maybe_owned::MaybeOwned,
};
use std::collections::HashMap;

pub struct Builtins<'s> {
    pub int: Arc<Ty<'s>>,
    pub float: Arc<Ty<'s>>,
    pub bool: Arc<Ty<'s>>,
    pub str: Arc<Ty<'s>>,

    pub sequence: Arc<TyTemplate<'s>>,
    pub stack: Arc<TyTemplate<'s>>,
    pub optional: Arc<TyTemplate<'s>>,
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

    fn optional(&self, ty: Arc<Ty<'s>>) -> Arc<Ty<'s>> {
        self.optional.instantiate(vec![ty])
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

    fn optional(&self, ty: Arc<Ty<'s>>) -> Arc<Ty<'s>> {
        self.optional.instantiate(vec![ty])
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
    ($b:ident, $g:expr, Optional<$ty:tt>) => {
        $b.optional(ty_gen!($b, $g, $ty))
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
    ($ref:expr, $ty:ident) => {{
        let ptr = (*rt_as_prim!($ref, Extended));
        let tn = (unsafe { &*ptr }).type_name();
        if tn != $ty::EXPECTED_TYPE_NAME {
            let err = format!("Expected {} got {:?}", $ty::EXPECTED_TYPE_NAME, tn);
            return to_return_value(Ok($ref.runtime().allocate(Err(err.into()))?));
        }
        let ptr = ptr as *const $ty;
        unsafe { &*ptr }
    }};
}

// pragma:replace-start
pub(crate) struct ItemsBuilder<'p, 's> {
    scope: CompilationScope<'p, 's, Builtins<'s>>,
    source_id: SourceId,
    next_id: usize,
    pub(crate) replacements: HashMap<&'static str, String>,
}

fn signature_code_and_name_from_statement<'a>(stmt: &'a Stmt<'_>) -> (&'a str, String) {
    let Stmt::Fn(func) = stmt else {
        panic!("expected function statement, got {:?}", stmt);
    };
    let name = func.name.as_ref();
    let generic_params = &func.generic_params;
    let args = &func.args;
    let ret = &func.return_ty;

    let gen_params_tokens = generic_params
        .iter()
        .map(|p| format!("\"{}\"", p))
        .collect::<Vec<_>>()
        .join(", ");
    let name_token = format!("\"{}\"", name);
    let mut args_tokens = Vec::with_capacity(args.len());
    for arg in args {
        let name = arg.name.as_ref();
        let ty = &arg.ty;
        args_tokens.push(match arg.default.as_ref() {
            Some(FnArgDefault::ResolveOverload(ResolveOverload { name: resolv })) => {
                format!("arg_gen!(bi, gen, {}~={}: {})", name, resolv, ty.inner.to_in_code())
            }
            Some(_) => {
                panic!()
            }
            None => {
                format!("arg_gen!(bi, gen, {}: {})", name, ty.inner.to_in_code())
            }
        })
    }
    let args_tokens = args_tokens.join(",\n                ");
    let ret_token = ret.inner.to_in_code();
    (
        name,
        format!(
            "
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
    "
        ),
    )
}

impl<'p, 's> ItemsBuilder<'p, 's> {
    fn get_next_id(&mut self) -> usize {
        let ret = self.next_id;
        self.next_id += 1;
        ret
    }

    fn add_item<'c>(&'c mut self, item: SetupItem<'s, Builtins<'s>>) {
        item.push_to_compilation(&mut self.scope, self.source_id, || {
            let ret = self.next_id;
            self.next_id += 1;
            ret
        });
    }

    fn build_source(&mut self, replacement_name: &'static str, code: &'static str) {
        let next_id = self.get_next_id();
        let func_stmt = dinoscript_core::grammar::parse_raw_function(code).unwrap();
        let mut sink = Vec::new();
        if let Err(err) = self.scope.feed_statement(&func_stmt, &mut sink) {
            panic!("compilation error (building {}): {}", replacement_name, err);
        }
        // the sink should now have two items, the first is the actual function, the second is popping it to some cell (that we don't care about)
        // in the future, the sink might also include default values and the like, but it will never include captures since these are globals functions
        assert!(matches!(sink.pop(), Some(Command::PopToCell(_)))); // remove the pop command
        let Command::MakeFunction(mk_func) = sink.pop().unwrap() else {
            panic!("Expected a MakeFunction command")
        };
        assert!(sink.is_empty());
        assert!(mk_func.n_captures == 0);
        let n_cells = mk_func.n_cells;
        let commands = to_in_code(&mk_func.commands, "                    ");
        // now we need the signature, we could re-create it from the statement, but it's easier (for now at least) to just get it from the code
        let (fn_name, signature) = signature_code_and_name_from_statement(&func_stmt.inner);
        let replacement = format!(
            "SetupItem::Function(SetupFunction::new(
            {signature},
            {{
                static LZ: std::sync::LazyLock<Vec<Command>> = std::sync::LazyLock::new(|| {{
                    {commands}
                }});
                SetupFunctionBody::User(dinoscript_core::dinobj::UserFn::without_capture({fn_name:?}, {n_cells}, &LZ))
            }}
        )),\n"
        );
        assert!(self.replacements.insert(replacement_name, replacement).is_none());
        // finally, we need to patch the newly created function in the scope, that it will target source, and not the current scope
        let NamedItem::Overloads(Overloads { overloads }) = self.scope.names.get_mut(fn_name).unwrap() else {
            panic!("Expected an overload in name {}", fn_name);
        };
        let overload = overloads.last_mut().unwrap();
        overload.set_loc(Location::System(SystemLoc::new(self.source_id, next_id)));
    }
}

fn prepare_build<'p>() -> ItemsBuilder<'p, 'static> {
    let mut scope = CompilationScope::root();
    let builtins = pre_items_setup(&mut scope);
    scope.builtins = Some(MaybeOwned::Owned(builtins));
    ItemsBuilder {
        scope,
        source_id: SOURCE_ID,
        next_id: 0,
        replacements: HashMap::new(),
    }
}
// pragma:replace-with-raw
// pragma:replace-end

pub(crate) const SOURCE_ID: SourceId = "std";
pub(crate) fn pre_items_setup<'s>(scope: &mut CompilationScope<'_, 's, Builtins<'s>>) -> Builtins<'s> {
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
    let float =
        register_type(scope, "float", TyTemplate::Builtin(BuiltinTemplate::primitive("float"))).instantiate(vec![]);
    let bool =
        register_type(scope, "bool", TyTemplate::Builtin(BuiltinTemplate::primitive("bool"))).instantiate(vec![]);
    let str = register_type(scope, "str", TyTemplate::Builtin(BuiltinTemplate::primitive("str"))).instantiate(vec![]);
    let sequence = register_type(
        scope,
        "Sequence",
        TyTemplate::Builtin(BuiltinTemplate::new(
            "Sequence",
            TemplateGenericSpecs::new(GenericSetId::unique(), 1),
        )),
    );
    let stacknode_gen_id = GenericSetId::unique();
    let stacknode = register_type(
        scope,
        "__std_StackNode",
        TyTemplate::Compound(CompoundTemplate::new(
            "__std_StackNode",
            CompoundKind::Struct,
            Some(TemplateGenericSpecs::new(stacknode_gen_id, 2)),
            vec![
                ("value".into(), Field::new(Arc::new(Ty::Generic(Generic::new(0, stacknode_gen_id))), 0)),
                ("next".into(), Field::new(Arc::new(Ty::Generic(Generic::new(1, stacknode_gen_id))), 1)),
                ("len".into(), Field::new(int.clone(), 2)),
            ]
            .into_iter()
            .collect(),
        )),
    );
    let stack_gen_id = GenericSetId::unique();

    let stack = register_type(
        scope,
        "Stack",
        TyTemplate::Compound(CompoundTemplate::new(
            "Stack",
            CompoundKind::Union,
            Some(TemplateGenericSpecs::new(stack_gen_id, 1)),
            vec![
                (
                    "Empty".into(),
                    Field::new(Arc::new(Ty::Tuple(vec![])), stack::tag::EMPTY.into()),
                ),
                (
                    "Node".into(),
                    Field::new(stacknode.instantiate(vec![Arc::new(Ty::Generic(Generic::new(0, stack_gen_id))), Arc::new(Ty::Tail(vec![Arc::new(Ty::Generic(Generic::new(0, stack_gen_id)))]))]), stack::tag::NODE.into()),
                ),
            ]
            .into_iter()
            .collect(),
        )),
    );
    let optional_gen_id = GenericSetId::unique();
    let optional = register_type(
        scope,
        "Optional",
        TyTemplate::Compound(CompoundTemplate::new(
            "Optional",
            CompoundKind::Union,
            Some(TemplateGenericSpecs::new(optional_gen_id, 1)),
            vec![
                (
                    "Some".into(),
                    Field::new(Arc::new(Ty::Generic(Generic::new(0, optional_gen_id))), optional::tag::SOME.into()),
                ),
                ("None".into(), Field::new(Arc::new(Ty::Tuple(vec![])), optional::tag::NONE.into())),
            ]
            .into_iter()
            .collect(),
        )),
    );

    Builtins {
        int,
        float,
        bool,
        str,
        sequence,
        stack,
        optional,
    }
}

fn to_return_value(result: DinoResult<'_>) -> SourceFnResult<'_> {
    match result {
        Ok(v) => Ok(ControlFlow::Break(v)),
        Err(e) => Err(e),
    }
}

// pragma:replace-start
pub(crate) fn setup_items<'a, 's>() -> ItemsBuilder<'a, 's>
// pragma:replace-with-raw
/*
pub(crate) fn setup_items<'s>()-> Vec<SetupItem<'s, Builtins<'s>>>
*/
// pragma:replace-end
{
    // pragma:skip 1
    let mut builder = prepare_build();
    vec![
        // region:option
        // pragma:unwrap
        builder.add_item(SetupItem::Value(SetupValue {
            name: "none".into(),
            ty_factory: |bi| bi.optional(Ty::unknown()),
            value: |rt: &Runtime<'_>| rt.none(),
        })),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| {
                let gen = SignatureGen::new(vec!["T"]);
                Signature::new_generic(
                    "and",
                    vec![arg_gen!(bi, gen, a: Optional<T>), arg_gen!(bi, gen, b: Optional<T>)],
                    ty_gen!(bi, gen, Optional<T>),
                    gen,
                )
            },
            SetupFunctionBody::System(Box::new(|frame| {
                let a_ref = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a_ref, Variant);

                if a.tag() == optional::tag::NONE {
                    to_return_value(Ok(Ok(a_ref)))
                } else {
                    frame.eval_pop_tca(TailCallAvailability::Allowed)
                }
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| {
                let gen = SignatureGen::new(vec!["T"]);
                Signature::new_generic("is_some", vec![arg_gen!(bi, gen, a: Optional<T>)], ty!(bi, bool), gen)
            },
            SetupFunctionBody::System(Box::new(|frame| {
                let a_ref = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a_ref, Variant);
                let tag = a.tag();
                to_return_value(frame.runtime().bool(tag == optional::tag::SOME))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| {
                let gen = SignatureGen::new(vec!["T", "U"]);
                Signature::new_generic(
                    "map_or",
                    vec![
                        arg_gen!(bi, gen, a: Optional<T>),
                        arg_gen!(bi, gen, f: (T)->(U)),
                        arg_gen!(bi, gen, d: U),
                    ],
                    ty_gen!(bi, gen, U),
                    gen,
                )
            },
            SetupFunctionBody::System(Box::new(|frame| {
                let a_ref = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a_ref, Variant);

                if a.tag() == optional::tag::SOME {
                    let f = rt_unwrap_value!(frame.eval_pop()?);
                    let obj = frame.runtime().clone_ref(Ok(a.obj()))?;
                    to_return_value(frame.call(&f, &[obj]))
                } else {
                    frame.stack.pop().unwrap();
                    frame.eval_pop_tca(TailCallAvailability::Allowed)
                }
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| {
                let gen = SignatureGen::new(vec!["T"]);
                Signature::new_generic(
                    "or",
                    vec![arg_gen!(bi, gen, a: Optional<T>), arg_gen!(bi, gen, b: Optional<T>)],
                    ty_gen!(bi, gen, Optional<T>),
                    gen,
                )
            },
            SetupFunctionBody::System(Box::new(|frame| {
                let a_ref = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a_ref, Variant);

                if a.tag() == optional::tag::SOME {
                    to_return_value(Ok(Ok(a_ref)))
                } else {
                    frame.eval_pop_tca(TailCallAvailability::Allowed)
                }
            })),
        ))),
        // endregion
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| {
                let gen = SignatureGen::new(vec!["T"]);
                Signature::new_generic(
                    "or",
                    vec![arg_gen!(bi, gen, a: Optional<T>), arg_gen!(bi, gen, b: T)],
                    ty_gen!(bi, gen, T),
                    gen,
                )
            },
            SetupFunctionBody::System(Box::new(|frame| {
                let a_ref = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a_ref, Variant);

                if a.tag() == optional::tag::SOME {
                    let obj = frame.runtime().clone_ref(Ok(a.obj()))?;
                    to_return_value(Ok(obj))
                } else {
                    frame.eval_pop_tca(TailCallAvailability::Allowed)
                }
            })),
        ))),
        // endregion
        // region:generic
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| {
                let gen = SignatureGen::new(vec!["T"]);
                Signature::new_generic("debug", vec![arg_gen!(bi, gen, a: T)], ty_gen!(bi, gen, T), gen)
            },
            SetupFunctionBody::System(Box::new(|frame| {
                let a = frame.eval_pop()?;
                println!("{:?}", a);
                to_return_value(Ok(a))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| {
                Signature::new(
                    "eq",
                    vec![Arg::new("a", Ty::unknown()), Arg::new("b", Ty::unknown())],
                    ty!(bi, bool),
                )
            },
            SetupFunctionBody::System(Box::new(|frame| unreachable!("unknown eq call"))),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("error", vec![arg!(bi, a: str)], Ty::unknown()),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Str);
                to_return_value(frame.runtime().allocate(Err(RuntimeError::Owned(a.to_string()))))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| {
                let gen = SignatureGen::new(vec!["T"]);
                Signature::new_generic("cast", vec![arg_gen!(bi, gen, a: T)], ty_gen!(bi, gen, T), gen)
            },
            SetupFunctionBody::System(Box::new(|frame| frame.eval_pop_tca(TailCallAvailability::Allowed))),
        ))),
        // endregion
        // region:int
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("add", vec![arg!(bi, a: int), arg!(bi, b: int)], ty!(bi, int)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);
                let b = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Int);
                let b = rt_as_prim!(b, Int);
                to_return_value(frame.runtime().allocate(Ok(DinObject::Int(a + b))))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("div", vec![arg!(bi, a: int), arg!(bi, b: int)], ty!(bi, float)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);
                let b = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Int);
                let b = rt_as_prim!(b, Int);
                to_return_value(if *b == 0 {
                    frame.runtime().allocate(Err("Division by zero".into()))
                } else {
                    frame
                        .runtime()
                        .allocate(Ok(DinObject::Float((*a as f64) / (*b as f64))))
                })
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("eq", vec![arg!(bi, a: int), arg!(bi, b: int)], ty!(bi, bool)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);
                let b = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Int);
                let b = rt_as_prim!(b, Int);
                to_return_value(frame.runtime().bool(a == b))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("gt", vec![arg!(bi, a: int), arg!(bi, b: int)], ty!(bi, bool)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);
                let b = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Int);
                let b = rt_as_prim!(b, Int);
                to_return_value(frame.runtime().bool(a > b))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("gte", vec![arg!(bi, a: int), arg!(bi, b: int)], ty!(bi, bool)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);
                let b = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Int);
                let b = rt_as_prim!(b, Int);
                to_return_value(frame.runtime().bool(a >= b))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("lt", vec![arg!(bi, a: int), arg!(bi, b: int)], ty!(bi, bool)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);
                let b = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Int);
                let b = rt_as_prim!(b, Int);
                to_return_value(frame.runtime().bool(a < b))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("lte", vec![arg!(bi, a: int), arg!(bi, b: int)], ty!(bi, bool)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);
                let b = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Int);
                let b = rt_as_prim!(b, Int);
                to_return_value(frame.runtime().bool(a <= b))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("mod", vec![arg!(bi, a: int), arg!(bi, b: int)], ty!(bi, int)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);
                let b = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Int);
                let b = rt_as_prim!(b, Int);
                to_return_value(if *b == 0 {
                    frame.runtime().allocate(Err("Division by zero".into()))
                } else {
                    frame.runtime().allocate(Ok(DinObject::Int(a % b)))
                })
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("mul", vec![arg!(bi, a: int), arg!(bi, b: int)], ty!(bi, int)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);
                let b = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Int);
                let b = rt_as_prim!(b, Int);
                to_return_value(frame.runtime().allocate(Ok(DinObject::Int(a * b))))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("neg", vec![arg!(bi, a: int)], ty!(bi, int)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Int);
                to_return_value(frame.runtime().allocate(Ok(DinObject::Int(-a))))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("sub", vec![arg!(bi, a: int), arg!(bi, b: int)], ty!(bi, int)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);
                let b = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Int);
                let b = rt_as_prim!(b, Int);
                to_return_value(frame.runtime().allocate(Ok(DinObject::Int(a - b))))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("to_str", vec![arg!(bi, a: int)], ty!(bi, str)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Int);
                to_return_value(frame.runtime().allocate(Ok(DinObject::Str(format!("{}", a).into()))))
            })),
        ))),
        // endregion
        // region:bool
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("and", vec![arg!(bi, a: bool), arg!(bi, b: bool)], ty!(bi, bool)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a_ref = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a_ref, Bool);
                if !a {
                    to_return_value(Ok(Ok(a_ref)))
                } else {
                    frame.eval_pop_tca(TailCallAvailability::Allowed)
                }
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("assert", vec![arg!(bi, a: bool)], ty!(bi, bool)),
            SetupFunctionBody::System(Box::new(|frame| {
                // todo: if the value is a pending, maybe we can add it to the error message?
                let a_ref = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a_ref, Bool);
                to_return_value(if *a {
                    Ok(Ok(a_ref))
                } else {
                    frame.runtime().allocate(Err("Assertion is false".into()))
                })
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
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
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("eq", vec![arg!(bi, a: bool), arg!(bi, b: bool)], ty!(bi, bool)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);
                let b = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Bool);
                let b = rt_as_prim!(b, Bool);

                to_return_value(frame.runtime().bool(a == b))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("not", vec![arg!(bi, a: bool)], ty!(bi, bool)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Bool);
                to_return_value(frame.runtime().allocate(Ok(DinObject::Bool(!a))))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("or", vec![arg!(bi, a: bool), arg!(bi, b: bool)], ty!(bi, bool)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a_ref = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a_ref, Bool);
                if *a {
                    to_return_value(Ok(Ok(a_ref)))
                } else {
                    frame.eval_pop_tca(TailCallAvailability::Allowed)
                }
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| {
                let gen = SignatureGen::new(vec!["T"]);
                Signature::new_generic(
                    "then",
                    vec![arg!(bi, b:bool), arg_gen!(bi, gen, t: T)],
                    ty_gen!(bi, gen, Optional<T>),
                    gen,
                )
            },
            SetupFunctionBody::System(Box::new(|frame| {
                let b = rt_unwrap_value!(frame.eval_pop()?);

                let b = rt_as_prim!(b, Bool);

                if !b {
                    to_return_value(frame.runtime().none())
                } else {
                    let inner = rt_unwrap_value!(frame.eval_pop()?);
                    let v = DinObject::Variant(VariantObject::new(optional::tag::SOME, inner));
                    to_return_value(frame.runtime().allocate(Ok(v)))
                }
            })),
        ))),
        // endregion bool
        // region:str
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("eq", vec![arg!(bi, a: str), arg!(bi, b: str)], ty!(bi, bool)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);
                let b = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Str);
                let b = rt_as_prim!(b, Str);

                to_return_value(frame.runtime().bool(a == b))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("add", vec![arg!(bi, a: str), arg!(bi, b: str)], ty!(bi, str)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a_ref = rt_unwrap_value!(frame.eval_pop()?);
                let b = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a_ref, Str);
                if a.is_empty() {
                    return to_return_value(Ok(Ok(b)));
                }
                
                let b = rt_as_prim!(b, Str);
                if b.is_empty() {
                    return to_return_value(Ok(Ok(a_ref)));
                }

                to_return_value(frame.runtime().allocate(Ok(DinObject::Str(format!("{}{}", a, b).into()))))
            })),
        ))),
        // endregion str
        // region:float
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("add", vec![arg!(bi, a: float), arg!(bi, b: float)], ty!(bi, float)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);
                let b = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Float);
                let b = rt_as_prim!(b, Float);

                to_return_value(frame.runtime().allocate(Ok(DinObject::Float(a + b))))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("eq", vec![arg!(bi, a: float), arg!(bi, b: float)], ty!(bi, bool)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);
                let b = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Float);
                let b = rt_as_prim!(b, Float);

                to_return_value(frame.runtime().bool(a == b))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("floor", vec![arg!(bi, a: float)], ty!(bi, int)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Float);
                to_return_value(frame.runtime().allocate(Ok(DinObject::Int(a.floor() as i64))))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| Signature::new("mul", vec![arg!(bi, a: float), arg!(bi, b: float)], ty!(bi, float)),
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);
                let b = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_prim!(a, Float);
                let b = rt_as_prim!(b, Float);

                to_return_value(frame.runtime().allocate(Ok(DinObject::Float(a * b))))
            })),
        ))),
        // endregion float
        // region:sequence
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| {
                let gen = SignatureGen::new(vec!["T"]);
                Signature::new_generic(
                    "add",
                    vec![arg_gen!(bi, gen, a: Sequence<T>), arg_gen!(bi, gen, b: Sequence<T>)],
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

                if a.is_empty() {
                    return to_return_value(Ok(Ok(b_ref)));
                } else if b.is_empty() {
                    return to_return_value(Ok(Ok(a_ref)));
                }

                let ret = Sequence::new_concat(frame.runtime(), vec![a_ref, b_ref])?;

                to_return_value(frame.runtime().allocate_ext(ret))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| {
                let gen = SignatureGen::new(vec!["T0", "T1"]);
                Signature::new_generic(
                    "eq",
                    vec![
                        arg_gen!(bi, gen, a: Sequence<T0>),
                        arg_gen!(bi, gen, b: Sequence<T1>),
                        arg_gen!(bi, gen, fn_eq~=eq: (T0,T1)->(bool)),
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
                if a.len() != b.len() {
                    return to_return_value(frame.runtime().bool(false));
                }
                for (a_it, b_it) in a.iter(frame).zip(b.iter(frame)) {
                    let a_it = a_it?;
                    let b_it = b_it?;
                    let res = frame.call(&fn_eq, &[a_it, b_it])?;
                    let res = rt_unwrap_value!(res);
                    let res = rt_as_prim!(res, Bool);
                    if !*res {
                        return to_return_value(frame.runtime().bool(false));
                    }
                }
                to_return_value(frame.runtime().bool(true))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| {
                let gen = SignatureGen::new(vec!["T"]);
                Signature::new_generic("len", vec![arg_gen!(bi, gen, a: Sequence<T>)], ty!(bi, int), gen)
            },
            SetupFunctionBody::System(Box::new(|frame| {
                let a = rt_unwrap_value!(frame.eval_pop()?);

                let a = rt_as_ext!(a, Sequence);
                let l = a.len() as i64;
                to_return_value(frame.runtime().allocate(Ok(DinObject::Int(l))))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| {
                let gen = SignatureGen::new(vec!["T"]);
                Signature::new_generic(
                    "lookup",
                    vec![arg_gen!(bi, gen, seq: Sequence<T>), arg!(bi, idx: int)],
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
                let item = seq.get(frame, idx)?;
                to_return_value(Ok(item))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| {
                let gen = SignatureGen::new(vec!["T", "V"]);
                Signature::new_generic(
                    "map",
                    vec![arg_gen!(bi, gen, seq: Sequence<T>), arg_gen!(bi, gen, fun: (T)->(V))],
                    ty_gen!(bi, gen, Sequence<V>),
                    gen,
                )
            },
            SetupFunctionBody::System(Box::new(|frame| {
                let seq_ref = rt_unwrap_value!(frame.eval_pop()?);
                let func = rt_unwrap_value!(frame.eval_pop()?);

                let seq = rt_as_ext!(seq_ref, Sequence);
                if seq.is_empty() {
                    return to_return_value(Ok(Ok(seq_ref)));
                }
                let ret = Sequence::new_map(seq_ref, func);
                to_return_value(frame.runtime().allocate_ext(ret))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
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
                let start_idx = match seq.idx_from_int(*start_idx) {
                    NormalizedIdx::Positive(idx) => idx,
                    NormalizedIdx::Negative => 0,
                    NormalizedIdx::Overflow => seq_len,
                };
                let end_idx = match seq.idx_from_int(*end_idx) {
                    NormalizedIdx::Positive(idx) => idx,
                    NormalizedIdx::Negative => 0,
                    NormalizedIdx::Overflow => seq_len,
                };

                if start_idx == 0 && end_idx >= seq_len {
                    return to_return_value(Ok(Ok(seq_ref)));
                }
                let ret = Sequence::new_slice(frame.runtime(), seq_ref, start_idx, end_idx)?;
                to_return_value(frame.runtime().allocate_ext(ret))
            })),
        ))),
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
            |bi: &Builtins<'_>| {
                let gen = SignatureGen::new(vec!["T"]);
                Signature::new_generic(
                    "to_array",
                    vec![arg_gen!(bi, gen, seq: Sequence<T>)],
                    ty_gen!(bi, gen, Sequence<T>),
                    gen,
                )
            },
            SetupFunctionBody::System(Box::new(|frame| {
                let seq_ref = rt_unwrap_value!(frame.eval_pop()?);

                let seq = rt_as_ext!(seq_ref, Sequence);

                if seq.is_array() {
                    return to_return_value(Ok(Ok(seq_ref)));
                }

                let seq_len = seq.len();
                let ret = {
                    let mut v = Vec::with_capacity(seq_len);
                    for item_ref in seq.iter(frame) {
                        let item_ref = match item_ref? {
                            Ok(v) => v,
                            other => return to_return_value(Ok(other)),
                        };
                        v.push(item_ref);
                    }
                    v
                };

                let ret = Sequence::new_array(ret);
                to_return_value(frame.runtime().allocate_ext(ret))
            })),
        ))),
        // endregion sequence
        // region:stack
        // pragma:unwrap
        builder.add_item(SetupItem::Function(SetupFunction::new(
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
                let stack = rt_as_prim!(stack, Variant);
                let arr = {
                    if stack.tag() == stack::tag::EMPTY {
                        Vec::new()
                    } else {
                        let node = stack.obj();
                        let mut node = rt_as_prim!(node, Struct);
                        let Some(len) = node.get(2) else { todo!() };
                        let len = rt_as_prim!(len, Int);
                        let mut arr = Vec::with_capacity(*len as usize);
                        loop {
                            let Some(item) = node.get(0) else { todo!() };
                            arr.push(frame.runtime().clone_ok_ref(item)?);
                            let Some(next) = node.get(1) else { todo!() };
                            let next = rt_as_prim!(next, Variant);
                            if next.tag() == stack::tag::EMPTY {
                                break;
                            }
                            let next_node = next.obj();
                            node = rt_as_prim!(next_node, Struct);
                        }
                        arr.reverse();
                        arr
                    }
                };
                let ret = Sequence::new_array(arr);
                to_return_value(frame.runtime().allocate_ext(ret))

            })),
        ))),
        // endregion stack
        // region:int-1
        // pragma:replace-start
        builder.build_source(
            // pragma:replace-id
            "abs",
            r#"fn abs(x: int)->int{
                if(x>=0, x, -x)
            }"#,
        ), // pragma:replace-end
        // endregion:int-1
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
            }"#,
        ), // pragma:replace-end
        // endregion:seq-1
        // region:generic-1
        // pragma:replace-start
        builder.build_source(
            // pragma:replace-id
            "neq",
            r#"fn neq<T0, T1>(x: T0, y: T1, fn_eq: (T0, T1)->(bool) ~= eq)->bool{
                !fn_eq(x, y)
            }"#,
        ), // pragma:replace-end
        // endregion:generic-1
        // region:optional-1
        // pragma:replace-start
        builder.build_source(
            // pragma:replace-id
            "eq",
            r#"fn eq<T, U>(a: Optional<T>, b: Optional<U>, fn_eq: (T,U)->(bool) ~= eq)->bool{
                if(is_some(a),
                    if(is_some(b),
                        fn_eq(a!:Some, b!:Some),
                        false
                    ),
                    !b.is_some()
                )
            }"#,
        ), // pragma:replace-end
        // pragma:replace-start
        builder.build_source(
            // pragma:replace-id
            "some",
            r#"fn some<T>(value: T)->Optional<T>{
                Optional::Some(value)
            }"#,
        ), // pragma:replace-end
        // pragma:replace-start
        builder.build_source(
            // pragma:replace-id
            "map",
            r#"fn map<T, U>(a: Optional<T>, func: (T)->(U))->Optional<U>{
                if(is_some(a),
                    some(func(a!:Some)),
                    none
                )
            }"#,
        ), // pragma:replace-end
        // endregion:optional-1
        // region:stack-1
        // pragma:replace-start
        builder.build_source(
            // pragma:replace-id
            "len",
            r#"fn len<T>(stack: Stack<T>)->int{
                let as_node = stack?:Node;
                if(as_node.is_some(),
                    as_node!:Some::len,
                    0
                )
            }"#,
        ), // pragma:replace-end
        // pragma:replace-start
        builder.build_source(
            // pragma:replace-id
            "push",
            r#"fn push<T>(stack: Stack<T>, item: T)->Stack<T>{
                Stack::Node(__std_StackNode(item, stack, len(stack)+1))
            }"#,
        ), // pragma:replace-end
    ]
    // pragma:skip 2
    ;
    builder
}
