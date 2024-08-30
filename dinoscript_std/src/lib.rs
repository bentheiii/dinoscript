use std::sync::{Arc, LazyLock};

use dinoscript_core::{
    bytecode::{Command, MakeFunction, PushFromSource, SourceId},
    compilation_scope::{
        self,
        ty::{BuiltinTemplate, Ty, TyTemplate},
        CompilationScope, NamedItem, NamedType,
    },
    dinobj::{DinObject, StackItem, UserFn},
    dinopack::{
        utils::{SetupFunction, SetupFunctionBody, SetupItem, Signature},
        DinoPack,
    },
    maybe_owned::MaybeOwned,
    runtime::RuntimeFrame,
};

pub struct StdPack;

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

macro_rules! signature_fn {
    ($name:ident ($($param_name:ident : $type_name:ident),*) -> $ret_type:ident) => {
        |b: &Builtins<'_>| Signature::new(
            std::borrow::Cow::Borrowed(stringify!($name)),
            vec![$(dinoscript_core::dinopack::utils::Arg::new(std::borrow::Cow::Borrowed(stringify!($param_name)), b.$type_name.clone())),*],
            b.$ret_type.clone(),
        )
    };
}

impl StdPack {
    const SOURCE_ID: SourceId = "core";
    fn setup_items<'a, 's>() -> Vec<SetupItem<'s, &'a Builtins<'s>>> {
        static DOUBLE: LazyLock<Vec<Command>> = LazyLock::new(|| {
            vec![
                dinoscript_core::bytecode::Command::PopToCell(0),
                dinoscript_core::bytecode::Command::PushFromCell(0),
                dinoscript_core::bytecode::Command::PushFromCell(0),
                dinoscript_core::bytecode::Command::PushFromSource(dinoscript_core::bytecode::PushFromSource::new("core",0)),
                dinoscript_core::bytecode::Command::MakePending(2),
                dinoscript_core::bytecode::Command::EvalTop
            ]                              
        });
        vec![
            SetupItem::Function(SetupFunction::new(
                signature_fn!(add (a: int, b: int) -> int),
                SetupFunctionBody::System(Box::new(|mut stack| {
                    let StackItem::Value(Ok(b)) = stack.pop().unwrap() else {
                        todo!()
                    };
                    let StackItem::Value(Ok(a)) = stack.pop().unwrap() else {
                        todo!()
                    };
                    match (a.as_ref(), b.as_ref()) {
                        (DinObject::Int(a), DinObject::Int(b)) => Ok(Ok(Arc::new(DinObject::Int(a + b)))),
                        _ => Ok(Err(())),
                    }
                })),
            )),
            SetupItem::Function(SetupFunction::new(
                signature_fn!(double (a: int) -> int),
                SetupFunctionBody::User(UserFn::without_capture(10, &DOUBLE)),
            )),
        ]
    }
}

impl DinoPack for StdPack {
    type Builtins<'s> = Builtins<'s>;

    fn setup_compiler<'p, 's>(&self, scope: &mut CompilationScope<'p, 's, Self::Builtins<'s>>) {
        let source_id = StdPack::SOURCE_ID;
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

        let builtins = Builtins { int, float, bool, str };

        let mut next_id = 0;
        let mut get_id = || {
            let ret = next_id;
            next_id += 1;
            ret
        };

        for item in Self::setup_items() {
            item.push_to_compilation(scope, &builtins, source_id, &mut get_id);
        }

        scope.builtins = Some(MaybeOwned::Owned(builtins));
    }

    fn setup_runtime(&self, frame: &mut RuntimeFrame) {
        let source_id = StdPack::SOURCE_ID;
        let mut items = Vec::new();

        for item in Self::setup_items() {
            items.push(item.to_dinobject());
        }

        frame.add_source(source_id, items);
    }
}
