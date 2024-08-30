use std::sync::Arc;

use crate::{
    bytecode::SourceId, compilation_scope::{
        ty::{BuiltinTemplate, Ty, TyTemplate},
        CompilationScope, NamedItem, NamedType,
    }, dinobj::{DinObject, StackItem}, dinopack::{
        utils::{SetupFunction, SetupFunctionBody, SetupItem, Signature},
        DinoPack,
    }, maybe_owned::MaybeOwned, runtime::RuntimeFrame
};

pub(crate) struct CorePack;

pub(crate) struct Builtins<'s> {
    pub int: Arc<Ty<'s>>,
    pub float: Arc<Ty<'s>>,
    pub bool: Arc<Ty<'s>>,
    pub str: Arc<Ty<'s>>,
}

macro_rules! signature_fn {
    ($name:ident ($($param_name:ident : $type_name:ident),*) -> $ret_type:ident) => {
        |b: &Builtins<'_>| Signature::new(
            std::borrow::Cow::Borrowed(stringify!($name)),
            vec![$($crate::dinopack::utils::Arg::new(std::borrow::Cow::Borrowed(stringify!($param_name)), b.$type_name.clone())),*],
            b.$ret_type.clone(),
        )
    };
}

impl CorePack {
    const SOURCE_ID: SourceId = "core";
    fn setup_items<'a, 's>() -> Vec<SetupItem<'s, &'a Builtins<'s>>> {
        vec![
            SetupItem::Function(SetupFunction::new(signature_fn!(add (a: int, b: int) -> int), SetupFunctionBody::System(Box::new(|mut stack| {
                let StackItem::Value(Ok(b)) = stack.pop().unwrap() else { todo!()};
                let StackItem::Value(Ok(a)) = stack.pop().unwrap() else { todo!()};
                match (a.as_ref(), b.as_ref()) {
                    (DinObject::Int(a), DinObject::Int(b)) => Ok(Ok(Arc::new(DinObject::Int(a + b)))),
                    _ => Ok(Err(())),
                }
            }))))
        ]
    }
        
}


impl DinoPack for CorePack {
    fn setup_compiler(&self, scope: &mut CompilationScope) {
        let source_id = CorePack::SOURCE_ID;
        fn register_type<'p, 's>(
            scope: &mut CompilationScope<'p, 's>,
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
        /*
        SetupItem::Function(SetupFunction::new(signature_fn!(add (a: int, b: int) -> int), SetupFunctionBody::System(Box::new(|mut stack| {
            let StackItem::Value(Ok(b)) = stack.pop().unwrap() else { todo!()};
            let StackItem::Value(Ok(a)) = stack.pop().unwrap() else { todo!()};
            match (a.as_ref(), b.as_ref()) {
                (DinObject::Int(a), DinObject::Int(b)) => Ok(Ok(Arc::new(DinObject::Int(a + b)))),
                _ => Ok(Err(())),
            }
        }))))
        .push_to_compilation(scope, &builtins, SOURCE_ID, &mut get_id);
        */

        scope.builtins = Some(MaybeOwned::Owned(builtins));
    }

    fn setup_runtime(&self, frame: &mut RuntimeFrame) {
        let source_id = CorePack::SOURCE_ID;
        let mut items = Vec::new();

        for item in Self::setup_items() {
            items.push(item.to_dinobject());
        }

        frame.add_source(source_id, items);
    }
}
