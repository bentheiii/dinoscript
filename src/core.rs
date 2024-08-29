use std::sync::Arc;

use crate::{
    compilation_scope::{
        ty::{BuiltinTemplate, Ty, TyTemplate},
        CompilationScope, NamedItem, NamedType,
    },
    dinopack::{utils::{SetupFunction, SetupItem, Signature}, DinoPack},
    maybe_owned::MaybeOwned,
    runtime::RuntimeFrame,
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

impl DinoPack for CorePack {
    fn setup_compiler_with_id(&self, scope: &mut CompilationScope, source_id: usize) {
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
        scope.builtins = Some(MaybeOwned::Owned(builtins));

        let mut next_id = 0;
        let mut get_id = || {
            let ret = next_id;
            next_id += 1;
            ret
        };

        
        SetupItem::Function(SetupFunction::new(
            signature_fn!(add (a: int, b: int) -> int),
            todo!()
        )).push_to_compilation(scope, &builtins, source_id, &mut get_id)
    }
}