use std::sync::Arc;

use crate::{compilation_scope::{ty::{BuiltinTemplate, TyTemplate}, NamedItem, NamedType}, dinopack::DinoPack};

pub(crate) struct CorePack;

impl DinoPack for CorePack {
    fn setup(&self, scope: &mut crate::compilation_scope::CompilationScope) {
        for primitive in ["int", "float", "bool", "str"] {
            let template = TyTemplate::Builtin(BuiltinTemplate::primitive(primitive));
            scope.names.insert(primitive.into(), NamedItem::Type(NamedType::Template(Arc::new(template))));
        }
    }
}