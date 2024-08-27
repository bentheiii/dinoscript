use crate::compilation_scope::CompilationScope;

pub trait DinoPack {
    fn setup(&self, scope: &mut CompilationScope);
}