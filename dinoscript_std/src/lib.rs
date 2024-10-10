mod items;
use items::{pre_items_setup, setup_items, Builtins, SOURCE_ID};

use dinoscript_core::{
    compilation_scope::CompilationScope, dinopack::DinoPack, maybe_owned::MaybeOwned, runtime::RuntimeFrame,
};

pub struct StdPack;

impl DinoPack for StdPack {
    type Builtins<'s> = Builtins<'s>;

    fn setup_compiler<'s>(&self, scope: &mut CompilationScope<'_, 's, Self::Builtins<'s>>) {
        let source_id = SOURCE_ID;

        let builtins = pre_items_setup(scope);

        let mut next_id = 0;
        let mut get_id = || {
            let ret = next_id;
            next_id += 1;
            ret
        };

        scope.builtins = Some(MaybeOwned::Owned(builtins));

        for item in setup_items() {
            item.push_to_compilation(scope, source_id, &mut get_id);
        }
    }

    fn setup_runtime(&self, frame: &mut RuntimeFrame) {
        let source_id = SOURCE_ID;
        let mut items = Vec::new();

        for item in setup_items() {
            items.push(item.to_dinobject(frame.runtime).unwrap());
        }

        frame.add_source(source_id, items);
    }
}
