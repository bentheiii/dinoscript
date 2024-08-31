use dinoscript_std::StdPack;
use dinoscript_core::compilation_scope::CompilationScope;
use dinoscript_core::dinopack::DinoPack;

fn main(){
    let mut scope = CompilationScope::root();

    let core_pack = StdPack;
    core_pack.setup_compiler(&mut scope);
}