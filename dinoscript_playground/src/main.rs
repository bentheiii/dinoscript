use dinoscript_core::compilation_scope::CompilationScope;
use dinoscript_core::dinopack::DinoPack;
use dinoscript_core::grammar::parse_raw_statements;
use dinoscript_core::runtime::RuntimeFrame;
use dinoscript_std::StdPack;

fn main() {
    let mut scope = CompilationScope::root();

    let core_pack = StdPack;
    core_pack.setup_compiler(&mut scope);

    let inp = r#"
    let x = 5;
    fn f(g: int) -> int {
        //"hello"//+to_string(g)
        g+g
    }
    let y = double(f(x));
    "#;
    let mut sink = Vec::new();
    let stmts = parse_raw_statements(inp).unwrap();
    for stmt in stmts {
        scope.feed_statement(&stmt, &mut sink).unwrap();
    }
    //print!("{}", to_in_code(&sink));
    //return;
    let mut runtime_frame = RuntimeFrame::root(scope.n_cells);
    core_pack.setup_runtime(&mut runtime_frame);
    for com in sink.iter() {
        runtime_frame.execute(com).unwrap();
    }
    println!("{:?}", runtime_frame.cells[2]);
}
