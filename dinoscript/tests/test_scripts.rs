use std::fs;

use dinoscript_core::bytecode::Command;
use dinoscript_core::compilation_scope::{NamedItem, Overload, OverloadLoc};
use dinoscript_core::dinopack::DinoPack;
use dinoscript_core::runtime::RuntimeFrame;
use dinoscript_core::{compilation_scope::CompilationScope, grammar::parse_raw_statements};
use dinoscript_std::StdPack;
use glob::glob;
use itertools::Itertools;

fn test_script(script_number: usize){
    let file_pattern = format!("test_scripts/{script_number:0>3}_*.ds");
    let file_path = glob(&file_pattern)
        .unwrap()
        .exactly_one()
        .map_err(|e| {
            format!(
                "multiple files matched {file_pattern}: {}",
                e.map(|b| format!("{b:?}")).join(", ")
            )
        })
        .unwrap()
        .unwrap();
    let input = fs::read_to_string(&file_path)
        .unwrap_or_else(|_| panic!("{}", file_path.to_str().unwrap().to_string()));

    let statements = parse_raw_statements(&input).unwrap();
    
    let mut scope = CompilationScope::root();
    let core_pack = StdPack;
    core_pack.setup_compiler(&mut scope);
    let mut commands = Vec::new();
    for stmt in statements {
        scope.feed_statement(&stmt, &mut commands).unwrap();
    }

    // we need to find the "main" function
    let NamedItem::Overloads(main_overload) = scope.names.get("main").expect("main function nor found") else {
        panic!("main is not an overload");
    };
    let Overload{loc: main_loc, ..} = main_overload.overloads.iter().next().expect("main overload not found");
    let OverloadLoc::Cell(main_cell) = main_loc else {
        panic!("main overload is not a cell");
    };
    let main_cell = *main_cell;

    let push_command = Command::PushFromCell(main_cell);
    let mut runtime_frame = RuntimeFrame::root(scope.n_cells);
    core_pack.setup_runtime(&mut runtime_frame);
    for com in commands.iter() {
        runtime_frame.execute(com).unwrap();
    }
    // we artificially run the main cell
    runtime_frame.execute(&push_command).unwrap();
    runtime_frame.execute(&Command::MakePending(0)).unwrap();
    runtime_frame.execute(&Command::EvalTop).unwrap();
    
    // result of main should now be on top of the stack
    let result = runtime_frame.stack.pop().unwrap();
    println!("{:?}", result);
}

#[test]
fn test_script_001(){
    test_script(1);
}