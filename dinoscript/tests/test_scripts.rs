use std::fs;

use dinoscript_core::bytecode::Command;
use dinoscript_core::compilation_scope::{Location, NamedItem};
use dinoscript_core::dinobj::{DinObject, StackItem};
use dinoscript_core::dinopack::DinoPack;
use dinoscript_core::runtime::{Runtime, RuntimeFrame};
use dinoscript_core::{compilation_scope::CompilationScope, grammar::parse_raw_statements};
use dinoscript_std::StdPack;
use glob::glob;
use itertools::Itertools;
use stdext::function_name;

const SHOW_COMMANDS: bool = false;

fn test_script(script_number: usize) {
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
    let input =
        fs::read_to_string(&file_path).unwrap_or_else(|_| panic!("{}", file_path.to_str().unwrap().to_string()));

    let statements = parse_raw_statements(&input).unwrap();

    let mut scope = CompilationScope::root();
    let core_pack = StdPack;
    core_pack.setup_compiler(&mut scope);
    let mut commands = Vec::new();
    println!("----compiling script {}----", script_number);
    for stmt in statements {
        if let Err(err) = scope.feed_statement(&stmt, &mut commands) {
            panic!("compilation error: {}", err);
        }
    }
    println!("----compiled script {}----", script_number);

    // we need to find the "main" function
    let NamedItem::Overloads(main_overload) = scope.names.get("main").expect("main function t found") else {
        panic!("main is not an overload");
    };
    let main_loc = main_overload.overloads.first().expect("main overload not found").loc();
    let Location::Cell(main_cell) = main_loc else {
        panic!("main overload is not a cell");
    };
    let main_cell = *main_cell;

    if SHOW_COMMANDS {
        println!("----commands for {}----", script_number);
        println!("{:#?}", commands);
    }

    {
        let push_command = Command::PushFromCell(main_cell);
        let runtime = Runtime::new();
        let mut runtime_frame = RuntimeFrame::root(scope.n_cells, &runtime);
        core_pack.setup_runtime(&mut runtime_frame);
        println!("----executing script {}----", script_number);
        for com in commands.iter() {
            runtime_frame.execute(com).unwrap();
        }
        println!("----running main {}----", script_number);
        // we artificially run the main cell
        runtime_frame.execute(&push_command).unwrap();
        runtime_frame.execute(&Command::MakePending(0)).unwrap();
        runtime_frame.execute(&Command::EvalTop).unwrap();

        // result of main should now be on top of the stack
        let popped = runtime_frame.stack.pop().unwrap();
        let StackItem::Value(Ok(result_ref)) = popped else {
            panic!("main did not return a value, got {:?}", popped);
        };
        let DinObject::Bool(true) = result_ref.as_ref() else {
            panic!("main did not return true, got {:?}", result_ref);
        };
        if !runtime_frame.stack.is_empty() {
            panic!("stack is not empty after main execution, got {:?}", runtime_frame.stack);
        }
        println!("----script {} passed----", script_number);
        drop(runtime_frame);
        println!("----dropped root frame----");
        drop(runtime);
    };
}

fn test_script_from_name(name: &str) {
    let script_number = name[name.len() - 3..].parse().unwrap();
    test_script(script_number);
}

#[test]
fn test_script_001() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_002() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_003() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_004() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_005() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_006() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_007() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_008() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_009() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_010() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_011() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_012() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_013() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_014() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_015() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_016() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_017() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_018() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_019() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_020() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_021() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_022() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_023() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_024() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_025() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_026() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_027() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_028() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_029() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_030() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_031() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_032() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_033() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_034() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_035() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_036() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_037() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_038() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_039() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_040() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_041() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_042() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_043() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_044() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_045() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_046() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_047() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_048() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_049() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_050() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_051() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_052() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_053() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_054() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_055() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_056() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_057() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_058() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_059() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_060() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_061() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_062() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_063() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_064() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_065() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_066() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_067() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_068() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_069() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_070() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_071() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_072() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_073() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_074() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_075() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_076() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_077() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_078() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_079() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_080() {
    test_script_from_name(function_name!());
}

#[test]
fn test_script_081() {
    test_script_from_name(function_name!());
}
