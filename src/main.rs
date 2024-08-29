
#[macro_use]
mod unique;

mod ast;
mod compilation_scope;
mod bytecode;
mod core;
mod dinopack;
mod dinobj;
mod runtime;
mod grammar;

use core::CorePack;
use std::sync::Arc;

use ast::statement::{self, FnArg, Stmt};
use compilation_scope::{ty::{BuiltinTemplate, TyTemplate}, CompilationScope};
use dinopack::DinoPack;
use grammar::{parse_raw_statement, parse_raw_statements};
use runtime::RuntimeFrame;

fn main() {

    let mut scope = CompilationScope::root();
    
    let core_pack = CorePack;
    core_pack.setup(&mut scope);
    /*
    let stmt1 = Stmt::Let(ast::statement::Let{
        var: "x".into(),
        ty: None,
        expr: ast::expression::Expr::LitInt(5),
    });
    let stmt_fn = Stmt::Fn(statement::Fn{
        name: "f".into(),
        generic_params: Vec::new(),
        args: vec![
            FnArg{
                name: "g".into(),
                ty: ast::ty::Ty::Ref("int".into()),
                default: None,
            }
        ],
        return_ty: ast::ty::Ty::Ref("str".into()),
        body: Vec::new(),
        ret: ast::expression::Expr::LitString("hello".into()),
    });
    let stmt2 = Stmt::Let(ast::statement::Let{
        var: "y".into(),
        ty: None,
        expr: ast::expression::Expr::Call(
            ast::expression::Call{
            functor: ast::expression::Functor::Expr(Box::new(ast::expression::Expr::Ref("f".into()))),
            args: vec![
                ast::expression::Expr::Ref("x".into()),
            ],
        }),
    });
    let mut sink = Vec::new();
    scope.feed_statement(&stmt1, &mut sink).unwrap();
    scope.feed_statement(&stmt_fn, &mut sink).unwrap();
    scope.feed_statement(&stmt2, &mut sink).unwrap();
    println!("{:#?}", sink);
    println!("{:?}", scope.names.get("y".into()));

    let mut runtime_frame = RuntimeFrame::root(3);
    for com in sink.iter() {
        runtime_frame.execute(com).unwrap();
    }
    println!("{:?}", runtime_frame.cells[2]);
    */

    let inp = r#"
    let x = 5;
    fn f(g: int) -> str {
        "hello"
    }
    let y = f(x);
    "#;
    let mut sink = Vec::new();
    let stmts = parse_raw_statements(inp).unwrap();
    for stmt in stmts {
        scope.feed_statement(&stmt, &mut sink).unwrap();
    }
    let mut runtime_frame = RuntimeFrame::root(scope.n_cells);
    for com in sink.iter() {
        runtime_frame.execute(com).unwrap();
    }
    println!("{:?}", runtime_frame.cells[2]);
}
