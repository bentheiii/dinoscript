use std::{cell::LazyCell, sync::LazyLock};

use pest::{
    iterators::Pair,
    pratt_parser::{Assoc, Op, PrattParser},
    Parser,
};
use pest_derive::Parser;

use crate::ast::{
    expression::{Attr, Call, Disambiguation, Expr, Functor, Lookup, MethodCall, Operator, Variant},
    statement::{Fn, FnArg, Let, Stmt, Type},
    ty::{SpecializedTy, Ty},
};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct DinoParse;

fn parse_type<'s>(input: Pair<'s, Rule>) -> Result<Ty<'s>, ()> {
    debug_assert!(matches!(input.as_rule(), Rule::complete_type));
    let mut inner = input.into_inner();
    let first = inner.next().unwrap();
    match first.as_rule() {
        Rule::signature => {
            todo!()
        }
        Rule::tup_type => {
            let mut parts = Vec::new();
            for part in first.into_inner() {
                let ty = parse_type(part)?;
                parts.push(ty);
            }
            return Ok(Ty::Tuple(parts));
        }
        _ => {
            // CNAME
            let name = first.as_str();
            let generic_args = inner
                .next()
                .map(|p| p.into_inner().map(|p| parse_type(p)).collect::<Result<Vec<_>, _>>())
                .transpose()?
                .unwrap_or_default();
            return Ok(Ty::Specialized(SpecializedTy {
                name: name.into(),
                args: generic_args,
            }));
        }
    }
}

fn parse_expr3<'s>(input: Pair<'s, Rule>) -> Result<Expr<'s>, ()> {
    debug_assert!(matches!(input.as_rule(), Rule::expression3));
    let inner = input.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::STRING => {
            let inner = inner.into_inner().next().unwrap();
            match inner.as_rule() {
                Rule::single_quote_str => {
                    let inner = inner.into_inner().next().unwrap();
                    let s = inner.as_str(); // todo string escapes
                    return Ok(Expr::LitString(s.into()));
                }
                Rule::double_quote_str => {
                    let inner = inner.into_inner().next().unwrap();
                    let s = inner.as_str(); // todo string escapes
                    return Ok(Expr::LitString(s.into()));
                }
                _ => {
                    todo!()
                }
            }
        }
        Rule::RAW_STRING => {
            let inner = inner.into_inner().next().unwrap();
            let s = inner.as_str(); // todo string escapes
            return Ok(Expr::LitString(s.into()));
        }
        Rule::FORMATTED_STRING => {
            todo!()
        }
        Rule::bool => {
            let inner = inner.into_inner().next().unwrap();
            let b = inner.as_str().parse().unwrap();
            return Ok(Expr::LitBool(b));
        }
        Rule::NUMBER_ANY => {
            // todo handle hex, bin, digit seps, floats, etc
            let n = inner.as_str().parse().unwrap();
            return Ok(Expr::LitInt(n));
        }
        Rule::expression => {
            todo!()
        }
        Rule::tuple => {
            let mut parts = Vec::new();
            for part in inner.into_inner() {
                let expr = todo!();
                parts.push(expr);
            }
            return Ok(Expr::Tuple(parts));
        }
        Rule::turbofish_cname => {
            let mut inner = inner.into_inner();
            let name = inner.next().unwrap().as_str();
            let mut args = Vec::new();
            for arg in inner.next().unwrap().into_inner() {
                let ty = parse_type(arg)?;
                args.push(ty);
            }
            return Ok(Expr::Disambiguation(Disambiguation {
                name: name.into(),
                arg_tys: args,
            }));
        }
        Rule::CNAME => {
            let name = inner.as_str();
            return Ok(Expr::Ref(name.into()));
        }
        _ => {
            unreachable!()
        }
    }
}

fn parse_expr2<'s>(input: Pair<'s, Rule>) -> Result<Expr<'s>, ()> {
    debug_assert!(matches!(input.as_rule(), Rule::expression2));
    let mut inners = input.into_inner();
    let mut ret = parse_expr3(inners.next().unwrap())?;
    while let Some(inner) = inners.next() {
        match inner.as_rule() {
            Rule::method => {
                let mut inner = inner.into_inner();
                let [method_name, arg_pairs] = [inner.next().unwrap(), inner.next().unwrap()];
                let mut args = Vec::new();
                for arg in arg_pairs.into_inner() {
                    let expr = parse_expr(arg)?;
                    args.push(expr);
                }
                ret = Expr::MethodCall(MethodCall {
                    obj: Box::new(ret),
                    name: method_name.as_str().into(),
                    args,
                });
            }
            Rule::call => {
                let arg_pairs = inner
                    .into_inner()
                    .next()
                    .unwrap()
                    .into_inner()
                    .next()
                    .map(|p| p.into_inner());
                let mut args = Vec::new();
                for arg in arg_pairs.into_iter().flatten() {
                    let expr = parse_expr(arg)?;
                    args.push(expr);
                }
                ret = Expr::Call(Call {
                    functor: Functor::Expr(Box::new(ret)),
                    args,
                });
            }
            Rule::member => {
                let member_name = inner.into_inner().next().unwrap().as_str();
                ret = Expr::Attr(Attr {
                    obj: Box::new(ret),
                    name: member_name.into(),
                });
            }
            Rule::member_opt_value => {
                ret = Expr::VariantOpt(Variant {
                    obj: Box::new(ret),
                    name: inner.into_inner().next().unwrap().as_str().into(),
                })
            }
            Rule::member_value => {
                ret = Expr::Variant(Variant {
                    obj: Box::new(ret),
                    name: inner.into_inner().next().unwrap().as_str().into(),
                })
            }
            Rule::index => {
                let mut args = Vec::new();
                for arg in inner.into_inner().next().unwrap().into_inner() {
                    let expr = parse_expr(arg)?;
                    args.push(expr);
                }
                ret = Expr::Lookup(Lookup {
                    obj: Box::new(ret),
                    keys: args,
                })
            }
            _ => {
                unreachable!()
            }
        }
    }
    return Ok(ret);
}

fn parse_expr1<'s>(input: Pair<'s, Rule>) -> Result<Expr<'s>, ()> {
    debug_assert!(matches!(input.as_rule(), Rule::expression1));
    let mut iter_rev = input.into_inner().rev();
    let mut expr = parse_expr2(iter_rev.next().unwrap())?;
    for inner in iter_rev {
        let op = match inner.as_rule() {
            Rule::UNARY_PLUS => Operator::UnPos,
            Rule::UNARY_MINUS => Operator::UnNeg,
            Rule::UNARY_NOT => Operator::UnNot,
            Rule::UNARY_INV => Operator::UnInv,
            _ => unreachable!(),
        };

        expr = Expr::Call(Call {
            functor: Functor::Operator(op),
            args: vec![expr],
        });
    }
    Ok(expr)
}

static BIN_PRATT: LazyLock<PrattParser<Rule>> = LazyLock::new(|| {
    PrattParser::new()
        .op(Op::infix(Rule::BINARY_AND, Assoc::Left) | Op::infix(Rule::BINARY_OR, Assoc::Left))
        .op(Op::infix(Rule::BINARY_EQ, Assoc::Left)
            | Op::infix(Rule::BINARY_NE, Assoc::Left)
            | Op::infix(Rule::BINARY_GT, Assoc::Left)
            | Op::infix(Rule::BINARY_LT, Assoc::Left)
            | Op::infix(Rule::BINARY_GE, Assoc::Left)
            | Op::infix(Rule::BINARY_LE, Assoc::Left))
        .op(Op::infix(Rule::BINARY_BIT_AND, Assoc::Left)
            | Op::infix(Rule::BINARY_BIT_OR, Assoc::Left)
            | Op::infix(Rule::BINARY_BIT_XOR, Assoc::Left))
        .op(Op::infix(Rule::BINARY_ADD, Assoc::Left) | Op::infix(Rule::BINARY_SUB, Assoc::Left))
        .op(Op::infix(Rule::BINARY_MUL, Assoc::Left)
            | Op::infix(Rule::BINARY_DIV, Assoc::Left)
            | Op::infix(Rule::BINARY_MOD, Assoc::Left))
        .op(Op::infix(Rule::BINARY_POW, Assoc::Right))
});

fn parse_expr<'s>(input: Pair<'s, Rule>) -> Result<Expr<'s>, ()> {
    debug_assert!(matches!(input.as_rule(), Rule::expression));
    (*BIN_PRATT)
        .map_primary(parse_expr1)
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::BINARY_AND => Operator::BinAnd,
                Rule::BINARY_OR => Operator::BinOr,
                Rule::BINARY_EQ => Operator::BinEq,
                Rule::BINARY_NE => Operator::BinNeq,
                Rule::BINARY_GT => Operator::BinGt,
                Rule::BINARY_LT => Operator::BinLt,
                Rule::BINARY_GE => Operator::BinGte,
                Rule::BINARY_LE => Operator::BinLte,
                Rule::BINARY_BIT_AND => Operator::BinBitAnd,
                Rule::BINARY_BIT_OR => Operator::BinBitOr,
                Rule::BINARY_BIT_XOR => Operator::BinBitXor,
                Rule::BINARY_ADD => Operator::BinAdd,
                Rule::BINARY_SUB => Operator::BinSub,
                Rule::BINARY_MUL => Operator::BinMul,
                Rule::BINARY_DIV => Operator::BinDiv,
                Rule::BINARY_MOD => Operator::BinMod,
                Rule::BINARY_POW => todo!(),
                _ => unreachable!(),
            };
            Ok(Expr::Call(Call {
                functor: Functor::Operator(op),
                args: vec![lhs?, rhs?],
            }))
        })
        .parse(input.into_inner())
}

fn parse_statement<'s>(input: Pair<'s, Rule>) -> Result<Stmt<'s>, ()> {
    debug_assert!(matches!(input.as_rule(), Rule::declaration));
    let mut inner = input.into_inner();
    let first = inner.next().unwrap();
    match first.as_rule() {
        Rule::value => {
            let mut inner = first.into_inner();
            let [raw_name, ty, expr] = [inner.next().unwrap(), inner.next().unwrap(), inner.next().unwrap()];
            let name = raw_name.as_str();
            let ty = ty.into_inner().next().map(parse_type).transpose()?;
            let expr = parse_expr(expr)?;
            return Ok(Stmt::Let(Let {
                var: name.into(),
                ty,
                expr,
            }));
        }
        Rule::function => {
            let mut inners = first.into_inner();
            let [raw_name, gen_params, params, ret_ty, body] = [
                inners.next().unwrap(),
                inners.next().unwrap(),
                inners.next().unwrap(),
                inners.next().unwrap(),
                inners.next().unwrap(),
            ];
            let name = raw_name.as_str();
            let gen_params = gen_params.into_inner().map(|p| p.as_str().into()).collect();
            let params = params
                .into_inner()
                .next()
                .map(|params| {
                    params
                        .into_inner()
                        .map(|p| {
                            let mut inner = p.into_inner();
                            let [name, ty] = [inner.next().unwrap(), inner.next().unwrap()];
                            let name = name.as_str();
                            let ty = parse_type(ty)?;
                            if let Some(default) = inner.next() {
                                todo!()
                            }
                            Ok(FnArg {
                                name: name.into(),
                                ty,
                                default: None,
                            })
                        })
                        .collect()
                })
                .transpose()?
                .unwrap_or_default();
            let ret_ty = parse_type(ret_ty)?;
            let mut body_inner = body.into_inner();
            let [body_exec, ret] = [body_inner.next().unwrap(), body_inner.next().unwrap()];
            let body = parse_execution(body_exec)?;
            let ret = parse_expr(ret)?;
            return Ok(Stmt::Fn(Fn {
                name: name.into(),
                generic_params: gen_params,
                args: params,
                return_ty: ret_ty,
                body,
                ret,
            }));
        }

        Rule::compound_def => {
            todo!()
        }
        Rule::type_def => {
            todo!()
        }
        _ => {
            unreachable!()
        }
    }
}

fn parse_execution<'s>(input: Pair<'s, Rule>) -> Result<Vec<Stmt<'s>>, ()> {
    debug_assert!(matches!(input.as_rule(), Rule::execution));
    return input.into_inner().map(parse_statement).collect();
}

pub fn parse_raw_statements<'s>(input: &'s str) -> Result<Vec<Stmt<'s>>, pest::error::Error<Rule>> {
    return DinoParse::parse(Rule::header, input)?
        .next()
        .unwrap()
        .into_inner()
        .next()
        .unwrap()
        .into_inner()
        .map(parse_statement)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|_e| todo!());
}
