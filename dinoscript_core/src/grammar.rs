use std::{borrow::Cow, sync::LazyLock};

use pest::{
    iterators::Pair,
    pratt_parser::{Assoc, Op, PrattParser},
    Parser,
};
use pest_derive::Parser;

use crate::ast::{
    expression::{Attr, Call, Disambiguation, Expr, ExprWithPair, Functor, Lookup, MethodCall, Operator, Variant},
    pairable::Pairable,
    statement::{Compound, CompoundKind, Field, Fn, FnArg, FnArgDefault, Let, ResolveOverload, Stmt, StmtWithPair},
    ty::{FnTy, SpecializedTy, Ty, TyWithPair},
};

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct DinoParse;

fn parse_type(input: Pair<'_, Rule>) -> Result<TyWithPair<'_>, ()> {
    debug_assert!(matches!(input.as_rule(), Rule::complete_type), "{:?}", input);
    let input_marker = input.clone();
    let mut inner = input.into_inner();
    let first = inner.next().unwrap();
    match first.as_rule() {
        Rule::signature => {
            let mut inner = first.into_inner();
            let [args, out] = [inner.next().unwrap(), inner.next().unwrap()];
            let args = args
                .into_inner()
                .next()
                .map(|p| p.into_inner().map(|p| parse_type(p)).collect::<Result<_, _>>())
                .transpose()?
                .unwrap_or_default();
            let out = parse_type(out)?;
            let fn_ty = FnTy::new(args, out);
            return Ok(Ty::Fn(fn_ty).with_pair(input_marker));
        }
        Rule::tup_type => {
            let mut parts = Vec::new();
            for part in first.into_inner() {
                let ty = parse_type(part)?;
                parts.push(ty);
            }
            return Ok(Ty::Tuple(parts).with_pair(input_marker));
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
            })
            .with_pair(input_marker));
        }
    }
}

fn parse_expr3(input: Pair<'_, Rule>) -> Result<ExprWithPair<'_>, ()> {
    debug_assert!(matches!(input.as_rule(), Rule::expression3));
    let inner = input.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::STRING => {
            let inner = inner.into_inner().next().unwrap();
            match inner.as_rule() {
                Rule::single_quote_str => {
                    let inner = inner.into_inner().next().unwrap();
                    let s = inner.as_str(); // todo string escapes
                    return Ok(Expr::LitString(s.into()).with_pair(inner));
                }
                Rule::double_quote_str => {
                    let inner = inner.into_inner().next().unwrap();
                    let s = inner.as_str(); // todo string escapes
                    return Ok(Expr::LitString(s.into()).with_pair(inner));
                }
                _ => {
                    todo!()
                }
            }
        }
        Rule::RAW_STRING => {
            let inner = inner.into_inner().next().unwrap();
            let s = inner.as_str(); // todo string escapes
            return Ok(Expr::LitString(s.into()).with_pair(inner));
        }
        Rule::FORMATTED_STRING => {
            todo!()
        }
        Rule::bool => {
            let b = inner.as_str().parse().unwrap();
            return Ok(Expr::LitBool(b).with_pair(inner));
        }
        Rule::NUMBER_ANY => {
            // todo handle hex, bin, digit seps, floats, etc
            let mut to_parse = Cow::Borrowed(inner.as_str());
            if to_parse.contains('_') {
                to_parse = Cow::Owned(to_parse.replace("_", ""));
            }
            if let Some(whole) = to_parse
                .parse::<i128>()
                .ok()
                .or_else(|| {
                    to_parse
                        .strip_prefix("0x")
                        .and_then(|s| i128::from_str_radix(s, 16).ok())
                })
                .or_else(|| {
                    to_parse
                        .strip_prefix("0b")
                        .and_then(|s| i128::from_str_radix(s, 2).ok())
                })
            {
                Ok(Expr::LitInt(whole).with_pair(inner))
            } else if let Ok(whole) = to_parse.parse::<f64>() {
                Ok(Expr::LitFloat(whole).with_pair(inner))
            } else {
                unreachable!() // todo is this unreachable?
            }
        }
        Rule::expression => {
            return parse_expr(inner);
        }
        Rule::tuple => {
            let pair_mark = inner.clone();
            let mut parts = Vec::new();
            for part in inner.into_inner() {
                let expr = parse_expr(part)?;
                parts.push(expr);
            }
            return Ok(Expr::Tuple(parts).with_pair(pair_mark));
        }
        Rule::disambiguation => {
            let pair_mark = inner.clone();
            let mut child = inner.into_inner();
            let name = child.next().unwrap().as_str();
            let mut args = Vec::new();
            for arg in child
                .next()
                .unwrap()
                .into_inner()
                .next()
                .map(|p| p.into_inner())
                .into_iter()
                .flatten()
            {
                let ty = parse_type(arg)?;
                args.push(ty);
            }
            return Ok(Expr::Disambiguation(Disambiguation {
                name: name.into(),
                arg_tys: args,
            })
            .with_pair(pair_mark));
        }
        Rule::CNAME => {
            let name = inner.as_str();
            return Ok(Expr::Ref(name.into()).with_pair(inner));
        }
        Rule::container => {
            let ret = inner
                .clone()
                .into_inner()
                .next()
                .map(|p| p.into_inner().map(parse_expr).collect::<Result<Vec<_>, _>>())
                .transpose()?
                .unwrap_or_default();
            return Ok(Expr::Array(ret).with_pair(inner));
        }
        _ => {
            unreachable!()
        }
    }
}

fn parse_expr2(input: Pair<'_, Rule>) -> Result<ExprWithPair<'_>, ()> {
    debug_assert!(matches!(input.as_rule(), Rule::expression2));
    let mut inners = input.into_inner();
    let mut ret = parse_expr3(inners.next().unwrap())?;
    for inner in inners {
        let pair_marker = inner.clone();
        ret = match inner.as_rule() {
            Rule::method => {
                let mut inner = inner.into_inner();
                let [method_name, arg_pairs] = [inner.next().unwrap(), inner.next().unwrap()];
                let mut args = Vec::new();
                for arg in arg_pairs
                    .into_inner()
                    .next()
                    .map(|p| p.into_inner().collect())
                    .unwrap_or(Vec::new())
                {
                    let expr = parse_expr(arg)?;
                    args.push(expr);
                }
                Expr::MethodCall(MethodCall {
                    obj: Box::new(ret),
                    name: method_name.as_str().into(),
                    args,
                })
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
                Expr::Call(Call {
                    functor: Functor::Expr(Box::new(ret)),
                    args,
                })
            }
            Rule::member => {
                let member_name = inner.into_inner().next().unwrap().as_str();
                Expr::Attr(Attr {
                    obj: Box::new(ret),
                    name: member_name.into(),
                })
            }
            Rule::member_opt_value => Expr::VariantAccessOpt(Variant {
                obj: Box::new(ret),
                name: inner.into_inner().next().unwrap().as_str().into(),
            }),
            Rule::member_value => Expr::VariantAccess(Variant {
                obj: Box::new(ret),
                name: inner.into_inner().next().unwrap().as_str().into(),
            }),
            Rule::index => {
                let mut args = Vec::new();
                for arg in inner.into_inner().next().unwrap().into_inner() {
                    let expr = parse_expr(arg)?;
                    args.push(expr);
                }
                Expr::Lookup(Lookup {
                    obj: Box::new(ret),
                    keys: args,
                })
            }
            _ => {
                unreachable!()
            }
        }
        .with_pair(pair_marker);
    }
    Ok(ret)
}

fn parse_expr1(input: Pair<'_, Rule>) -> Result<ExprWithPair<'_>, ()> {
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
        }
        .with_pair(inner.clone());

        expr = Expr::Call(Call {
            functor: Functor::Operator(op),
            args: vec![expr],
        })
        .with_pair(inner);
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

fn parse_expr(input: Pair<'_, Rule>) -> Result<ExprWithPair<'_>, ()> {
    debug_assert!(matches!(input.as_rule(), Rule::expression));
    (*BIN_PRATT)
        .map_primary(parse_expr1)
        .map_infix(|lhs, op_pair, rhs| {
            let op = match op_pair.as_rule() {
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
            }
            .with_pair(op_pair.clone());
            Ok(Expr::Call(Call {
                functor: Functor::Operator(op),
                args: vec![lhs?, rhs?],
            })
            .with_pair(op_pair))
        })
        .parse(input.into_inner())
}

fn parse_function(input: Pair<'_, Rule>) -> Result<StmtWithPair<'_>, ()> {
    debug_assert!(matches!(input.as_rule(), Rule::function));
    let pair = input.clone();
    let mut inners = input.into_inner();
    let [raw_name, gen_params, params, ret_ty, body] = [
        inners.next().unwrap(),
        inners.next().unwrap(),
        inners.next().unwrap(),
        inners.next().unwrap(),
        inners.next().unwrap(),
    ];
    let name = raw_name.as_str();
    let gen_params = gen_params
        .into_inner()
        .next()
        .map(|g_pair| g_pair.into_inner().map(|p| p.as_str().into()).collect())
        .unwrap_or_default();
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
                    let default = if let Some(default) = inner.next() {
                        match default.as_rule() {
                            Rule::expr_default => {
                                let expr_pair = default.into_inner().next().unwrap();
                                Some(FnArgDefault::Value(parse_expr(expr_pair)?))
                            }
                            Rule::resolve_default => {
                                let name = default.into_inner().next().unwrap().as_str();
                                Some(FnArgDefault::ResolveOverload(ResolveOverload::new(name)))
                            }
                            _ => {
                                unreachable!()
                            }
                        }
                    } else {
                        None
                    };
                    Ok(FnArg {
                        name: name.into(),
                        ty,
                        default,
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
    })
    .with_pair(pair));
}

pub fn parse_raw_function(input: &str) -> Result<StmtWithPair<'_>, Box<pest::error::Error<Rule>>> {
    return parse_function(DinoParse::parse(Rule::function, input)?.next().unwrap()).map_err(|_e| todo!());
}

fn parse_statement(input: Pair<'_, Rule>) -> Result<StmtWithPair<'_>, ()> {
    debug_assert!(matches!(input.as_rule(), Rule::declaration));
    let input_marker = input.clone();
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
            })
            .with_pair(input_marker));
        }
        Rule::function => {
            return parse_function(first);
        }

        Rule::compound_def => {
            let mut inner = first.into_inner();
            let [kind, raw_name, gen_params, fields] = [
                inner.next().unwrap(),
                inner.next().unwrap(),
                inner.next().unwrap(),
                inner.next().unwrap(),
            ];
            let name = raw_name.as_str();
            let kind = match kind.as_str() {
                "struct" => CompoundKind::Struct,
                "union" => CompoundKind::Union,
                _ => unreachable!(),
            };
            let gen_params = gen_params
                .into_inner()
                .next()
                .map(|p| p.into_inner().map(|p| p.as_str().into()).collect())
                .unwrap_or_default();
            let fields = fields
                .into_inner()
                .map(|field| {
                    let mut inner = field.into_inner();
                    let [name, ty] = [inner.next().unwrap(), inner.next().unwrap()];
                    let name = name.as_str();
                    let ty = parse_type(ty)?;
                    Ok(Field::new(name, ty))
                })
                .collect::<Result<_, _>>()?;
            return Ok(Stmt::Compound(Compound::new(kind, name, gen_params, fields)).with_pair(input_marker));
        }
        Rule::type_def => {
            todo!()
        }
        _ => {
            unreachable!()
        }
    }
}

fn parse_execution(input: Pair<'_, Rule>) -> Result<Vec<StmtWithPair<'_>>, ()> {
    debug_assert!(matches!(input.as_rule(), Rule::execution));
    return input.into_inner().map(parse_statement).collect();
}

pub fn parse_raw_statements(input: &str) -> Result<Vec<StmtWithPair<'_>>, Box<pest::error::Error<Rule>>> {
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
