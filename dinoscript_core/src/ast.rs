pub mod pairable{
    #[derive(Debug, Clone)]
    pub struct WithPair<'s, T>{
        pub inner: T,
        pub pair: pest::iterators::Pair<'s, crate::grammar::Rule>
    }

    pub trait Pairable<'s>: Sized{
        fn with_pair(self, pair: pest::iterators::Pair<'s, crate::grammar::Rule>) -> WithPair<'s, Self>{
            WithPair{
                inner: self,
                pair
            }
        }
    }
}

pub mod ty {

    use std::borrow::Cow;

    use super::pairable::{Pairable, WithPair};
    #[derive(Debug, Clone)]
    pub enum Ty<'s> {
        Tuple(Vec<TyWithPair<'s>>),
        Fn(FnTy<'s>),
        Specialized(SpecializedTy<'s>),
    }

    impl<'s> Ty<'s> {
        pub fn to_in_code(&self)->String{
            match self {
                Ty::Tuple(tys) => {
                    format!("({})", tys.iter().map(|t| t.inner.to_in_code()).collect::<Vec<_>>().join(", "))
                },
                Ty::Fn(fn_ty) => {
                    let args = fn_ty.args.iter().map(|t| t.inner.to_in_code()).collect::<Vec<_>>().join(", ");
                    let ret = fn_ty.ret.inner.to_in_code();
                    format!("({}) -> ({})", args, ret)
                },
                Ty::Specialized(specialized_ty) => {
                    if specialized_ty.args.is_empty(){
                        return specialized_ty.name.to_string();
                    }
                    let args = specialized_ty.args.iter().map(|t| t.inner.to_in_code()).collect::<Vec<_>>().join(", ");
                    format!("{}<{}>", specialized_ty.name, args)
                }
            }
        }
    } 

    impl<'s> Pairable<'s> for Ty<'s>{}

    pub type TyWithPair<'s> = WithPair<'s, Ty<'s>>;

    #[derive(Debug, Clone)]
    pub struct FnTy<'s> {
        pub args: Vec<TyWithPair<'s>>,
        pub ret: Box<TyWithPair<'s>>,
    }

    impl<'s> FnTy<'s> {
        pub fn new(args: Vec<TyWithPair<'s>>, ret: TyWithPair<'s>) -> Self {
            FnTy {
                args,
                ret: Box::new(ret),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct SpecializedTy<'s> {
        pub name: Cow<'s, str>,
        pub args: Vec<TyWithPair<'s>>,
    }
}

pub mod expression {
    use super::{pairable::{Pairable, WithPair}, ty::TyWithPair};
    use std::borrow::Cow;

    pub type ExprWithPair<'s> = WithPair<'s, Expr<'s>>;

    #[derive(Debug, Clone)]
    pub enum Expr<'s> {
        LitInt(i64),
        LitBool(bool),
        LitFloat(f64),
        LitString(Cow<'s, str>),
        Ref(Cow<'s, str>),
        Disambiguation(Disambiguation<'s>),
        Attr(Attr<'s>),
        Tuple(Vec<ExprWithPair<'s>>),
        Variant(Variant<'s>),
        VariantOpt(Variant<'s>),
        MethodCall(MethodCall<'s>),
        Call(Call<'s>),
        Formatted(Vec<FormattedPart<'s>>),
        Lookup(Lookup<'s>),
        Array(Vec<ExprWithPair<'s>>),
    }

    impl<'s> Pairable<'s> for Expr<'s>{}

    #[derive(Debug, Clone)]
    pub struct Lookup<'s> {
        pub obj: Box<ExprWithPair<'s>>,
        pub keys: Vec<ExprWithPair<'s>>,
    }

    #[derive(Debug, Clone)]
    pub enum FormattedPart<'s> {
        Literal(Cow<'s, str>),
        Expr(FormattedExpression<'s>),
    }

    #[derive(Debug, Clone)]
    struct FormattedExpression<'s> {
        expr: ExprWithPair<'s>,
        format: Option<Cow<'s, str>>,
    }

    #[derive(Debug, Clone)]
    pub struct Attr<'s> {
        pub obj: Box<ExprWithPair<'s>>,
        pub name: Cow<'s, str>,
    }

    #[derive(Debug, Clone)]
    pub struct Variant<'s> {
        pub obj: Box<ExprWithPair<'s>>,
        pub name: Cow<'s, str>,
    }

    #[derive(Debug, Clone)]
    pub struct Disambiguation<'s> {
        pub name: Cow<'s, str>,
        pub arg_tys: Vec<TyWithPair<'s>>,
    }

    #[derive(Debug, Clone)]
    pub struct SpecializedFunctor<'s> {
        name: Cow<'s, str>,
        args: Vec<TyWithPair<'s>>,
    }

    #[derive(Debug, Clone)]
    pub enum Functor<'s> {
        Expr(Box<ExprWithPair<'s>>),
        Operator(WithPair<'s, Operator>),
        Specialized(SpecializedFunctor<'s>),
    }

    #[derive(Debug, Clone)]
    pub enum Operator {
        BinAdd,
        BinSub,
        BinMul,
        BinDiv,
        BinMod,
        BinAnd,
        BinOr,
        BinEq,
        BinNeq,
        BinGt,
        BinLt,
        BinGte,
        BinLte,
        BinBitAnd,
        BinBitOr,
        BinBitXor,
        UnPos,
        UnNeg,
        UnNot,
        UnInv,
        Lookup,
    }

    impl<'s> Pairable<'s> for Operator{}

    impl Operator {
        pub fn func_name(&self) -> &'static str {
            match self {
                Operator::BinAdd => "add",
                Operator::BinSub => "sub",
                Operator::BinMul => "mul",
                Operator::BinDiv => "div",
                Operator::BinMod => "mod",
                Operator::BinAnd => "and",
                Operator::BinOr => "or",
                Operator::BinEq => "eq",
                Operator::BinNeq => "neq",
                Operator::BinGt => "gt",
                Operator::BinLt => "lt",
                Operator::BinGte => "gte",
                Operator::BinLte => "lte",
                Operator::BinBitAnd => "bitand",
                Operator::BinBitOr => "bitor",
                Operator::BinBitXor => "bitxor",
                Operator::UnPos => "pos",
                Operator::UnNeg => "neg",
                Operator::UnNot => "not",
                Operator::UnInv => "inv",
                Operator::Lookup => "lookup",
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Call<'s> {
        pub functor: Functor<'s>,
        pub args: Vec<ExprWithPair<'s>>,
    }

    #[derive(Debug, Clone)]
    pub struct MethodCall<'s> {
        pub obj: Box<ExprWithPair<'s>>,
        pub name: Cow<'s, str>,
        pub args: Vec<ExprWithPair<'s>>,
    }
}

pub mod statement {
    use crate::grammar::Rule;

    type Pair<'s> = pest::iterators::Pair<'s, Rule>;

    use super::expression::ExprWithPair;
    use super::pairable::{Pairable, WithPair};
    use super::ty::{Ty, TyWithPair};
    use std::borrow::Cow;

    pub type StmtWithPair<'s> = WithPair<'s, Stmt<'s>>;

    #[derive(Debug)]
    pub enum Stmt<'s> {
        Let(Let<'s>),
        Fn(Fn<'s>),
        Type(Type<'s>),
        Compound(Compound<'s>),
    }

    impl<'s> Pairable<'s> for Stmt<'s>{}

    #[derive(Debug)]
    pub struct Type<'s> {
        name: Cow<'s, str>,
        generic_params: Vec<Cow<'s, str>>,
        value: TyWithPair<'s>,
    }

    #[derive(Debug)]
    pub struct Compound<'s> {
        pub name: Cow<'s, str>,
        pub kind: CompoundKind,
        pub generic_params: Vec<Cow<'s, str>>,
        pub fields: Vec<Field<'s>>,
    }

    impl<'s> Compound<'s> {
        pub fn new(
            kind: CompoundKind,
            name: impl Into<Cow<'s, str>>,
            generic_params: Vec<Cow<'s, str>>,
            fields: Vec<Field<'s>>,
        ) -> Compound<'s> {
            Compound {
                kind,
                name: name.into(),
                generic_params,
                fields,
            }
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub enum CompoundKind {
        Struct,
        Union,
    }

    #[derive(Debug)]
    pub struct Field<'s> {
        pub name: Cow<'s, str>,
        pub ty: TyWithPair<'s>,
    }

    impl<'s> Field<'s> {
        pub fn new(name: impl Into<Cow<'s, str>>, ty: TyWithPair<'s>) -> Field<'s> {
            Field {
                name: name.into(),
                ty,
            }
        }
    }

    #[derive(Debug)]
    pub struct Let<'s> {
        pub var: Cow<'s, str>,
        pub ty: Option<TyWithPair<'s>>,
        pub expr: ExprWithPair<'s>,
    }

    #[derive(Debug)]
    pub struct Fn<'s> {
        pub name: Cow<'s, str>,
        pub generic_params: Vec<Cow<'s, str>>,
        pub args: Vec<FnArg<'s>>,
        pub return_ty: TyWithPair<'s>,
        pub body: Vec<StmtWithPair<'s>>,
        pub ret: ExprWithPair<'s>,
    }

    #[derive(Debug)]
    pub struct FnArg<'s> {
        pub name: Cow<'s, str>,
        pub ty: TyWithPair<'s>,
        pub default: Option<FnArgDefault<'s>>,
    }

    #[derive(Debug, Clone)]
    pub enum FnArgDefault<'s> {
        Value(ExprWithPair<'s>),
        ResolveOverload(ResolveOverload<'s>),
    }

    #[derive(Debug, Clone)]
    pub struct ResolveOverload<'s> {
        pub name: Cow<'s, str>,
    }

    impl<'s> ResolveOverload<'s> {
        pub fn new(name: impl Into<Cow<'s, str>>) -> ResolveOverload<'s> {
            ResolveOverload { name: name.into() }
        }
    }
}
