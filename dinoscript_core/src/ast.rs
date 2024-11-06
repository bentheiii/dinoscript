pub mod pairable {
    use std::{fmt::Debug, panic::Location};

    pub struct WithPair<'s, T> {
        pub inner: T,
        pub pair: pest::iterators::Pair<'s, crate::grammar::Rule>,
        pub location: Location<'static>,
    }

    pub trait Pairable<'s>: Sized {
        #[track_caller]
        fn with_pair(self, pair: pest::iterators::Pair<'s, crate::grammar::Rule>) -> WithPair<'s, Self> {
            WithPair { inner: self, pair, location: *Location::caller() }
        }
    }

    impl<'s, T: Debug> Debug for WithPair<'s, T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?}", self.inner)
        }
    }
}

pub mod ty {

    use std::borrow::Cow;

    use super::pairable::{Pairable, WithPair};
    #[derive(Debug)]
    pub enum Ty<'s> {
        Tuple(Vec<TyWithPair<'s>>),
        Fn(FnTy<'s>),
        Specialized(SpecializedTy<'s>),
    }

    impl<'s> Ty<'s> {
        pub fn to_in_code(&self) -> String {
            match self {
                Ty::Tuple(tys) => {
                    format!(
                        "({})",
                        tys.iter().map(|t| t.inner.to_in_code()).collect::<Vec<_>>().join(", ")
                    )
                }
                Ty::Fn(fn_ty) => {
                    let args = fn_ty
                        .args
                        .iter()
                        .map(|t| t.inner.to_in_code())
                        .collect::<Vec<_>>()
                        .join(", ");
                    let ret = fn_ty.ret.inner.to_in_code();
                    format!("({}) -> ({})", args, ret)
                }
                Ty::Specialized(specialized_ty) => {
                    if specialized_ty.args.is_empty() {
                        return specialized_ty.name.to_string();
                    }
                    let args = specialized_ty
                        .args
                        .iter()
                        .map(|t| t.inner.to_in_code())
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{}<{}>", specialized_ty.name, args)
                }
            }
        }
    }

    impl<'s> Pairable<'s> for Ty<'s> {}

    pub type TyWithPair<'s> = WithPair<'s, Ty<'s>>;

    #[derive(Debug)]
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

    #[derive(Debug)]
    pub struct SpecializedTy<'s> {
        pub name: Cow<'s, str>,
        pub args: Vec<TyWithPair<'s>>,
    }

    impl<'s> SpecializedTy<'s> {
        pub fn new(name: Cow<'s, str>, args: Vec<TyWithPair<'s>>) -> Self {
            SpecializedTy { name, args }
        }
    }
}

pub mod expression {
    use super::{
        pairable::{Pairable, WithPair},
        statement::{FnArg, StmtWithPair},
        ty::TyWithPair,
    };
    use std::borrow::Cow;

    pub(crate) type ExprWithPair<'s> = WithPair<'s, Expr<'s>>;

    #[derive(Debug)]
    pub(crate) enum Expr<'s> {
        LitInt(i128),
        LitBool(bool),
        LitFloat(f64),
        LitString(Cow<'s, str>),
        Ref(Cow<'s, str>),
        Disambiguation(Disambiguation<'s>),
        Attr(Attr<'s>),
        Tuple(Vec<ExprWithPair<'s>>),
        VariantAccess(Variant<'s>),
        VariantAccessOpt(Variant<'s>),
        MethodCall(MethodCall<'s>),
        Call(Call<'s>),
        Formatted(Vec<FormattedPart<'s>>),
        Lookup(Lookup<'s>),
        Array(Vec<ExprWithPair<'s>>),
        Lambda(Lambda<'s>),
    }

    impl<'s> Pairable<'s> for Expr<'s> {}

    #[derive(Debug)]
    pub struct Lookup<'s> {
        pub obj: Box<ExprWithPair<'s>>,
        pub keys: Vec<ExprWithPair<'s>>,
    }

    #[derive(Debug)]
    pub enum FormattedPart<'s> {
        Literal(Cow<'s, str>),
        Expr(FormattedExpression<'s>),
    }

    #[derive(Debug)]
    pub struct FormattedExpression<'s> {
        expr: ExprWithPair<'s>,
        format: Option<Cow<'s, str>>,
    }

    #[derive(Debug)]
    pub struct Attr<'s> {
        pub obj: Box<ExprWithPair<'s>>,
        pub name: Cow<'s, str>,
    }

    #[derive(Debug)]
    pub struct Variant<'s> {
        pub obj: Box<ExprWithPair<'s>>,
        pub name: Cow<'s, str>,
    }

    #[derive(Debug)]
    pub struct Disambiguation<'s> {
        pub name: Cow<'s, str>,
        pub arg_tys: Vec<TyWithPair<'s>>,
    }

    #[derive(Debug)]
    pub struct SpecializedFunctor<'s> {
        name: Cow<'s, str>,
        args: Vec<TyWithPair<'s>>,
    }

    #[derive(Debug)]
    pub enum Functor<'s> {
        Expr(Box<ExprWithPair<'s>>),
        Operator(WithPair<'s, Operator>),
        Specialized(SpecializedFunctor<'s>),
    }

    #[derive(Debug)]
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

    impl<'s> Pairable<'s> for Operator {}

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

    #[derive(Debug)]
    pub struct Call<'s> {
        pub functor: Functor<'s>,
        pub args: Vec<ExprWithPair<'s>>,
    }

    #[derive(Debug)]
    pub struct MethodCall<'s> {
        pub obj: Box<ExprWithPair<'s>>,
        pub name: Cow<'s, str>,
        pub args: Vec<ExprWithPair<'s>>,
    }

    #[derive(Debug)]
    pub struct Lambda<'s> {
        pub args: Vec<FnArg<'s>>,
        pub body: Vec<StmtWithPair<'s>>,
        pub ret: Box<ExprWithPair<'s>>,
    }
}

pub mod statement {
    use super::expression::ExprWithPair;
    use super::pairable::{Pairable, WithPair};
    use super::ty::TyWithPair;
    use std::borrow::Cow;

    pub type StmtWithPair<'s> = WithPair<'s, Stmt<'s>>;

    #[derive(Debug)]
    pub enum Stmt<'s> {
        Let(Let<'s>),
        Fn(Fn<'s>),
        Type(Type<'s>),
        Compound(Compound<'s>),
    }

    impl<'s> Pairable<'s> for Stmt<'s> {}

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
            Field { name: name.into(), ty }
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

    #[derive(Debug)]
    pub enum FnArgDefault<'s> {
        Value(ExprWithPair<'s>),
        ResolveOverload(ResolveOverload<'s>),
    }

    #[derive(Debug)]
    pub struct ResolveOverload<'s> {
        pub name: Cow<'s, str>,
    }

    impl<'s> ResolveOverload<'s> {
        pub fn new(name: impl Into<Cow<'s, str>>) -> ResolveOverload<'s> {
            ResolveOverload { name: name.into() }
        }
    }
}
