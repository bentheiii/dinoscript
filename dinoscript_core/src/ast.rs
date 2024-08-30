pub mod ty {

    use std::borrow::Cow;
    #[derive(Debug, Clone)]
    pub enum Ty<'s> {
        Ref(Cow<'s, str>),
        Tuple(Vec<Ty<'s>>),
        Fn(FnTy<'s>),
        Specialized(SpecializedTy<'s>),
    }

    #[derive(Debug, Clone)]
    pub struct FnTy<'s> {
        pub args: Vec<Ty<'s>>,
        pub ret: Box<Ty<'s>>,
    }

    #[derive(Debug, Clone)]
    pub struct SpecializedTy<'s> {
        pub name: Cow<'s, str>,
        pub args: Vec<Ty<'s>>,
    }
}

pub mod expression {
    use super::ty::Ty;
    use std::borrow::Cow;

    #[derive(Debug, Clone)]
    pub enum Expr<'s> {
        LitInt(i64),
        LitBool(bool),
        LitFloat(f64),
        LitString(Cow<'s, str>),
        Ref(Cow<'s, str>),
        Disambiguation(Disambiguation<'s>),
        Attr(Attr<'s>),
        Tuple(Vec<Expr<'s>>),
        Variant(Variant<'s>),
        VariantOpt(Variant<'s>),
        MethodCall(MethodCall<'s>),
        Call(Call<'s>),
        Formatted(Vec<FormattedPart<'s>>),
        Lookup(Lookup<'s>),
    }

    #[derive(Debug, Clone)]
    pub struct Lookup<'s> {
        pub obj: Box<Expr<'s>>,
        pub keys: Vec<Expr<'s>>,
    }

    #[derive(Debug, Clone)]
    pub enum FormattedPart<'s> {
        Literal(Cow<'s, str>),
        Expr(FormattedExpression<'s>),
    }

    #[derive(Debug, Clone)]
    struct FormattedExpression<'s> {
        expr: Expr<'s>,
        format: Option<Cow<'s, str>>,
    }

    #[derive(Debug, Clone)]
    pub struct Attr<'s> {
        pub obj: Box<Expr<'s>>,
        pub name: Cow<'s, str>,
    }

    #[derive(Debug, Clone)]
    pub struct Variant<'s> {
        pub obj: Box<Expr<'s>>,
        pub name: Cow<'s, str>,
    }

    #[derive(Debug, Clone)]
    pub struct Disambiguation<'s> {
        pub name: Cow<'s, str>,
        pub arg_tys: Vec<Ty<'s>>,
    }

    #[derive(Debug, Clone)]
    pub struct SpecializedFunctor<'s> {
        name: Cow<'s, str>,
        args: Vec<Ty<'s>>,
    }

    #[derive(Debug, Clone)]
    pub enum Functor<'s> {
        Expr(Box<Expr<'s>>),
        Operator(Operator),
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
        pub args: Vec<Expr<'s>>,
    }

    #[derive(Debug, Clone)]
    pub struct MethodCall<'s> {
        pub obj: Box<Expr<'s>>,
        pub name: Cow<'s, str>,
        pub args: Vec<Expr<'s>>,
    }
}

pub mod statement {

    use super::expression::Expr;
    use super::ty::Ty;
    use std::borrow::Cow;

    pub enum Stmt<'s> {
        Let(Let<'s>),
        Fn(Fn<'s>),
        Type(Type<'s>),
        Compound(Compound<'s>),
    }

    pub struct Type<'s> {
        name: Cow<'s, str>,
        generic_params: Vec<Cow<'s, str>>,
        value: Ty<'s>,
    }

    pub struct Compound<'s> {
        kind: CompoundKind,
        name: Cow<'s, str>,
        generic_params: Vec<Cow<'s, str>>,
        fields: Vec<Field<'s>>,
    }

    pub enum CompoundKind {
        Struct,
        Union,
    }

    pub struct Field<'s> {
        name: Cow<'s, str>,
        ty: Ty<'s>,
    }

    pub struct Let<'s> {
        pub var: Cow<'s, str>,
        pub ty: Option<Ty<'s>>,
        pub expr: Expr<'s>,
    }

    pub struct Fn<'s> {
        pub name: Cow<'s, str>,
        pub generic_params: Vec<Cow<'s, str>>,
        pub args: Vec<FnArg<'s>>,
        pub return_ty: Ty<'s>,
        pub body: Vec<Stmt<'s>>,
        pub ret: Expr<'s>,
    }

    pub struct FnArg<'s> {
        pub name: Cow<'s, str>,
        pub ty: Ty<'s>,
        pub default: Option<FnArgDefault<'s>>,
    }

    #[derive(Debug, Clone)]
    pub enum FnArgDefault<'s> {
        Value(Expr<'s>),
        ResolveOverload(ResolveOverload<'s>),
    }

    #[derive(Debug, Clone)]
    pub struct ResolveOverload<'s> {
        name: Cow<'s, str>,
        arg_tys: Vec<Ty<'s>>,
        ret_ty: Ty<'s>,
    }
}
