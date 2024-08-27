pub(crate) mod ty{
    
    use std::borrow::Cow;
    #[derive(Debug, Clone)]
    pub(crate) enum Ty<'s> {
        Ref(Cow<'s, str>),
        Tuple(Vec<Ty<'s>>),
        Fn(FnTy<'s>),
        Specialized(SpecializedTy<'s>),
    }

    #[derive(Debug, Clone)]
    pub(crate) struct FnTy<'s> {
        pub(crate) args: Vec<Ty<'s>>,
        pub(crate) ret: Box<Ty<'s>>,
    }

    #[derive(Debug, Clone)]
    pub(crate) struct SpecializedTy<'s> {
        pub(crate) name: Cow<'s, str>,
        pub(crate) args: Vec<Ty<'s>>,
    }
}


pub(crate) mod expression{
    use std::borrow::Cow;
    use super::ty::Ty;

    #[derive(Debug, Clone)]
    pub(crate) enum Expr<'s>{
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
    }


    #[derive(Debug, Clone)]
    pub(crate) struct Attr<'s> {
        pub(crate) obj: Box<Expr<'s>>,
        pub(crate) name: Cow<'s, str>,
    }

    #[derive(Debug, Clone)]
    pub(crate) struct Variant<'s> {
        pub(crate) obj: Box<Expr<'s>>,
        pub(crate) name: Cow<'s, str>,
    }

    #[derive(Debug, Clone)]
    pub(crate) struct Disambiguation<'s> {
        name: Cow<'s, str>,
        arg_tys: Vec<Ty<'s>>,
    }
    
    #[derive(Debug, Clone)]
    pub(crate) struct SpecializedFunctor<'s> {
        name: Cow<'s, str>,
        args: Vec<Ty<'s>>,
    }
    
    #[derive(Debug, Clone)]
    pub(crate) enum Functor<'s> {
        Expr(Box<Expr<'s>>),
        Operator(Operator),
        Specialized(SpecializedFunctor<'s>),
    }

    #[derive(Debug, Clone)]
    pub(crate) enum Operator {
        Add,
        Sub,
        Mul,
        Div,
        Mod,
        And,
        Or,
        Not,
        Eq,
        Neq,
        Gt,
        Lt,
        Gte,
        Lte,
        BitAnd,
        BitOr,
        BitXor,
    }

    impl Operator {
        pub(crate) fn func_name(&self) -> &'static str {
            match self {
                Operator::Add => "op_add",
                Operator::Sub => "op_sub",
                Operator::Mul => "op_mul",
                Operator::Div => "op_div",
                Operator::Mod => "op_mod",
                Operator::And => "op_and",
                Operator::Or => "op_or",
                Operator::Not => "op_not",
                Operator::Eq => "op_eq",
                Operator::Neq => "op_neq",
                Operator::Gt => "op_gt",
                Operator::Lt => "op_lt",
                Operator::Gte => "op_gte",
                Operator::Lte => "op_lte",
                Operator::BitAnd => "op_bitand",
                Operator::BitOr => "op_bitor",
                Operator::BitXor => "op_bitxor",
            }
        }
    }
    
    #[derive(Debug, Clone)]
    pub(crate) struct Call<'s> {
        pub(crate) functor: Functor<'s>,
        pub(crate) args: Vec<Expr<'s>>,
    }

    #[derive(Debug, Clone)]
    pub(crate) struct MethodCall<'s> {
        pub(crate) obj: Box<Expr<'s>>,
        pub(crate) name: Cow<'s, str>,
        pub(crate) args: Vec<Expr<'s>>,
    }
}

pub(crate) mod statement{
    
    use std::borrow::Cow;
    use super::expression::Expr;
    use super::ty::Ty;
    
    pub(crate) enum Stmt<'s> {
        Let(Let<'s>),
        Fn(Fn<'s>),
        Type(Type<'s>),
        Compound(Compound<'s>),
    }

    pub(crate) struct Type<'s> {
        name: Cow<'s, str>,
        generic_params: Vec<Cow<'s, str>>,
        value: Ty<'s>,
    }

    pub(crate) struct Compound<'s> {
        kind: CompoundKind,
        name: Cow<'s, str>,
        generic_params: Vec<Cow<'s, str>>,
        fields: Vec<Field<'s>>,
    }

    pub(crate) enum CompoundKind {
        Struct,
        Union,
    }

    pub(crate) struct Field<'s> {
        name: Cow<'s, str>,
        ty: Ty<'s>,
    }

    pub(crate) struct Let<'s> {
        pub(crate) var: Cow<'s, str>,
        pub(crate) ty: Option<Ty<'s>>,
        pub(crate) expr: Expr<'s>,
    }

    pub(crate) struct Fn<'s> {
        pub(crate) name: Cow<'s, str>,
        pub(crate) generic_params: Vec<Cow<'s, str>>,
        pub(crate) args: Vec<FnArg<'s>>,
        pub(crate) return_ty: Ty<'s>,
        pub(crate) body: Vec<Stmt<'s>>,
        pub(crate) ret: Expr<'s>,
    }

    pub(crate) struct FnArg<'s> {
        pub(crate) name: Cow<'s, str>,
        pub(crate) ty: Ty<'s>,
        pub(crate) default: Option<FnArgDefault<'s>>,
    }

    #[derive(Debug, Clone)]
    pub(crate) enum FnArgDefault<'s> {
        Value(Expr<'s>),
        ResolveOverload(ResolveOverload<'s>),
    }

    #[derive(Debug, Clone)]
    pub(crate) struct ResolveOverload<'s> {
        name: Cow<'s, str>,
        arg_tys: Vec<Ty<'s>>,
        ret_ty: Ty<'s>,
    }
}