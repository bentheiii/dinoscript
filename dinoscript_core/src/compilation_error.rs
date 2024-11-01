use std::{borrow::Cow, error::Error, fmt::Display, sync::Arc};

use crate::{
    ast::pairable::{Pairable, WithPair},
    compilation_scope::{ty::Ty, NamedItem, NamedType},
};

#[derive(Debug, Clone)] // todo why is this clone?
pub enum CompilationError<'c, 's> {
    LetTypeMismatch {
        var: Cow<'c, str>,
        expected_ty: Arc<Ty<'s>>,
        actual_ty: Arc<Ty<'s>>,
    },
    TypeUsedAsValue {
        ty: NamedType<'s>,
    },
    ArgumentCountMismatch {
        func_name: Cow<'s, str>,
        min_n: usize,
        max_n: usize,
        actual_n: usize,
    },
    ArgumentTypeMismatch {
        func_name: Cow<'s, str>,
        param_n: usize,
        param_name: Option<Cow<'s, str>>,
        expected_ty: Arc<Ty<'s>>,
        actual_ty: Arc<Ty<'s>>,
    },
    NameNotFound {
        name: Cow<'c, str>,
    },
    NoOverloads {
        name: Cow<'s, str>,
        arg_types: Vec<Arc<Ty<'s>>>,
    },
    AmbiguousOverloads {
        name: Cow<'s, str>,
        arg_types: Vec<Arc<Ty<'s>>>,
    },
    NotCallable {
        ty: Arc<Ty<'s>>,
    },
    ArrayItemTypeMismatch {
        expected_ty: Arc<Ty<'s>>,
        actual_ty: Arc<Ty<'s>>,
    },
    ParameterWithoutDefaultAfterDefault {
        param_name: Cow<'s, str>,
        default_param_name: Cow<'s, str>,
    },
    FailedDefaultResolution {
        fn_name: Cow<'s, str>,
        param_n: usize,
        overload_name: Cow<'s, str>,
    },
    VariantTypeMismtach {
        union_name: Cow<'s, str>,
        variant_name: Cow<'s, str>,
        expected_ty: Arc<Ty<'s>>,
        actual_ty: Arc<Ty<'s>>,
    },
    ForwardTypeNotAllowed {
        name: Cow<'s, str>,
    },
    IllegalShadowing {
        name: Cow<'s, str>,
        existing: ShadowingItemKind,
        overrider: ShadowingItemKind,
    },
}

#[derive(Debug, Clone)]
pub enum ShadowingItemKind {
    Overload,
    Variable,
    Type
}

impl ShadowingItemKind{
    pub(crate) fn of(item: &NamedItem) -> Self {
        match item {
            NamedItem::Overloads(_) => ShadowingItemKind::Overload,
            NamedItem::Variable(_) => ShadowingItemKind::Variable,
            NamedItem::Type(_) => ShadowingItemKind::Type
        }
    }
}

impl Display for ShadowingItemKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ShadowingItemKind::Overload => write!(f, "function"),
            ShadowingItemKind::Variable => write!(f, "variable"),
            ShadowingItemKind::Type => write!(f, "type")
        }
    }
}

impl<'c, 's> Error for CompilationError<'c, 's> {}

impl Pairable<'_> for CompilationError<'_, '_> {}

pub type CompilationErrorWithPair<'c, 's> = WithPair<'c, CompilationError<'c, 's>>;

impl Display for CompilationError<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilationError::LetTypeMismatch {
                var,
                expected_ty,
                actual_ty,
            } => write!(
                f,
                "Type mismatch in let statement: variable {} expected {}, got {}",
                var, expected_ty, actual_ty
            ),
            CompilationError::TypeUsedAsValue { ty } => {
                write!(f, "Type {} cannot be used as a value", ty)
            }
            CompilationError::ArgumentCountMismatch {
                func_name: struct_name,
                min_n,
                max_n,
                actual_n,
            } => {
                if min_n == max_n {
                    write!(
                        f,
                        "Callable {} expects {} arguments, got {}",
                        struct_name, min_n, actual_n
                    )
                } else {
                    write!(
                        f,
                        "Callable {} expects between {} and {} arguments, got {}",
                        struct_name, min_n, max_n, actual_n
                    )
                }
            }
            CompilationError::ArgumentTypeMismatch {
                func_name,
                param_n,
                param_name,
                expected_ty,
                actual_ty,
            } => match param_name {
                None => write!(
                    f,
                    "Type mismatch in callable {}: param #{} expects {}, got {}",
                    func_name, param_n, expected_ty, actual_ty
                ),
                Some(param_name) => write!(
                    f,
                    "Type mismatch in callable {}: param {} ({}) expects {}, got {}",
                    func_name, param_n, param_name, expected_ty, actual_ty
                ),
            },
            CompilationError::NameNotFound { name } => write!(f, "Unknown name: {}", name),
            CompilationError::NoOverloads { name, arg_types } => write!(
                f,
                "No overloads found for callable {} with arguments {}",
                name,
                arg_types.iter().map(|ty| ty.to_string()).collect::<Vec<_>>().join(", ")
            ),
            CompilationError::AmbiguousOverloads { name, arg_types } => write!(
                f,
                "Ambiguous overloads found for callable {} with arguments {}",
                name,
                arg_types.iter().map(|ty| ty.to_string()).collect::<Vec<_>>().join(", ")
            ),
            CompilationError::NotCallable { ty } => write!(f, "Object of type {} is not callable", ty),
            CompilationError::ArrayItemTypeMismatch { expected_ty, actual_ty } => write!(
                f,
                "Type mismatch in array: expected item type {}, got {}",
                expected_ty, actual_ty
            ),
            CompilationError::ParameterWithoutDefaultAfterDefault {
                param_name,
                default_param_name,
            } => write!(
                f,
                "Parameter {} cannot be declared without a default value after parameter {}, that has a default value",
                param_name, default_param_name
            ),
            CompilationError::FailedDefaultResolution {
                fn_name,
                param_n,
                overload_name,
            } => write!(
                f,
                "Failed to resolve default value for parameter {} in function {} overload {}",
                param_n, fn_name, overload_name
            ),
            CompilationError::VariantTypeMismtach {
                union_name,
                variant_name,
                expected_ty,
                actual_ty,
            } => write!(
                f,
                "Type mismatch in union {}: expected type of variant {} to be {}, got {}",
                union_name, variant_name, expected_ty, actual_ty
            ),
            CompilationError::ForwardTypeNotAllowed { name } => write!(f, "Forward type ~{} is not allowed here", name),
            CompilationError::IllegalShadowing { name, existing, overrider } => write!(f, "Cannot shadow {} {} with a {}", existing, name, overrider)
        }
    }
}

impl Display for CompilationErrorWithPair<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (line, col) = self.pair.line_col();
        write!(
            f,
            "line {} col {}- \"{}\": {}",
            line,
            col,
            self.pair.as_str(),
            self.inner
        )
    }
}
