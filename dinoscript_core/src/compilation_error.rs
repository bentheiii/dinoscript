use std::{borrow::Cow, error::Error, fmt::Display, sync::Arc};

use crate::{ast::pairable::{Pairable, WithPair}, compilation_scope::{ty::Ty, NamedType}};

#[derive(Debug)]
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
        expected_n: usize,
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
            },
            CompilationError::ArgumentCountMismatch {
                func_name: struct_name,
                expected_n,
                actual_n,
            } => write!(
                f,
                "Callable {} expects {} arguments, got {}",
                struct_name, expected_n, actual_n
            ),
            CompilationError::ArgumentTypeMismatch {
                func_name,
                param_n,
                param_name,
                expected_ty,
                actual_ty,
            } => match param_name{
                    None => write!(
                    f,
                    "Type mismatch in callable {}: param #{} expects {}, got {}",
                    func_name, param_n, expected_ty, actual_ty
                ), Some(param_name) => write!(
                    f,
                    "Type mismatch in callable {}: param {} ({}) expects {}, got {}",
                    func_name, param_n, param_name, expected_ty, actual_ty
                )
            },
            CompilationError::NameNotFound { name } => write!(f, "Unknown name: {}", name),
            CompilationError::NoOverloads { name, arg_types } => write!(
                f,
                "No overloads found for callable {} with arguments {}",
                name,
                arg_types
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            CompilationError::AmbiguousOverloads { name, arg_types } => write!(
                f,
                "Ambiguous overloads found for callable {} with arguments {}",
                name,
                arg_types
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl Display for CompilationErrorWithPair<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (line, col) = self.pair.line_col();
        write!(f, "line {} col {}- \"{}\": {}", line, col, self.pair.as_str(), self.inner)
    }
}