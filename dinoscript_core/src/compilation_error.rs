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
    StructInstantiationFieldsMismatch {
        struct_name: Cow<'s, str>,
        expected_num_fields: usize,
        actual_num_fields: usize,
    },
    ArgumentTypeMismatch {
        func_name: Cow<'s, str>,
        param_n: usize,
        param_name: Option<Cow<'s, str>>,
        expected_ty: Arc<Ty<'s>>,
        actual_ty: Arc<Ty<'s>>,
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
            CompilationError::StructInstantiationFieldsMismatch {
                struct_name,
                expected_num_fields,
                actual_num_fields,
            } => write!(
                f,
                "Struct {} expects {} fields, got {}",
                struct_name, expected_num_fields, actual_num_fields
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
            }
        }
    }
}

impl Display for CompilationErrorWithPair<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (line, col) = self.pair.line_col();
        write!(f, "line {} col {}- \"{}\": {}", line, col, self.pair.as_str(), self.inner)
    }
}