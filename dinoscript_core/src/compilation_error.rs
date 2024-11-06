use std::{borrow::Cow, fmt::Display, sync::Arc};
use thiserror::Error;

use crate::{
    ast::pairable::{Pairable, WithPair},
    compilation_scope::{ty::Ty, NamedItem, NamedType, RelativeNamedItem},
};

#[derive(Debug, Error)]
pub enum CompilationError<'c, 's> {
    #[error("Type mismatch in let statement: variable {var} expected {expected_ty}, got {actual_ty}")]
    LetTypeMismatch {
        var: Cow<'c, str>,
        expected_ty: Arc<Ty<'s>>,
        actual_ty: Arc<Ty<'s>>,
    },
    #[error("Type {ty} cannot be used as a value")]
    TypeUsedAsValue { ty: NamedType<'s> },
    #[error("Callable {func_name} expects {expected} arguments, got {actual_n}")]
    ArgumentCountMismatch {
        func_name: Cow<'s, str>,
        expected: ExpectedArgCount,
        actual_n: usize,
    },
    #[error("Type mismatch in callable {func_name}: param {param_id} expects {expected_ty}, got {actual_ty}")]
    ArgumentTypeMismatch {
        func_name: Cow<'s, str>,
        param_id: ParamIdentifier<'s>,
        expected_ty: Arc<Ty<'s>>,
        actual_ty: Arc<Ty<'s>>,
    },
    #[error("Unknown name: {name}")]
    NameNotFound { name: Cow<'c, str> },
    #[error("No overloads found for callable {name} with arguments {arg_types}")]
    NoOverloads {
        name: Cow<'s, str>,
        arg_types: TypeList<'s>,
    },
    #[error("Ambiguous overloads found for callable {name} with arguments {arg_types}")]
    AmbiguousOverloads {
        name: Cow<'s, str>,
        arg_types: TypeList<'s>,
    },
    #[error("Object of type {ty} is not callable")]
    NotCallable { ty: Arc<Ty<'s>> },
    #[error("Type mismatch in array: expected item type {expected_ty}, got {actual_ty}")]
    ArrayItemTypeMismatch {
        expected_ty: Arc<Ty<'s>>,
        actual_ty: Arc<Ty<'s>>,
    },
    #[error("Parameter {param_name} cannot be declared without a default value after parameter {default_param_name}, that has a default value")]
    ParameterWithoutDefaultAfterDefault {
        param_name: Cow<'s, str>,
        default_param_name: Cow<'s, str>,
    },
    #[error(
        "Failed to resolve default value for parameter {param_n} in function {func_name} overload {overload_name}"
    )]
    FailedDefaultResolution {
        func_name: Cow<'s, str>,
        param_n: usize,
        overload_name: Cow<'s, str>,
    },
    #[error("Type mismatch in union {union_name}: expected type of variant {variant_name} to be {expected_ty}, got {actual_ty}")]
    VariantTypeMismtach {
        union_name: Cow<'s, str>,
        variant_name: Cow<'s, str>,
        expected_ty: Arc<Ty<'s>>,
        actual_ty: Arc<Ty<'s>>,
    },
    #[error("cannot shadow {existing} {name} with a {overrider}")]
    IllegalShadowing {
        name: Cow<'s, str>,
        existing: ItemKind,
        overrider: ItemKind,
    },
    #[error("generic {template_name} expects {expected_n} arguments, got {actual_n}")]
    GenericArgCountMismatch {
        template_name: Cow<'s, str>,
        expected_n: usize,
        actual_n: usize,
    },
    #[error("Type {name} is not generic")]
    NonTemplateTypeInstantiation { name: Cow<'s, str> },
    #[error("{kind} {name} cannot be used as a type")]
    NonTypeUsedAsType { name: Cow<'s, str>, kind: ItemKind },
}

#[derive(Debug)]
pub enum ItemKind {
    Overload,
    Variable,
    Type,
}

pub(crate) trait HasItemKind {
    fn kind(&self) -> ItemKind;
}

impl HasItemKind for NamedItem<'_> {
    fn kind(&self) -> ItemKind {
        match self {
            NamedItem::Overloads(_) => ItemKind::Overload,
            NamedItem::Variable(_) => ItemKind::Variable,
            NamedItem::Type(_) => ItemKind::Type,
        }
    }
}

impl HasItemKind for RelativeNamedItem<'_, '_> {
    fn kind(&self) -> ItemKind {
        match self {
            RelativeNamedItem::Overloads(..) => ItemKind::Overload,
            RelativeNamedItem::Variable(..) => ItemKind::Variable,
            RelativeNamedItem::Type(..) => ItemKind::Type,
        }
    }
}

impl ItemKind {
    pub(crate) fn of(item: &impl HasItemKind) -> Self {
        item.kind()
    }
}

impl Display for ItemKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ItemKind::Overload => write!(f, "function"),
            ItemKind::Variable => write!(f, "variable"),
            ItemKind::Type => write!(f, "type"),
        }
    }
}

impl Pairable<'_> for CompilationError<'_, '_> {}

pub type CompilationErrorWithPair<'c, 's> = WithPair<'c, CompilationError<'c, 's>>;

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

#[derive(Debug)]
pub enum ExpectedArgCount {
    Exact(usize),
    Between(usize, usize),
}

impl ExpectedArgCount {
    pub fn from_range(min: usize, max: usize) -> Self {
        if min == max {
            ExpectedArgCount::Exact(min)
        } else {
            ExpectedArgCount::Between(min, max)
        }
    }
}

impl Display for ExpectedArgCount {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpectedArgCount::Exact(n) => write!(f, "{}", n),
            ExpectedArgCount::Between(min, max) => write!(f, "between {} and {}", min, max),
        }
    }
}

#[derive(Debug)]
pub struct ParamIdentifier<'s> {
    idx: usize,
    name: Option<Cow<'s, str>>,
}

impl<'s> ParamIdentifier<'s> {
    pub fn new(idx: usize, name: Option<Cow<'s, str>>) -> Self {
        Self { idx, name }
    }

    pub fn positional(idx: usize) -> Self {
        Self::new(idx, None)
    }
}

impl<'s> Display for ParamIdentifier<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.name {
            None => write!(f, "#{}", self.idx),
            Some(name) => write!(f, "{} ({})", name, self.idx),
        }
    }
}

#[derive(Debug)]
pub struct TypeList<'s>(pub Vec<Arc<Ty<'s>>>);

impl<'s> Display for TypeList<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for (i, ty) in self.0.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", ty)?;
        }
        write!(f, "]")
    }
}
