use std::{
    borrow::Cow, collections::{hash_map::Entry, HashMap}, fmt::Display, sync::Arc
};

use indexmap::IndexMap;
use ty::{CompoundKind, CompoundTemplate, Field, Fn, Generic, GenericSetId, Ty, TyTemplate};

use crate::{
    ast::{
        self, expression::{Attr, Call, Expr, ExprWithPair, Functor, MethodCall, Variant}, pairable::{Pairable, WithPair}, statement::{self, Compound, Let, Stmt, StmtWithPair}, ty::FnTy
    }, bytecode::{Command, MakeFunction, PushFromSource, SourceId}, compilation_error::{CompilationError, CompilationErrorWithPair}, maybe_owned::MaybeOwned, overloads::BindingResolution
};

pub mod ty {
    use std::{borrow::Cow, collections::HashMap, fmt::Display, num::NonZero, sync::Arc};
    use indexmap::IndexMap;

    use crate::unique;

    #[derive(Debug)]
    pub struct TemplateGenericSpecs{
        pub id: GenericSetId,
        pub n_generics: NonZero<usize>,
    }

    impl TemplateGenericSpecs {
        fn new(n_generics: usize) -> Self {
            Self {
                id: GenericSetId::unique(),
                n_generics: NonZero::new(n_generics).unwrap(),
            }
        }        
    }

    #[derive(Debug)]
    pub enum TyTemplate<'s> {
        Builtin(BuiltinTemplate),
        Compound(CompoundTemplate<'s>),
    }
    impl<'s> TyTemplate<'s> {
        pub fn instantiate(self: &Arc<Self>, args: Vec<Arc<Ty<'s>>>) -> Arc<Ty<'s>> {
            assert_eq!(
                args.len(),
                self.n_generics(),
            );
            Arc::new(Ty::Specialized(Specialized {
                template: self.clone(),
                args,
            }))
        }

        pub fn n_generics(&self) -> usize {
            match self {
                TyTemplate::Builtin(template) => template.generics.as_ref(),
                TyTemplate::Compound(template) => template.generics.as_ref(),
            }.map_or(0, |specs| specs.n_generics.get())
        }

        pub fn generic_id(&self) -> Option<GenericSetId> {
            match self {
                TyTemplate::Builtin(template) => template.generics.as_ref(),
                TyTemplate::Compound(template) => template.generics.as_ref(),
            }.map(|specs| specs.id)
        }

        pub fn name(&self) -> &Cow<'s, str> {
            match self {
                TyTemplate::Builtin(template) => &template.name,
                TyTemplate::Compound(template) => &template.name,
            }
        }
    }

    #[derive(Debug)]
    pub struct BuiltinTemplate {
        name: Cow<'static, str>,
        generics: Option<TemplateGenericSpecs>,
    }

    impl BuiltinTemplate {
        pub fn primitive(name: impl Into<Cow<'static, str>>) -> Self {
            Self {
                name: name.into(),
                generics: None,
            }
        }

        pub fn generic(name: impl Into<Cow<'static, str>>, n_generics: usize) -> Self {
            Self {
                name: name.into(),
                generics: Some(TemplateGenericSpecs::new(n_generics)),
            }
        }
    }

    #[derive(Debug)]
    pub struct CompoundTemplate<'s> {
        pub name: Cow<'s, str>,
        pub compound_kind: CompoundKind,
        pub generics: Option<TemplateGenericSpecs>,
        pub fields: IndexMap<Cow<'s, str>, Field<'s>>,
    }

    impl<'s> CompoundTemplate<'s> {
        pub fn new(
            name: impl Into<Cow<'s, str>>,
            compound_kind: CompoundKind,
            generics: Option<TemplateGenericSpecs>,
            fields: IndexMap<Cow<'s, str>, Field<'s>>,
        ) -> Self {
            Self {
                name: name.into(),
                compound_kind,
                generics,
                fields,
            }
        }
        
    }

    #[derive(Debug, Clone)]
    pub struct Field<'s> {
        pub ty: Arc<Ty<'s>>,
        pub idx: usize,
    }

    #[derive(Debug, Clone)]
    pub enum CompoundKind {
        Struct,
        Union,
    }

    impl From<crate::ast::statement::CompoundKind> for CompoundKind {
        fn from(kind: crate::ast::statement::CompoundKind) -> Self {
            match kind {
                crate::ast::statement::CompoundKind::Struct => Self::Struct,
                crate::ast::statement::CompoundKind::Union => Self::Union,
            }
        }
    }

    #[derive(Debug, Clone)]
    pub enum Ty<'s> {
        Specialized(Specialized<'s>),
        Tuple(Vec<Arc<Ty<'s>>>),
        Fn(Fn<'s>),
        // only applicable inside compound templates and generic functions
        Generic(Generic),
        // only applicable inside compound templates
        Tail,
    }

    impl<'s> Ty<'s> {
        pub fn resolve(self: &Arc<Self>, tail: &Arc<Self>, generic_args: &Vec<Arc<Self>>, gen_id: Option<GenericSetId>) -> Arc<Self> {
            match self.as_ref() {
                Ty::Specialized(specialized) => Arc::new(Ty::Specialized(Specialized {
                    template: specialized.template.clone(),
                    args: specialized
                        .args
                        .iter()
                        .map(|ty| ty.resolve(tail, generic_args, gen_id))
                        .collect(),
                })),
                Ty::Tuple(tys) => Arc::new(Ty::Tuple(tys.iter().map(|ty| ty.resolve(tail, generic_args, gen_id)).collect())),
                Ty::Fn(fn_ty) => Arc::new(Ty::Fn(Fn::new(
                    fn_ty.args.iter().map(|arg| arg.resolve(tail, generic_args, gen_id)).collect(),
                    fn_ty.return_ty.resolve(tail, generic_args, gen_id),
                ))),
                Ty::Generic(gen) if gen_id.is_some_and(|gid| gen.gen_id == gid) => generic_args[gen.idx].clone(),
                Ty::Generic(_) => self.clone(),
                Ty::Tail => tail.clone(),
            }
        }
    }

    impl<'s> Display for Ty<'s>{
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Ty::Specialized(specialized) => {
                    if specialized.args.is_empty(){
                        write!(f, "{}", specialized.template.name())
                    } else {
                        write!(f, "{}<{}>", specialized.template.name(), specialized.args.iter().map(|ty| ty.to_string()).collect::<Vec<_>>().join(", "))
                    }
                },
                Ty::Tuple(tys) => {
                    write!(f, "({})", tys.iter().map(|ty| ty.to_string()).collect::<Vec<_>>().join(", "))
                },
                Ty::Fn(fn_ty) => {
                    write!(f, "({}) -> {}", fn_ty.args.iter().map(|arg| arg.to_string()).collect::<Vec<_>>().join(", "), fn_ty.return_ty)
                },
                Ty::Generic(gen) => {
                    write!(f, "T_{}", gen.idx)
                },
                Ty::Tail => {
                    write!(f, "...")
                }
            }
        }
    }

    unique!(pub struct GenericSetId);

    #[derive(Debug, Clone)]
    pub struct Generic{
        pub idx: usize,
        pub gen_id: GenericSetId,
    }

    impl Generic {
        pub fn new(idx: usize, gen_id: GenericSetId) -> Self {
            Self { idx, gen_id }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Fn<'s> {
        pub args: Vec<Arc<Ty<'s>>>,
        pub return_ty: Arc<Ty<'s>>,
    }

    impl<'s> Fn<'s> {
        pub fn new(args: Vec<Arc<Ty<'s>>>, return_ty: Arc<Ty<'s>>) -> Self {
            Self { args, return_ty }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Specialized<'s> {
        pub template: Arc<TyTemplate<'s>>,
        pub args: Vec<Arc<Ty<'s>>>,
    }

    impl<'s> Specialized<'s> {
        pub fn get_field(&self, name: &Cow<'s, str>, tail: &Arc<Ty<'s>>) -> Option<Field<'s>> {
            let raw = match self.template.as_ref() {
                TyTemplate::Compound(template) => template.fields.get(name).cloned(),
                _ => None,
            };
            raw.map(|field| Field {
                ty: field.ty.resolve(tail, &self.args, self.template.generic_id()),
                idx: field.idx,
            })
        }
    }
}

#[derive(Debug, Clone)]
pub enum NamedItem<'s> {
    Overloads(Overloads<'s>),
    Variable(Variable<'s>),
    Type(NamedType<'s>),
    Tail,
}

#[derive(Debug, Clone)]
pub struct Variable<'s> {
    ty: Arc<Ty<'s>>,
    cell_idx: usize,
}

#[derive(Debug, Clone)]
pub enum NamedType<'s> {
    Template(Arc<TyTemplate<'s>>),
    Concrete(Arc<Ty<'s>>), // will always be a generic parameter
}

impl Display for NamedType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NamedType::Template(template) => write!(f, "{}", template.name()),
            NamedType::Concrete(ty) => write!(f, "{}", ty),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Overloads<'s> {
    pub overloads: Vec<Overload<'s>>,
}

impl<'s> Overloads<'s> {
    fn extend(&mut self, other: &Overloads<'s>) {
        self.overloads.extend(other.overloads.iter().cloned());
    }
}

#[derive(Debug, Clone)]
pub struct Overload<'s> {
    pub generic_params: Vec<Cow<'s, str>>,
    pub args: Vec<OverloadArg<'s>>,
    pub return_ty: Arc<Ty<'s>>,
    pub loc: OverloadLoc,
}

impl<'s> Overload<'s> {
    fn get_type(&self) -> Arc<Ty<'s>> {
        Arc::new(Ty::Fn(Fn::new(
            self.args.iter().map(|arg| arg.ty.clone()).collect(),
            self.return_ty.clone(),
        )))
    }
}

#[derive(Debug, Clone)]
pub enum OverloadLoc {
    Cell(usize),
    // this will only be in the root scope
    System(SystemLoc),
}

#[derive(Debug, Clone)]
pub struct SystemLoc {
    pub source: SourceId,
    pub id: usize,
}

impl SystemLoc {
    pub fn new(source: SourceId, id: usize) -> Self {
        Self { source, id }
    }
}

#[derive(Debug, Clone)]
pub struct OverloadArg<'s> {
    pub ty: Arc<Ty<'s>>,
    pub default: Option<OverloadArgDefault<'s>>,
}

#[derive(Debug, Clone)]
pub enum OverloadArgDefault<'s> {
    Expr(Expr<'s>),
    OverloadResolve(OverloadResolve<'s>),
}

#[derive(Debug, Clone)]
struct OverloadResolve<'s> {
    name: Cow<'s, str>,
    args: Vec<Ty<'s>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct PendingCapture {
    ancestor_height: usize,
    cell_idx: usize,
}

#[derive(Debug, Clone)]
enum RelativeNamedItem<'p, 's> {
    Variable(RelativeNamedItemVariable<'s>),
    Type(&'p NamedType<'s>),
    Overloads(RelativeNamedItemOverloads<'p, 's>),
    Tail(usize),
}

impl<'p, 's> RelativeNamedItem<'p, 's> {
    fn add_height(&self, height: usize) -> Self {
        match self {
            RelativeNamedItem::Variable(variable) => RelativeNamedItem::Variable(RelativeNamedItemVariable {
                loc: variable.loc.add_height(height),
                ty: variable.ty.clone(),
            }),
            RelativeNamedItem::Type(ty) => RelativeNamedItem::Type(ty),
            RelativeNamedItem::Overloads(overloads) => RelativeNamedItem::Overloads(RelativeNamedItemOverloads {
                overloads: overloads
                    .overloads
                    .iter()
                    .map(|overload| RelativeOverload {
                        overload: overload.overload,
                        loc: overload.loc.add_height(height),
                    })
                    .collect(),
            }),
            RelativeNamedItem::Tail(idx) => RelativeNamedItem::Tail(*idx + height),
        }
    }
}

#[derive(Debug, Clone)]
enum RelativeLocation {
    Local(usize),
    Capture(RelativeLocationCapture),
    Global(usize),
    System(SystemLoc),
}

impl RelativeLocation {
    fn from_cell_idx<'p, 's, B>(scope: &CompilationScope<'s, 'p, B>, cell_idx: usize) -> Self {
        if scope.is_root() {
            RelativeLocation::Global(cell_idx)
        } else {
            RelativeLocation::Local(cell_idx)
        }
    }

    fn add_height(&self, height: usize) -> Self {
        match self {
            RelativeLocation::Local(cell_idx) => RelativeLocation::Capture(RelativeLocationCapture {
                ancestor_height: height,
                cell_idx: *cell_idx,
            }),
            RelativeLocation::Capture(capture) => RelativeLocation::Capture(RelativeLocationCapture {
                ancestor_height: capture.ancestor_height + height,
                cell_idx: capture.cell_idx,
            }),
            RelativeLocation::Global(cell_idx) => RelativeLocation::Global(*cell_idx),
            RelativeLocation::System(system_loc) => RelativeLocation::System(system_loc.clone()),
        }
    }
}

#[derive(Debug, Clone)]
struct RelativeLocationCapture {
    ancestor_height: usize, // 0 is the current scope, therefore this will always be positive
    cell_idx: usize,
}

#[derive(Debug, Clone)]
struct RelativeNamedItemVariable<'s> {
    loc: RelativeLocation,
    ty: Arc<Ty<'s>>,
}

#[derive(Debug, Clone)]
struct RelativeNamedItemType<'s> {
    // Todo i don't think we need this
    loc: RelativeLocation,
    ty: Arc<Ty<'s>>,
}

#[derive(Debug, Clone)]
struct RelativeNamedItemOverloads<'p, 's> {
    overloads: Vec<RelativeOverload<'p, 's>>,
}

#[derive(Debug, Clone)]
struct RelativeOverload<'p, 's> {
    overload: &'p Overload<'s>,
    loc: RelativeLocation,
}

pub trait Builtins<'s> {
    fn int(&self) -> Arc<Ty<'s>>;
    fn float(&self) -> Arc<Ty<'s>>;
    fn bool(&self) -> Arc<Ty<'s>>;
    fn str(&self) -> Arc<Ty<'s>>;
}

pub struct CompilationScope<'p, 's, B> {
    id: GenericSetId, // the scope id, used to differentiate between generic params of scopes
    parent: Option<&'p Self>,
    pub builtins: Option<MaybeOwned<'p, B>>,
    pub names: HashMap<Cow<'s, str>, NamedItem<'s>>,
    pub n_cells: usize,
    pending_captures: Vec<PendingCapture>,
}

impl<'p, 's, B> CompilationScope<'p, 's, B> {
    fn is_root(&self) -> bool {
        self.parent.is_none()
    }

    pub fn add_overload(&mut self, name: Cow<'s, str>, overload: Overload<'s>) {
        let existing_name = self.names.entry(name);
        match existing_name {
            Entry::Occupied(mut entry) => match entry.get_mut() {
                NamedItem::Overloads(overloads) => overloads.overloads.push(overload),
                _ => todo!(),
            },
            Entry::Vacant(entry) => {
                entry.insert(NamedItem::Overloads(Overloads {
                    overloads: vec![overload],
                }));
            }
        }
    }
}

impl<'p, 's, B: Builtins<'s>> CompilationScope<'p, 's, B> {
    fn get_cell_idx(&mut self) -> usize {
        let idx = self.n_cells;
        self.n_cells += 1;
        idx
    }

    pub fn root() -> Self {
        Self {
            id: GenericSetId::unique(),
            parent: None,
            builtins: None,
            names: HashMap::new(),
            n_cells: 0,
            pending_captures: Vec::new(),
        }
    }

    fn child(&'p self) -> Self {
        Self {
            id: GenericSetId::unique(),
            parent: Some(self),
            builtins: self.builtins.as_ref().map(|b| b.borrowed()),
            names: HashMap::new(),
            n_cells: 0,
            pending_captures: Vec::new(),
        }
    }

    fn get_capture(&mut self, capture: PendingCapture) -> usize {
        if let Some(idx) = self.pending_captures.iter().position(|c| c == &capture) {
            idx
        } else {
            let idx = self.pending_captures.len();
            self.pending_captures.push(capture);
            idx
        }
    }

    fn gen_prim_type(&self, name: &'static str, args: Vec<Arc<Ty<'s>>>) -> Arc<Ty<'s>> {
        // todo: in the future, source objects should have caches
        let item = self.get_named_item(&Cow::Borrowed(name)).unwrap();
        let RelativeNamedItem::Type(NamedType::Template(template)) = item else {
            unreachable!()
        };
        template.instantiate(args)
    }

    fn int(&self) -> Arc<Ty<'s>> {
        self.builtins.as_ref().unwrap().int()
    }

    fn float(&self) -> Arc<Ty<'s>> {
        self.builtins.as_ref().unwrap().float()
    }

    fn bool(&self) -> Arc<Ty<'s>> {
        self.builtins.as_ref().unwrap().bool()
    }

    fn str(&self) -> Arc<Ty<'s>> {
        self.builtins.as_ref().unwrap().str()
    }

    fn optional(&self, ty: Arc<Ty<'s>>) -> Arc<Ty<'s>> {
        self.gen_prim_type("Optional", vec![ty])
    }

    fn parse_type<'c: 's>(&self, ty: &ast::ty::Ty<'c>) -> Result<Arc<Ty<'s>>, CompilationErrorWithPair<'c, 's>> {
        match ty {
            ast::ty::Ty::Ref(name) => {
                let item = self.get_named_item(name).unwrap();
                match item {
                    RelativeNamedItem::Type(NamedType::Concrete(ty)) => Ok(ty.clone()),
                    RelativeNamedItem::Type(NamedType::Template(template)) if template.n_generics() == 0 => {
                        Ok(template.instantiate(vec![]))
                    }
                    RelativeNamedItem::Type(NamedType::Template(template)) => todo!(), // raise an error
                    _ => todo!(),                                                      // raise an error
                }
            }
            ast::ty::Ty::Tuple(inners) => {
                let inners = inners
                    .iter()
                    .map(|inner| self.parse_type(inner))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Arc::new(Ty::Tuple(inners)))
            }
            ast::ty::Ty::Fn(ast::ty::FnTy { args, ret }) => {
                let args = args
                    .iter()
                    .map(|arg| self.parse_type(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                let return_ty = self.parse_type(&ret)?;
                Ok(Arc::new(Ty::Fn(Fn::new(args, return_ty))))
            }
            ast::ty::Ty::Specialized(ast::ty::SpecializedTy { name, args }) => {
                let item = self.get_named_item(name).unwrap();
                match item {
                    RelativeNamedItem::Type(NamedType::Template(template)) => {
                        let args = args
                            .iter()
                            .map(|arg| self.parse_type(arg))
                            .collect::<Result<Vec<_>, _>>()?;
                        // todo check that the number of args match
                        Ok(template.instantiate(args))
                    }
                    _ => todo!(), // raise an error
                }
            }
        }
    }

    fn get_named_item(&'p self, name: &Cow<'s, str>) -> Option<RelativeNamedItem<'p, 's>> {
        match &self.names.get(name) {
            Some(NamedItem::Overloads(overloads)) => {
                let mut rel_overloads = RelativeNamedItemOverloads {
                    overloads: overloads
                        .overloads
                        .iter()
                        .map(|overload| RelativeOverload {
                            overload,
                            loc: match &overload.loc {
                                OverloadLoc::Cell(cell_idx) if self.is_root() => RelativeLocation::Global(*cell_idx),
                                OverloadLoc::Cell(cell_idx) => RelativeLocation::Local(*cell_idx),
                                OverloadLoc::System(system_loc) => RelativeLocation::System(system_loc.clone()),
                            },
                        })
                        .collect(),
                };

                let parent_overloads = self
                    .parent
                    .and_then(|parent| parent.get_named_item(name))
                    .map(|n| n.add_height(1));
                // invariant, if parent_overloads is Some, then it is an Overloads
                if let Some(RelativeNamedItem::Overloads(parent_overloads)) = parent_overloads {
                    rel_overloads.overloads.extend(parent_overloads.overloads.into_iter());
                }
                Some(RelativeNamedItem::Overloads(rel_overloads))
            }
            Some(NamedItem::Variable(variable)) => {
                let loc = match variable.cell_idx {
                    cell_idx if self.is_root() => RelativeLocation::Global(cell_idx),
                    cell_idx => RelativeLocation::Local(cell_idx),
                };
                Some(RelativeNamedItem::Variable(RelativeNamedItemVariable {
                    loc,
                    ty: variable.ty.clone(),
                }))
            }
            Some(NamedItem::Type(ty)) => Some(RelativeNamedItem::Type(ty)),
            Some(NamedItem::Tail) => Some(RelativeNamedItem::Tail(0)),
            None => match self.parent {
                // todo increment the parent's cell indices
                Some(parent) => parent.get_named_item(name),
                None => None,
            },
        }
    }

    fn set_generics(&mut self, names: impl IntoIterator<Item = Cow<'s, str>>) {
        for (i, name) in names.into_iter().enumerate() {
            self.names
                .insert(name, NamedItem::Type(NamedType::Concrete(Arc::new(Ty::Generic(Generic::new(i, self.id))))));
        }
    }

    fn set_tail(&mut self, name: Cow<'s, str>) {
        self.names.insert(name, NamedItem::Tail);
    }

    fn feed_params<'c:'s>(
        &mut self,
        params: impl IntoIterator<Item = (Cow<'c, str>, Arc<Ty<'s>>)>,
        sink: &mut Vec<Command<'c>>,
    ) {
        // we expect the stack to be in the following state when the function is called:
        // [argN, ..., arg0]
        // where arg0 is the first argument
        for (name, ty) in params {
            let variable = Variable {
                ty,
                cell_idx: self.get_cell_idx(),
            };
            sink.push(Command::PopToCell(variable.cell_idx));
            self.names.insert(name, NamedItem::Variable(variable));
        }
    }

    fn feed_call<'a, 'c: 's>(
        &mut self,
        functor: &Functor<'c>,
        args: impl IntoIterator<Item = &'a ExprWithPair<'c>>,
        sink: &mut Vec<Command<'c>>,
    ) -> Result<Arc<Ty<'s>>, CompilationErrorWithPair<'c, 's>>
    where
        's: 'a,
    {
        match functor {
            Functor::Expr(expr) => {
                // todo consider case where expr is a type
                // todo consider case where expr is a variant
                // todo consider case where expr is an overload that needs to be resolved

                // note that the arguments must be sunk into dummy lists first, since we might need to also put in default values
                let mut arg_types = Vec::new();
                let mut arg_sinks = Vec::new();
                for arg in args {
                    let mut arg_sink = Vec::new();
                    let ty = self.feed_expression(&arg, &mut arg_sink)?;
                    arg_types.push(ty);
                    arg_sinks.push(arg_sink);
                }

                for arg_sink in arg_sinks.into_iter().rev() {
                    sink.extend(arg_sink);
                }

                if let Expr::Ref(name) = &expr.as_ref().inner {
                    let named_item = self.get_named_item(name);
                    if let Some(RelativeNamedItem::Overloads(RelativeNamedItemOverloads {overloads})) = named_item{
                        // TODO: do we want to process this flow if there's only one overload? For now it's fine
                        let mut resolved_overloads = Vec::new();
                        'outer: for rel_overload in overloads {
                            if arg_types.len() != rel_overload.overload.args.len() {
                                continue;
                            }
                            let mut resolution = BindingResolution::new(None, 0);
                            for (param, arg_ty) in rel_overload.overload.args.iter().zip(arg_types.iter()) {
                                if let Err(_) = resolution.assign(&param.ty, arg_ty){
                                    // todo report the error?
                                    continue 'outer;
                                }
                            }
                            resolved_overloads.push(rel_overload);
                        }
                        if resolved_overloads.len() != 1 {
                            todo!("no single overload for {:?} ({:?})", name, arg_types);
                        }
                        let resolved_overload = resolved_overloads.into_iter().next().unwrap();
                        let fn_ty = resolved_overload.overload.get_type();
                        self.feed_relative_location(&resolved_overload.loc, sink);
                        sink.push(Command::MakePending(arg_types.len()));
                        return match fn_ty.as_ref() {
                            Ty::Fn(fn_ty) => {
                                // todo we should also consider the case where the function is generic
                                Ok(fn_ty.return_ty.clone())
                            }
                            _ => todo!(), // raise an error
                        }
                    }
                    if let Some(RelativeNamedItem::Type(NamedType::Template(template_arc))) = named_item {
                        if let TyTemplate::Compound(CompoundTemplate { compound_kind: CompoundKind::Struct, generics, fields , name: struct_name}) = template_arc.as_ref(){
                            if fields.len() != arg_types.len() {
                                return Err(CompilationError::StructInstantiationFieldsMismatch { struct_name: struct_name.clone(), expected_num_fields: fields.len(), actual_num_fields: arg_types.len()}.with_pair(expr.pair.clone()));
                            }
                            let mut resolution = BindingResolution::new(generics.as_ref().map(|g| g.id), generics.as_ref().map(|g| g.n_generics.get()).unwrap_or_default());
                            for (i, ((name, field), arg_ty)) in fields.iter().zip(arg_types.iter()).enumerate() {
                                if let Err(_) = resolution.assign(&field.ty, arg_ty){
                                    return Err(CompilationError::ArgumentTypeMismatch { func_name: struct_name.clone(), param_n: i, param_name: Some(name.clone()), expected_ty: field.ty.clone(), actual_ty: arg_ty.clone()}.with_pair(expr.pair.clone()));
                                }
                            }
                            let resolved_ty = template_arc.instantiate(resolution.bound_generics.into_iter().map(|ty| ty.unwrap()).collect());
                            sink.push(Command::Struct(fields.len()));
                            return Ok(resolved_ty);
                        }
                    }
                }

                let ty = self.feed_expression(expr.as_ref(), sink)?;

                // todo check that the args match
                sink.push(Command::MakePending(arg_types.len()));

                match ty.as_ref() {
                    Ty::Fn(fn_ty) => {
                        // todo we should also consider the case where the function is generic
                        Ok(fn_ty.return_ty.clone())
                    }
                    _ => todo!(), // raise an error
                }
            }
            Functor::Operator(op) => self.feed_call(
                &Functor::Expr(Box::new(Expr::Ref(Cow::Borrowed(op.inner.func_name())).with_pair(op.pair.clone()))),
                args,
                sink,
            ),
            Functor::Specialized(..) => todo!(),
        }
    }

    fn feed_relative_location<'c: 's>(&mut self, loc: &RelativeLocation, sink: &mut Vec<Command<'c>>) {
        match loc {
            RelativeLocation::Local(cell_idx) => {
                sink.push(Command::PushFromCell(*cell_idx));
            }
            RelativeLocation::Capture(capture) => {
                sink.push(Command::PushFromCapture(self.get_capture(PendingCapture {
                    ancestor_height: capture.ancestor_height,
                    cell_idx: capture.cell_idx,
                })));
            }
            RelativeLocation::Global(cell_idx) => {
                sink.push(Command::PushGlobal(*cell_idx));
            }
            RelativeLocation::System(system_loc) => {
                sink.push(Command::PushFromSource(PushFromSource {
                    source: system_loc.source,
                    id: system_loc.id,
                }));
            }
        }
    }

    fn feed_expression<'c: 's>(&mut self, expr: &ExprWithPair<'c>, sink: &mut Vec<Command<'c>>) -> Result<Arc<Ty<'s>>, CompilationErrorWithPair<'c, 's>> {
        // put commands in the sink to ensure that the (possibly lazy) expression is at the top of the stack
        match &expr.inner {
            Expr::LitInt(value) => {
                sink.push(Command::PushInt(*value));
                Ok(self.int())
            }
            Expr::LitFloat(value) => {
                sink.push(Command::PushFloat(*value));
                Ok(self.float())
            }
            Expr::LitBool(value) => {
                sink.push(Command::PushBool(*value));
                Ok(self.bool())
            }
            Expr::LitString(value) => {
                sink.push(Command::PushString(value.clone()));
                Ok(self.str())
            }
            Expr::Ref(name) => {
                match self.get_named_item(name) {
                    Some(RelativeNamedItem::Variable(variable)) => {
                        match &variable.loc {
                            RelativeLocation::Local(cell_idx) => {
                                sink.push(Command::PushFromCell(*cell_idx));
                            }
                            RelativeLocation::Capture(capture) => {
                                sink.push(Command::PushFromCapture(self.get_capture(PendingCapture {
                                    ancestor_height: capture.ancestor_height,
                                    cell_idx: capture.cell_idx,
                                })));
                            }
                            RelativeLocation::Global(cell_idx) => {
                                sink.push(Command::PushGlobal(*cell_idx));
                            }
                            RelativeLocation::System(system_loc) => {
                                sink.push(Command::PushFromSource(PushFromSource {
                                    source: system_loc.source,
                                    id: system_loc.id,
                                }));
                            }
                        }
                        Ok(variable.ty.clone())
                    }
                    Some(RelativeNamedItem::Overloads(overloads)) => {
                        if overloads.overloads.len() != 1 {
                            todo!() // raise an error
                        }
                        let overload = overloads.overloads.into_iter().next().unwrap();
                        let ty = overload.overload.get_type();
                        self.feed_relative_location(&overload.loc, sink);
                        Ok(ty)
                    }
                    Some(RelativeNamedItem::Type(nt)) => {
                        Err(CompilationError::TypeUsedAsValue { ty:  nt.clone()}.with_pair(expr.pair.clone()))
                    }
                    Some(RelativeNamedItem::Tail(idx)) => {
                        sink.push(Command::PushTail(idx));
                        Ok(Arc::new(Ty::Tail))
                    }
                    None => {
                        dbg!(name);
                        todo!() // raise an error
                    }
                }
            }
            Expr::Disambiguation(..) => {
                todo!()
            }
            Expr::Attr(Attr { obj, name }) => {
                let obj_type = self.feed_expression(obj, sink)?;
                match obj_type.as_ref() {
                    Ty::Specialized(specialized) => {
                        let Some(field) = specialized.get_field(&name, &obj_type) else {
                            todo!()
                        }; // raise an error
                        sink.push(Command::Attr(field.idx));
                        Ok(field.ty)
                    }
                    Ty::Tuple(tup) => {
                        let Some(idx) = name.strip_prefix("item").and_then(|idx| idx.parse().ok()) else {
                            todo!()
                        }; // raise an error
                        if idx >= tup.len() {
                            todo!() // raise an error
                        }
                        sink.push(Command::Attr(idx));
                        Ok(tup[idx].clone())
                    }
                    _ => todo!(), // raise an error
                }
            }
            Expr::Tuple(items) => {
                let tys = items
                    .into_iter()
                    .rev()
                    .map(|item| self.feed_expression(item, sink))
                    .collect::<Result<Vec<_>, _>>()?;
                sink.push(Command::Struct(tys.len()));
                Ok(Arc::new(Ty::Tuple(tys)))
            }
            Expr::Variant(Variant { obj, name }) => {
                let obj_type = self.feed_expression(obj, sink)?;
                match obj_type.as_ref() {
                    Ty::Specialized(specialized) => {
                        let Some(field) = specialized.get_field(&name, &obj_type) else {
                            todo!()
                        }; // raise an error
                        sink.push(Command::Variant(field.idx));
                        Ok(field.ty)
                    }
                    _ => todo!(), // raise an error
                }
            }
            Expr::VariantOpt(Variant { obj, name }) => {
                let obj_type = self.feed_expression(obj, sink)?;
                match obj_type.as_ref() {
                    Ty::Specialized(specialized) => {
                        let Some(field) = specialized.get_field(&name, &obj_type) else {
                            todo!()
                        }; // raise an error
                        sink.push(Command::VariantOpt(field.idx));
                        Ok(self.optional(field.ty))
                    }
                    _ => todo!(), // raise an error
                }
            }
            Expr::Call(Call { functor, args }) => self.feed_call(functor, args, sink),
            Expr::MethodCall(MethodCall { obj, name, args }) => {
                let args = std::iter::once(obj.as_ref()).chain(args.iter());
                self.feed_call(&Functor::Expr(Box::new(Expr::Ref(name.clone()).with_pair(expr.pair.clone()))), args, sink)
            }

            _ => todo!(),
        }
    }

    fn feed_return<'c: 's>(&mut self, expr: &ExprWithPair<'c>, sink: &mut Vec<Command<'c>>) -> Result<Arc<Ty<'s>>, CompilationErrorWithPair<'c, 's>> {
        let ret = self.feed_expression(expr, sink)?;
        sink.push(Command::EvalTop);
        Ok(ret)
    }

    pub fn feed_statement<'c: 's>(&mut self, stmt: &StmtWithPair<'c>, sink: &mut Vec<Command<'c>>) -> Result<(), CompilationErrorWithPair<'c, 's>> {
        match &stmt.inner {
            Stmt::Let(Let { var, ty, expr }) => {
                let actual_ty = self.feed_expression(&expr, sink)?;
                let declared_ty = if let Some(explicit_ty) = ty {
                    // we should check that actual_ty is a subtype of ty
                    let mut assignment = BindingResolution::primitive();
                    let ty = self.parse_type(explicit_ty)?;
                    if let Err(_) = assignment.assign(&ty, &actual_ty) {
                        return Err(CompilationError::LetTypeMismatch { var: var.clone(), expected_ty: ty, actual_ty }.with_pair(stmt.pair.clone()));
                    }
                    ty
                } else {
                    actual_ty.clone()
                };
                let cell_idx = self.get_cell_idx();
                sink.push(Command::PopToCell(cell_idx));
                // todo check that the variable is not already defined/can be shadowed
                self.names.insert(
                    var.clone(),
                    NamedItem::Variable(Variable {
                        ty: declared_ty,
                        cell_idx,
                    }),
                );
                Ok(())
            }
            Stmt::Fn(statement::Fn {
                name,
                generic_params,
                args,
                return_ty,
                body,
                ret,
            }) => {
                let mut subscope_sink = Vec::new();
                let cell_idx = self.get_cell_idx();
                let mut subscope = self.child();
                let args = args
                    .into_iter()
                    .map(|fn_arg| -> Result<_, _> {
                        Ok((
                            fn_arg.name.clone(),
                            subscope.parse_type(&fn_arg.ty)?,
                            fn_arg.default.clone(),
                        ))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let expected_return_ty = subscope.parse_type(return_ty)?;
                subscope.set_generics(generic_params.iter().cloned());
                subscope.set_tail(name.clone());
                subscope.feed_params(
                    args.iter().map(|(name, ty, _)| (name.clone(), ty.clone())),
                    &mut subscope_sink,
                );
                for body_stmt in body {
                    subscope.feed_statement(body_stmt, &mut subscope_sink)?;
                }
                let actual_return_ty = subscope.feed_return(&ret, &mut subscope_sink)?;
                // todo check that return_tys match
                // todo populate the pending captures
                sink.push(Command::MakeFunction(MakeFunction {
                    n_captures: subscope.pending_captures.len(),
                    n_cells: subscope.n_cells,
                    commands: subscope_sink,
                }));
                // todo check that we don't shadow a variable

                let new_overload = Overload {
                    generic_params: generic_params.clone(),
                    args: args
                        .into_iter()
                        .map(|(_name, ty, default)| OverloadArg {
                            ty: ty,
                            default: default.map(|_| todo!()),
                        })
                        .collect(),
                    return_ty: expected_return_ty,
                    loc: OverloadLoc::Cell(cell_idx),
                };
                self.add_overload(name.clone(), new_overload);
                sink.push(Command::PopToCell(cell_idx));
                Ok(())
            }
            Stmt::Compound(compound) => {
                let generics = if compound.generic_params.is_empty() {
                    None
                } else {
                    todo!()
                };
                let fields = compound
                    .fields
                    .iter()
                    .map(|field| -> Result<_, _> {
                        Ok((
                            // todo check for duplicates
                            field.name.clone(),
                            Field {
                                ty: self.parse_type(&field.ty)?,
                                idx: self.get_cell_idx(),
                            },
                        ))
                    })
                    .collect::<Result<IndexMap<_, _>, _>>()?;
                let template = TyTemplate::Compound(
                    CompoundTemplate::new(compound.name.clone(), compound.kind.into(), generics, fields)
                );
                self.names.insert(
                    compound.name.clone(),
                    NamedItem::Type(NamedType::Template(Arc::new(template))),
                );
                Ok(())
            }
            _ => todo!(),
        }
    }
}
