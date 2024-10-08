use std::{
    borrow::Cow, collections::{hash_map::Entry, HashMap}, fmt::Display, iter::repeat, sync::Arc, vec
};

use indexmap::IndexMap;
use ty::{CompoundKind, CompoundTemplate, Field, Fn, Generic, GenericSetId, TemplateGenericSpecs, Ty, TyTemplate};

use crate::{
    ast::{
        self, expression::{Attr, Call, Disambiguation, Expr, ExprWithPair, Functor, Lookup, MethodCall, Variant}, pairable::Pairable, statement::{self, FnArgDefault, Let, Stmt, StmtWithPair}
    }, bytecode::{Command, MakeFunction, PushFromSource, SourceId}, compilation_error::{CompilationError, CompilationErrorWithPair}, maybe_owned::MaybeOwned, overloads::{combine_types, BindingResolution}
};

pub mod ty {
    use std::{borrow::Cow, fmt::Display, num::NonZero, sync::{Arc, LazyLock}};
    use indexmap::IndexMap;

    use crate::unique;

    #[derive(Debug)]
    pub struct TemplateGenericSpecs{
        pub id: GenericSetId,
        pub n_generics: NonZero<usize>,
    }

    impl TemplateGenericSpecs {
        pub fn new(gen_id: GenericSetId, n_generics: usize) -> Self {
            Self {
                id: gen_id,
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

        pub fn new(name: impl Into<Cow<'static, str>>, generics: TemplateGenericSpecs) -> Self {
            Self {
                name: name.into(),
                generics: Some(generics),
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
        pub raw_ty: Arc<Ty<'s>>,
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
        Unknown
    }

    impl<'s> Ty<'s> {
        pub fn resolve(self: &Arc<Self>, tail: Option<&Arc<Self>>, generic_args: &Vec<Arc<Self>>, gen_id: Option<GenericSetId>) -> Arc<Self> {
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
                Ty::Generic(_) | Ty::Unknown => self.clone(),
                Ty::Tail => tail.unwrap().clone(),
            }
        }

        pub fn unknown() -> Arc<Self> {
            static UNKNOWN_LZ: LazyLock<Arc<Ty<'static>>> = LazyLock::new(|| Arc::new(Ty::Unknown));
            UNKNOWN_LZ.clone()
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
                Ty::Unknown => {
                    write!(f, "?")
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
                raw_ty: field.raw_ty.resolve(Some(tail), &self.args, self.template.generic_id()),
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

#[derive(Debug, Clone)]
pub struct OverloadGenericParams<'s> {
    pub gen_id: GenericSetId,
    pub generic_params: Vec<Cow<'s, str>>,
}

impl<'s> OverloadGenericParams<'s> {
    pub fn new(gen_id: GenericSetId, generic_params: Vec<Cow<'s, str>>) -> Self {
        Self { gen_id, generic_params }
    }
}

// todo why is this clone?
#[derive(Debug, Clone)]
pub struct Overload<'s> {
    generic_params: Option<OverloadGenericParams<'s>>,
    args: Vec<OverloadArg<'s>>,
    return_ty: Arc<Ty<'s>>,
    loc: OverloadLoc,
    min_args_n: usize,
}

impl<'s> Overload<'s> {
    fn get_type(&self) -> Arc<Ty<'s>> {
        Arc::new(Ty::Fn(Fn::new(
            self.args.iter().map(|arg| arg.ty.clone()).collect(),
            self.return_ty.clone(),
        )))
    }

    fn min_args_n(&self) -> usize {
        self.min_args_n
    }

    pub fn new(
        generic_params: Option<OverloadGenericParams<'s>>,
        args: Vec<OverloadArg<'s>>,
        return_ty: Arc<Ty<'s>>,
        loc: OverloadLoc,
    ) -> Self {
        let min_args_n = args.iter().take_while(|arg| arg.default.is_none()).count();
        Self {
            generic_params,
            args,
            return_ty,
            loc,
            min_args_n,
        }
    }

    pub fn loc(&self) -> &OverloadLoc {
        &self.loc
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
    Value(
        // this is the cell idx relative to the overload location
        usize
    ),
    OverloadResolve(OverloadResolve<'s>),
}

#[derive(Debug, Clone)]
pub struct OverloadResolve<'s> {
    name: Cow<'s, str>,
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

    /// "retarget" a relative location to another cell/id on the same level
    fn retarget(&self, new_idx: usize) -> Self {
        match self {
            RelativeLocation::Local(_) => RelativeLocation::Local(new_idx),
            RelativeLocation::Capture(capture) => RelativeLocation::Capture(RelativeLocationCapture {
                ancestor_height: capture.ancestor_height,
                cell_idx: new_idx,
            }),
            RelativeLocation::Global(_) => RelativeLocation::Global(new_idx),
            RelativeLocation::System(system_loc) => RelativeLocation::System(SystemLoc {
                source: system_loc.source,
                id: new_idx,
            }),
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
    fn sequence(&self, ty: Arc<Ty<'s>>) -> Arc<Ty<'s>>;
}
enum AdditionalParam<'s>{
    Value(usize),
    OverloadResolve(OverloadCandidate<'s>),
}

struct OverloadCandidate<'s> {
    loc: RelativeLocation,
    output_ty: Arc<Ty<'s>>,
    additional_params: Vec<AdditionalParam<'s>>,
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

    fn int_ty(&self) -> Arc<Ty<'s>> {
        self.builtins.as_ref().unwrap().int()
    }

    fn float_ty(&self) -> Arc<Ty<'s>> {
        self.builtins.as_ref().unwrap().float()
    }

    fn bool_ty(&self) -> Arc<Ty<'s>> {
        self.builtins.as_ref().unwrap().bool()
    }

    fn str_ty(&self) -> Arc<Ty<'s>> {
        self.builtins.as_ref().unwrap().str()
    }

    fn sequence_ty(&self, ty: Arc<Ty<'s>>) -> Arc<Ty<'s>> {
        self.builtins.as_ref().unwrap().sequence(ty)
    }

    fn optional(&self, ty: Arc<Ty<'s>>) -> Arc<Ty<'s>> {
        self.gen_prim_type("Optional", vec![ty])
    }

    fn parse_type<'c: 's>(&self, ty: &ast::ty::TyWithPair<'c>, gen_params: &Option<OverloadGenericParams>) -> Result<Arc<Ty<'s>>, CompilationErrorWithPair<'c, 's>> {
        // todo gen-aprams should be option<&...>, not &option<...>
        match &ty.inner {
            ast::ty::Ty::Tuple(inners) => {
                let inners = inners
                    .iter()
                    .map(|inner| self.parse_type(inner, gen_params))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Arc::new(Ty::Tuple(inners)))
            }
            ast::ty::Ty::Fn(ast::ty::FnTy { args, ret }) => {
                let args = args
                    .iter()
                    .map(|arg| self.parse_type(arg, gen_params))
                    .collect::<Result<Vec<_>, _>>()?;
                let return_ty = self.parse_type(&ret, gen_params)?;
                Ok(Arc::new(Ty::Fn(Fn::new(args, return_ty))))
            }
            ast::ty::Ty::Specialized(ast::ty::SpecializedTy { name, args }) => {
                let Some(item) = self.get_named_item(name) else {
                    if let Some(gen) = gen_params.as_ref() {
                        // todo make this a hash search?
                        if let Some(idx) = gen.generic_params.iter().position(|param| param == name) {
                            return Ok(Arc::new(Ty::Generic(Generic::new(idx, gen.gen_id))));
                        }
                    }
                    return Err(CompilationError::NameNotFound { name: name.clone() }.with_pair(ty.pair.clone()));
                };
                match item {
                    RelativeNamedItem::Type(NamedType::Template(template)) => {
                        let args = args
                            .iter()
                            .map(|arg| self.parse_type(arg, gen_params))
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
            None => match self.parent {
                // todo increment the parent's cell indices
                Some(parent) => parent.get_named_item(name).map(|n| n.add_height(1)),
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
    
    fn resolve_overloads<'a, 'c: 's>(&'a self, name: &Cow<'c, str>, overloads: Vec<RelativeOverload<'p, 's>>, arg_types: &Vec<Arc<Ty<'s>>>, binding: Option<()>)->Result<OverloadCandidate<'s>, CompilationError<'c, 's>>{
        // TODO we only need the name in this function for error reporting, we should remove it
        if let Some(binding) = binding {
            todo!()
        }
        let is_one_option = overloads.len() == 1;
        let mut resolved_overloads = Vec::new();
        'outer: for rel_overload in overloads {
            let overload = rel_overload.overload;
            if arg_types.len() > overload.args.len() || arg_types.len() < overload.min_args_n() {
                if is_one_option {
                    // if there's only one overload, we can raise a better error here
                    return Err(CompilationError::ArgumentCountMismatch { func_name: name.clone(), min_n: overload.min_args_n(), max_n: overload.args.len(), actual_n: arg_types.len()});
                }
                continue;
            }
            let gen_id = overload.generic_params.as_ref().map(|g| g.gen_id);
            let mut resolution = BindingResolution::new(overload.generic_params.as_ref().map(|g| g.gen_id), overload.generic_params.as_ref().map(|g| g.generic_params.len()).unwrap_or_default());
            let mut additional_params = Vec::with_capacity(overload.args.len() - arg_types.len());
            for (i,(param, arg_ty)) in overload.args.iter().zip(arg_types.iter().map(Some).chain(repeat(None))).enumerate() {
                if let Some(arg_ty) = arg_ty {
                    if let Err(_) = resolution.assign(&param.ty, arg_ty){
                        if is_one_option {
                            // if there's only one overload, we can raise a better error here
                            return Err(CompilationError::ArgumentTypeMismatch { func_name: name.clone(), param_n: i, param_name: None, expected_ty: param.ty.clone(), actual_ty: arg_ty.clone()});
                        }
                        continue 'outer;
                    }
                } else {
                    if let Some(default) = &param.default {
                        match default {
                            OverloadArgDefault::Value(cell_idx) => {
                                additional_params.push(AdditionalParam::Value(*cell_idx));
                            },
                            OverloadArgDefault::OverloadResolve(OverloadResolve{name: ov_name}) => {
                                let resolved_ty = param.ty.resolve(None, &resolution.bound_generics, gen_id);
                                let Ty::Fn(expected_signature ) = resolved_ty.as_ref() else {
                                    unreachable!()
                                };
                                // todo avoid infinite recursion (note that it might be a double recursion)
                                let candidate = match self.get_named_item(ov_name) {
                                    Some(RelativeNamedItem::Overloads(RelativeNamedItemOverloads { overloads })) => {
                                        self.resolve_overloads(ov_name, overloads, &expected_signature.args, None)
                                    }
                                    Some(_) => {
                                        todo!()
                                    }
                                    None => {
                                      todo!()  
                                    }
                                };
                                let candidate = match candidate{
                                    Ok(candidate) => candidate,
                                    Err(_) => {
                                        if is_one_option {
                                            // if there's only one overload, we can raise a better error here
                                            // todo work the error into the error type
                                            return Err(CompilationError::FailedDefaultResolution { fn_name: name.clone(), param_n: i, overload_name: ov_name.clone()});
                                        }
                                        continue 'outer;
                                    }
                                };
                                additional_params.push(AdditionalParam::OverloadResolve(candidate));
                            }
                        }
                    } else {
                        unreachable!()
                    }
                }
            }
            // todo assert that all generics are resolved (not unknown)
            let output_type = overload.return_ty.resolve(None, &resolution.bound_generics, gen_id);
            resolved_overloads.push(OverloadCandidate {
                loc: rel_overload.loc,
                output_ty: output_type,
                additional_params,
            });
        };
        if resolved_overloads.is_empty() {
            return Err(CompilationError::NoOverloads { name: name.clone(), arg_types: arg_types.clone()});
        }
        else if resolved_overloads.len() > 1 {
            return Err(CompilationError::AmbiguousOverloads { name: name.clone(), arg_types: arg_types.clone()});
        }
        Ok(resolved_overloads.into_iter().next().unwrap())
    }

    fn feed_additional_param<'c: 's>(&mut self, main_loc: &RelativeLocation, param: AdditionalParam<'s>, sink: &mut Vec<Command<'c>>) {
        match param {
            AdditionalParam::Value(cell_idx) => {
                self.feed_relative_location(&main_loc.retarget(cell_idx), sink);
            }
            AdditionalParam::OverloadResolve(candidate) => {
                self.feed_overload_candidate_as_value(candidate, sink);
            }
        }
    }

    fn feed_overload_candidate_as_value<'c: 's>(&mut self, candidate: OverloadCandidate<'s>, sink: &mut Vec<Command<'c>>) -> Arc<Ty<'s>> {
        let OverloadCandidate{loc, output_ty, additional_params} = candidate;
        // TODO I'm pretty sure we can do away with reversing here
        let n_additional_params = additional_params.len();
        for param in additional_params.into_iter().rev() {
            self.feed_additional_param(&loc, param, sink);
        }
        self.feed_relative_location(&loc, sink);
        if n_additional_params != 0 {
            sink.push(Command::BindBack(n_additional_params));
        }
        output_ty
    }

    fn feed_overload_candidate_as_call<'c: 's>(&mut self, candidate: OverloadCandidate<'s>, arg_sinks: Vec<Vec<Command<'c>>>, sink: &mut Vec<Command<'c>>) -> Arc<Ty<'s>> {
        let OverloadCandidate{loc, output_ty, additional_params} = candidate;
        let n_args = arg_sinks.len();
        let n_additional_params = additional_params.len();
        // TODO I'm pretty sure we can do away with reversing here
        for param in additional_params.into_iter().rev() {
            self.feed_additional_param(&loc, param, sink);
        }
        for arg_sink in arg_sinks.into_iter().rev() {
            sink.extend(arg_sink);
        }
        self.feed_relative_location(&loc, sink);
        sink.push(Command::MakePending(n_args + n_additional_params));
        output_ty
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

                if let Expr::Ref(name) = &expr.as_ref().inner {
                    let named_item = self.get_named_item(name);
                    if let Some(RelativeNamedItem::Overloads(RelativeNamedItemOverloads { overloads })) = named_item{
                        let cand = self.resolve_overloads(name, overloads, &arg_types, None).map_err(|e| e.with_pair(expr.pair.clone()))?;
                        let output_ty = self.feed_overload_candidate_as_call(cand, arg_sinks, sink);
                        return Ok(output_ty)
                    }
                    if let Some(RelativeNamedItem::Type(NamedType::Template(template_arc))) = named_item {
                        if let TyTemplate::Compound(CompoundTemplate { compound_kind: CompoundKind::Struct, generics, fields , name: struct_name}) = template_arc.as_ref(){
                            if fields.len() != arg_types.len() {
                                return Err(CompilationError::ArgumentCountMismatch { func_name: struct_name.clone(), min_n: fields.len(), max_n:fields.len(), actual_n: arg_types.len()}.with_pair(expr.pair.clone()));
                            }
                            let mut resolution = BindingResolution::new(generics.as_ref().map(|g| g.id), generics.as_ref().map(|g| g.n_generics.get()).unwrap_or_default());
                            for (i, ((name, field), arg_ty)) in fields.iter().zip(arg_types.iter()).enumerate() {
                                if let Err(_) = resolution.assign(&field.raw_ty, arg_ty){
                                    return Err(CompilationError::ArgumentTypeMismatch { func_name: struct_name.clone(), param_n: i, param_name: Some(name.clone()), expected_ty: field.raw_ty.clone(), actual_ty: arg_ty.clone()}.with_pair(expr.pair.clone()));
                                }
                            }
                            // todo assert all types are resolved (not unknown)
                            let resolved_ty = template_arc.instantiate(resolution.bound_generics.into_iter().collect());
                            for arg_sink in arg_sinks.into_iter().rev() {
                                sink.extend(arg_sink);
                            }
                            sink.push(Command::Struct(fields.len()));
                            return Ok(resolved_ty);
                        }
                    }
                }

                for arg_sink in arg_sinks.into_iter().rev() {
                    sink.extend(arg_sink);
                }
                let ty = self.feed_expression(expr.as_ref(), sink)?;
                // todo check that the args match
                sink.push(Command::MakePending(arg_types.len()));

                match ty.as_ref() {
                    Ty::Fn(fn_ty) => {
                        // todo we should also consider the case where the function is generic
                        Ok(fn_ty.return_ty.clone())
                    }
                    _ => Err(CompilationError::NotCallable { ty: ty.clone()}.with_pair(expr.pair.clone()))
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
                Ok(self.int_ty())
            }
            Expr::LitFloat(value) => {
                sink.push(Command::PushFloat(*value));
                Ok(self.float_ty())
            }
            Expr::LitBool(value) => {
                sink.push(Command::PushBool(*value));
                Ok(self.bool_ty())
            }
            Expr::LitString(value) => {
                sink.push(Command::PushString(value.clone()));
                Ok(self.str_ty())
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
                    None => {
                        return Err(CompilationError::NameNotFound { name: name.clone() }.with_pair(expr.pair.clone()));
                    }
                }
            }
            Expr::Disambiguation(Disambiguation{name, arg_tys}) => {
                let arg_tys = arg_tys.iter().map(|ty| self.parse_type(ty, &None)).collect::<Result<Vec<_>, _>>()?;
                match self.get_named_item(name) {
                    Some(RelativeNamedItem::Overloads(overloads)) => {
                        let candidate = self.resolve_overloads(name, overloads.overloads, &arg_tys, None).map_err(|e| e.with_pair(expr.pair.clone()))?;
                        let output_ty = self.feed_overload_candidate_as_value(candidate, sink);
                        let fn_type = Arc::new(Ty::Fn(Fn::new(arg_tys, output_ty)));
                        Ok(fn_type)
                    }
                    None => {
                        return Err(CompilationError::NameNotFound { name: name.clone() }.with_pair(expr.pair.clone()));
                    }
                    _ => todo!(), // error?
                }
            }
            Expr::Attr(Attr { obj, name }) => {
                let obj_type = self.feed_expression(obj, sink)?;
                match obj_type.as_ref() {
                    Ty::Specialized(specialized) => {
                        let Some(field) = specialized.get_field(&name, &obj_type) else {
                            todo!()
                        }; // raise an error
                        sink.push(Command::Attr(field.idx));
                        let resolved_type = field.raw_ty.resolve(Some(&obj_type), &specialized.args, specialized.template.generic_id());
                        Ok(resolved_type)
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
                        Ok(field.raw_ty)
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
                        Ok(self.optional(field.raw_ty))
                    }
                    _ => todo!(), // raise an error
                }
            }
            Expr::Call(Call { functor, args }) => self.feed_call(functor, args, sink),
            Expr::MethodCall(MethodCall { obj, name, args }) => {
                let args = std::iter::once(obj.as_ref()).chain(args.iter());
                self.feed_call(&Functor::Expr(Box::new(Expr::Ref(name.clone()).with_pair(expr.pair.clone()))), args, sink)
            }
            Expr::Array(items)=>{
                let mut arr_ty = None;
                for item in items.iter().rev() {
                    let item_ty = self.feed_expression(item, sink)?;
                    if let Some(ty) = arr_ty.as_ref() {
                        match combine_types(ty, &item_ty){
                            Ok(combined) => arr_ty = Some(combined),
                            Err(_) => return Err(CompilationError::ArrayItemTypeMismatch { expected_ty: ty.clone(), actual_ty: item_ty}.with_pair(expr.pair.clone()))
                        }
                    }
                    else {
                        arr_ty = Some(item_ty);
                    }
                }
                let arr_ty = arr_ty.unwrap();
                sink.push(Command::Array(items.len()));
                Ok(self.sequence_ty(arr_ty))
            }
            Expr::Lookup(Lookup { obj, keys }) => {
                let mut args = vec![obj.as_ref()];
                args.extend(keys.iter());
                self.feed_call(&Functor::Expr(Box::new(Expr::Ref(Cow::Borrowed("lookup")).with_pair(expr.pair.clone()))), args.into_iter(), sink)
            }

            _ => todo!("handle expression {:?}", expr.inner),
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
                    let ty = self.parse_type(explicit_ty, &None)?;
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
                let fn_cell_idx = self.get_cell_idx();
                let gen_params = if generic_params.is_empty() {
                    None
                } else {
                    Some(OverloadGenericParams::new(GenericSetId::unique(), generic_params.iter().map(|p| p.clone()).collect()))
                };

                let raw_args = args
                    .into_iter()
                    .map(|fn_arg| -> Result<_, _> {
                        Ok((
                            fn_arg.name.clone(),
                            self.parse_type(&fn_arg.ty, &gen_params)?,
                            fn_arg.default.clone(),
                        ))
                    })
                    .collect::<Result<Vec<_>, _>>()?;                
                let expected_return_ty = self.parse_type(return_ty, &gen_params)?;

                let mut args = Vec::with_capacity(raw_args.len());
                let mut first_default = None;
                for (name, ty, default) in raw_args.iter() {
                    let default = if let Some(default) = default {
                        if first_default.is_none() {
                            first_default = Some(name);
                        }

                        Some(match default{
                            FnArgDefault::Value(expr) => {
                                let default_cell_idx = self.get_cell_idx();
                                let default_ty = self.feed_expression(expr, sink)?;
                                // todo check that default_ty is a subtype of ty
                                sink.push(Command::PopToCell(default_cell_idx));
                                OverloadArgDefault::Value(default_cell_idx)
                            }
                            FnArgDefault::ResolveOverload(res) => {
                                // todo check that the  type is a function
                                OverloadArgDefault::OverloadResolve(OverloadResolve{name: res.name.clone()})
                            }
                        })
                    } else {
                        if first_default.is_some(){
                            return Err(CompilationError::ParameterWithoutDefaultAfterDefault { param_name: name.clone(), default_param_name: first_default.cloned().unwrap() }.with_pair(stmt.pair.clone()));
                        }
                        None
                    };
                    args.push(
                        OverloadArg {
                            ty: ty.clone(),
                            default,
                        }
                    );
                }

                // todo check that we don't shadow a variable
                let new_overload = Overload::new(gen_params, args, expected_return_ty, OverloadLoc::Cell(fn_cell_idx));
                self.add_overload(name.clone(), new_overload);
                
                
                let mut subscope = self.child();
                assert!(!subscope.is_root());
                let mut subscope_sink = Vec::new();
                subscope.set_generics(generic_params.iter().cloned());
                subscope.feed_params(
                    raw_args.iter().map(|(name, ty, _)| (name.clone(), ty.clone())),
                    &mut subscope_sink,
                );
                for body_stmt in body {
                    subscope.feed_statement(body_stmt, &mut subscope_sink)?;
                }
                let actual_return_ty = subscope.feed_return(&ret, &mut subscope_sink)?;
                // todo check that return_tys match
                let n_captures = subscope.pending_captures.len();
                let n_cells = subscope.n_cells;
                for PendingCapture{ancestor_height, cell_idx} in subscope.pending_captures.iter() {
                    if *ancestor_height == 1 {
                        if cell_idx == &fn_cell_idx {
                            sink.push(Command::PushTail);
                        }
                        else {
                            sink.push(Command::PushFromCell(*cell_idx));
                        }
                    }
                    else {
                        self.pending_captures.push(PendingCapture{ancestor_height: *ancestor_height - 1, cell_idx: *cell_idx});
                    }
                }
                sink.push(Command::MakeFunction(MakeFunction {
                    n_captures,
                    n_cells,
                    commands: subscope_sink,
                }));
                
                sink.push(Command::PopToCell(fn_cell_idx));
                Ok(())
            }
            Stmt::Compound(compound) => {
                let (generics, ov) = if compound.generic_params.is_empty() {
                    (None, None)
                } else {
                    let id = GenericSetId::unique();
                    (Some(TemplateGenericSpecs::new(id, compound.generic_params.len())), Some(OverloadGenericParams::new(id, compound.generic_params.iter().map(|p| p.clone()).collect())))
                };
                let fields = compound
                    .fields
                    .iter()
                    .map(|field| -> Result<_, _> {
                        Ok((
                            // todo check for duplicates
                            field.name.clone(),
                            Field {
                                raw_ty: self.parse_type(&field.ty, &ov)?,
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
