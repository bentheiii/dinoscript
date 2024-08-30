use crate::{bytecode::SourceId, compilation_scope::CompilationScope, runtime::RuntimeFrame};

pub trait DinoPack {
    fn setup_compiler(&self, scope: &mut CompilationScope);
    fn setup_runtime(&self, frame: &mut RuntimeFrame);
}

pub(crate) mod utils {
    use std::{borrow::Cow, sync::Arc};

    use crate::{
        bytecode::SourceId, compilation_scope::{ty::Ty, CompilationScope, Overload, OverloadArg, OverloadLoc, SystemLoc}, dinobj::{DinObject, SourceFnFunc, UserFn}
    };

    pub(crate) enum SetupItem<'s, C> {
        Function(SetupFunction<'s, C>),
    }

    impl<'s, C> SetupItem<'s, C> {
        pub(crate) fn push_to_compilation(
            &self,
            scope: &mut CompilationScope<'_, 's>,
            c: C,
            source: SourceId,
            mut id_generator: impl FnMut() -> usize,
        ) {
            match self {
                SetupItem::Function(f) => {
                    let (name, overload) = f.to_overload(
                        c,
                        SystemLoc::new(source, id_generator()),
                    );
                    scope.add_overload(name, overload);
                }
            }
        }

        pub(crate) fn to_dinobject(self) -> Arc<DinObject<'s>> {
            match self {
                SetupItem::Function(f) => f.to_dinobject(),
            }
        }
    }

    pub(crate) struct SetupFunction<'s, C> {
        pub(crate) sig: fn(C) -> Signature<'s>,
        pub(crate) body: SetupFunctionBody<'s>,
    }

    impl<'s, C> SetupFunction<'s, C> {
        pub(crate) fn new(sig: fn(C) -> Signature<'s>, body: SetupFunctionBody<'s>) -> Self {
            Self { sig, body }
        }

        pub(crate) fn to_overload(&self, c: C, loc: SystemLoc) -> (Cow<'s, str>, Overload<'s>) {
            let sig = (self.sig)(c);
            (sig.name.clone(), sig.to_overload(loc))
        }

        pub(crate) fn to_dinobject(self) -> Arc<DinObject<'s>> {
            match self.body {
                SetupFunctionBody::System(f) => Arc::new(DinObject::SourceFn(f)),
                SetupFunctionBody::User(f) => Arc::new(DinObject::UserFn(f)),
            }
        }
    }

    pub(crate) struct Signature<'s> {
        name: Cow<'s, str>,
        // todo generics
        args: Vec<Arg<'s>>,
        ret: Arc<Ty<'s>>,
    }

    impl<'s> Signature<'s> {
        pub(crate) fn new(name: Cow<'s, str>, args: Vec<Arg<'s>>, ret: Arc<Ty<'s>>) -> Self {
            Self { name, args, ret }
        }

        pub(crate) fn to_overload(&self, loc: SystemLoc) -> Overload<'s> {
            Overload {
                generic_params: Vec::new(),
                args: self.args.iter().map(Arg::to_overload_arg).collect(),
                return_ty: self.ret.clone(),
                loc: OverloadLoc::System(loc),
            }
        }
    }

    pub(crate) struct Arg<'s> {
        name: Cow<'s, str>,
        ty: Arc<Ty<'s>>,
        // todo default
    }

    impl<'s> Arg<'s> {
        pub(crate) fn new(name: Cow<'s, str>, ty: Arc<Ty<'s>>) -> Self {
            Self { name, ty }
        }

        pub(crate) fn to_overload_arg(&self) -> OverloadArg<'s> {
            OverloadArg {
                ty: self.ty.clone(),
                default: None,
            }
        }
    }

    pub(crate) enum SetupFunctionBody<'s> {
        System(SourceFnFunc),
        User(UserFn<'s>),
    }
}
