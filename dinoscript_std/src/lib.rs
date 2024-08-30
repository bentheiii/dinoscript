use std::{fs::File, sync::{Arc, LazyLock}};
use std::io::Write;

use dinoscript_core::{
    bytecode::{Command, MakeFunction, PushFromSource, SourceId, to_in_code},
    compilation_scope::{
        self,
        ty::{BuiltinTemplate, Ty, TyTemplate},
        CompilationScope, NamedItem, NamedType,
    },
    dinobj::{DinObject, StackItem, UserFn},
    dinopack::{
        utils::{SetupFunction, SetupFunctionBody, SetupItem, Signature},
        DinoPack,
    },
    maybe_owned::MaybeOwned,
    runtime::RuntimeFrame,
};

pub struct StdPack;

pub struct Builtins<'s> {
    pub int: Arc<Ty<'s>>,
    pub float: Arc<Ty<'s>>,
    pub bool: Arc<Ty<'s>>,
    pub str: Arc<Ty<'s>>,
}

impl<'s> compilation_scope::Builtins<'s> for Builtins<'s> {
    fn int(&self) -> Arc<Ty<'s>> {
        self.int.clone()
    }

    fn float(&self) -> Arc<Ty<'s>> {
        self.float.clone()
    }

    fn bool(&self) -> Arc<Ty<'s>> {
        self.bool.clone()
    }

    fn str(&self) -> Arc<Ty<'s>> {
        self.str.clone()
    }
}

macro_rules! signature_fn {
    ($name:ident ($($param_name:ident : $type_name:ident),*) -> $ret_type:ident) => {
        |b: &Builtins<'_>| Signature::new(
            std::borrow::Cow::Borrowed(stringify!($name)),
            vec![$(dinoscript_core::dinopack::utils::Arg::new(std::borrow::Cow::Borrowed(stringify!($param_name)), b.$type_name.clone())),*],
            b.$ret_type.clone(),
        )
    };
}


#[cfg(feature="build_ds")]
macro_rules! from_source {
    ($source_id:ident, $builder:expr) => {
        {
            static EMPTY_COMMANDS: Vec<Command> = Vec::new();
            let raw_src = include_str!(concat!("ds_std_funcs/", stringify!($source_id), ".dsf"));
            let (commands, n_cells) = $builder.parse_and_save(stringify!($source_id), raw_src);
            // we return an empty vec since we don't really care what happens in real time
            SetupFunctionBody::User(UserFn::without_capture(n_cells, &EMPTY_COMMANDS))
        }
    };
}

#[cfg(not(feature="build_ds"))]
macro_rules! from_source {
    ($source_id:ident, $builder:expr) => {
        {
            static LZ: LazyLock<Vec<Command>> = LazyLock::new(|| {
                include!(concat!("ds_std_funcs/", stringify!($source_id), ".dsbi"))
            });
            SetupFunctionBody::User(UserFn::without_capture(include!(concat!("ds_std_funcs/", stringify!($source_id), ".cln")), &LZ))
        }
    };
}

#[cfg(feature="build_ds")]
struct SourceBuilder<'p, 's> {
    scope: CompilationScope<'p, 's, Builtins<'s>>,
    builtins: Builtins<'s>,
    source_id: SourceId,
    next_id: usize,
}

#[cfg(not(feature="build_ds"))]
struct SourceBuilder<'p, 's>{
    p_phantom: std::marker::PhantomData<&'p ()>,
    s_phantom: std::marker::PhantomData<&'s ()>,
}

#[cfg(feature="build_ds")]
impl<'p, 's> SourceBuilder<'p, 's> {
    fn parse_and_save(&mut self, name: &'static str, code: &'static str)->(Vec<Command<'static>>, usize){
        let func_stmt = dinoscript_core::grammar::parse_raw_function(code).unwrap();
        let mut sink = Vec::new();
        self.scope.feed_statement(&func_stmt, &mut sink).unwrap();
        // the sink should now have two items, the first is the actual function, the second is popping it to some cell (that we don't care about)
        // in the future, the sink might also include default values and the like, but it will never include captures since these are globals functions
        sink.pop().unwrap();  // remove the pop command
        let Command::MakeFunction(mk_func) = sink.pop().unwrap() else { panic!("Expected a MakeFunction command") };
        assert!(mk_func.n_captures == 0);
        let n_cells = mk_func.n_cells;
        let commands = mk_func.commands;
        // now we save the commands and n_cells to the file
        File::create(format!("dinoscript_std/src/ds_std_funcs/{name}.dsbi")).unwrap().write_all(to_in_code(&commands).as_bytes()).unwrap();
        File::create(format!("dinoscript_std/src/ds_std_funcs/{name}.cln")).unwrap().write_all(n_cells.to_string().as_bytes()).unwrap();
        (commands, n_cells)
    }
    fn add_item<'c>(&'c mut self, item: SetupItem<'s, &'c Builtins<'s>>) -> SetupItem<'s, &'c Builtins<'s>> {
        let builtins = &self.builtins;
        item.push_to_compilation(&mut self.scope, builtins, self.source_id, || {
            let ret = self.next_id;
            self.next_id += 1;
            ret
        });
        item
    }
}

#[cfg(not(feature="build_ds"))]
impl SourceBuilder<'_, '_> {
    fn new()->Self{
        SourceBuilder{p_phantom: std::marker::PhantomData, s_phantom: std::marker::PhantomData}
    }
    fn add_item<'a, 'b, 'c>(&mut self, item: SetupItem<'a, &'c Builtins<'b>>) -> SetupItem<'a, &'c Builtins<'b>> {
        item
    }
}


impl StdPack {
    const SOURCE_ID: SourceId = "core";
    #[cfg(feature="build_ds")]
    fn prepare_build<'p>()->SourceBuilder<'p, 'static>{
        let mut scope = CompilationScope::root();
        let builtins = Self::pre_items_setup(&mut scope);
        SourceBuilder{scope, builtins, source_id: Self::SOURCE_ID, next_id: 0}
    }

    #[cfg(not(feature="build_ds"))]
    fn prepare_build()->SourceBuilder<'static, 'static>{
        SourceBuilder::new()
    }

    fn pre_items_setup<'p, 's>(scope: &mut CompilationScope<'p, 's, Builtins<'s>>)->Builtins<'s>{
        fn register_type<'p, 's, B>(
            scope: &mut CompilationScope<'p, 's, B>,
            name: &'s str,
            template: TyTemplate<'s>,
        ) -> Arc<Ty<'s>> {
            let template = Arc::new(template);
            let ret = template.instantiate(vec![]);
            scope
                .names
                .insert(name.into(), NamedItem::Type(NamedType::Template(template)));
            ret
        }

        let int = register_type(scope, "int", TyTemplate::Builtin(BuiltinTemplate::primitive("int")));
        let float = register_type(scope, "float", TyTemplate::Builtin(BuiltinTemplate::primitive("float")));
        let bool = register_type(scope, "bool", TyTemplate::Builtin(BuiltinTemplate::primitive("bool")));
        let str = register_type(scope, "str", TyTemplate::Builtin(BuiltinTemplate::primitive("str")));

        Builtins { int, float, bool, str }
    }
    fn setup_items<'a, 's>() -> Vec<SetupItem<'s, &'a Builtins<'s>>> {
        let mut builder = Self::prepare_build();
        vec![
            builder.add_item(
                SetupItem::Function(SetupFunction::new(
                    signature_fn!(add (a: int, b: int) -> int),
                    SetupFunctionBody::System(Box::new(|mut stack| {
                        let StackItem::Value(Ok(b)) = stack.pop().unwrap() else {
                            todo!()
                        };
                        let StackItem::Value(Ok(a)) = stack.pop().unwrap() else {
                            todo!()
                        };
                        match (a.as_ref(), b.as_ref()) {
                            (DinObject::Int(a), DinObject::Int(b)) => Ok(Ok(Arc::new(DinObject::Int(a + b)))),
                            _ => Ok(Err(())),
                        }
                    })),
                ))
            ),
            SetupItem::Function(SetupFunction::new(
                signature_fn!(double (a: int) -> int),{
                    from_source!(double, builder)
                },
            )),
        ]
    }
}

impl DinoPack for StdPack {
    type Builtins<'s> = Builtins<'s>;

    fn setup_compiler<'p, 's>(&self, scope: &mut CompilationScope<'p, 's, Self::Builtins<'s>>) {
        let source_id = StdPack::SOURCE_ID;

        let builtins = Self::pre_items_setup(scope);

        let mut next_id = 0;
        let mut get_id = || {
            let ret = next_id;
            next_id += 1;
            ret
        };

        for item in Self::setup_items() {
            item.push_to_compilation(scope, &builtins, source_id, &mut get_id);
        }

        scope.builtins = Some(MaybeOwned::Owned(builtins));
    }

    fn setup_runtime(&self, frame: &mut RuntimeFrame) {
        let source_id = StdPack::SOURCE_ID;
        let mut items = Vec::new();

        for item in Self::setup_items() {
            items.push(item.to_dinobject());
        }

        frame.add_source(source_id, items);
    }
}
