use std::borrow::Cow;

#[derive(Debug)]
pub enum Command<'s> {
    PopToCell(usize), // POP
    EvalTop,          // EVL // this is always for a return

    PushInt(i64),                   // PIC
    PushFloat(f64),                 // PFC
    PushBool(bool),                 // PBC
    PushString(Cow<'s, str>),       // PSC
    PushFromCell(usize),            // PCL
    PushFromSource(PushFromSource), // PSR
    PushFromCapture(usize),         // PCP
    PushGlobal(usize),              // PGL
    PushTail(usize),                // PT-

    MakeFunction(MakeFunction<'s>), // MFN
    MakePending(usize),             // MPE

    Attr(usize),       // ATR
    Struct(usize),     // STR
    Variant(usize),    // VAR
    VariantOpt(usize), // VOP
}

impl<'s> Command<'s> {
    /// Convert the command to rust code that can be used to generate the command
    pub fn to_in_code(&self, indent: &str) -> String {
        match self{
            Command::PopToCell(cell)=>format!("{indent}dinoscript_core::bytecode::Command::PopToCell({})",cell),
            Command::EvalTop=>format!("{indent}dinoscript_core::bytecode::Command::EvalTop"),
            Command::PushInt(i)=>format!("{indent}dinoscript_core::bytecode::Command::PushInt({})",i),
            Command::PushFloat(f)=>format!("{indent}dinoscript_core::bytecode::Command::PushFloat({})",f),
            Command::PushBool(b)=>format!("{indent}dinoscript_core::bytecode::Command::PushBool({})",b),
            Command::PushString(s)=>format!("{indent}dinoscript_core::bytecode::Command::PushString({}.into())",s),
            Command::PushFromCell(cell)=>format!("{indent}dinoscript_core::bytecode::Command::PushFromCell({})",cell),
            Command::PushFromSource(source)=>format!("{indent}dinoscript_core::bytecode::Command::PushFromSource(dinoscript_core::bytecode::PushFromSource::new(\"{}\",{}))",source.source,source.id),
            Command::PushFromCapture(capture)=>format!("{indent}dinoscript_core::bytecode::Command::PushFromCapture({})",capture),
            Command::PushGlobal(global)=>format!("{indent}dinoscript_core::bytecode::Command::PushGlobal({})",global),
            Command::PushTail(tail)=>format!("{indent}dinoscript_core::bytecode::Command::PushTail({})",tail),
            Command::MakeFunction(function)=>{
                let inner_commands=function.commands.iter().map(|c|c.to_in_code(&format!("{indent}    "))).collect::<Vec<String>>().join(&format!(",\n{indent}"));
                format!("{indent}dinoscript_core::bytecode::Command::MakeFunction(dinoscript_core::bytecode::MakeFunction::new({},{},vec![\n{inner_commands}\n{indent}]))",function.n_captures,function.n_cells)
            },
            Command::MakePending(pending)=>format!("{indent}dinoscript_core::bytecode::Command::MakePending({})",pending),
            Command::Attr(attr)=>format!("{indent}dinoscript_core::bytecode::Command::Attr({})",attr),
            Command::Struct(structure)=>format!("{indent}dinoscript_core::bytecode::Command::Struct({})",structure),
            Command::Variant(variant)=>format!("{indent}dinoscript_core::bytecode::Command::Variant({})",variant),
            Command::VariantOpt(variant_opt)=>format!("{indent}dinoscript_core::bytecode::Command::VariantOpt({})",variant_opt),
        }
    }
}

pub fn to_in_code<'s>(commands: &Vec<Command<'s>>, indent: &str) -> String {
    let inner = commands
        .iter()
        .map(|c| c.to_in_code(&format!("{indent}    ")))
        .collect::<Vec<String>>()
        .join(&format!(",\n"));
    format!("vec![\n{inner}\n{indent}]")
}

pub type SourceId = &'static str;

#[derive(Debug)]
pub struct PushFromSource {
    pub source: SourceId,
    pub id: usize,
}

impl PushFromSource {
    pub fn new(source: SourceId, id: usize) -> Self {
        Self { source, id }
    }
}

#[derive(Debug)]
pub struct MakeFunction<'s> {
    pub n_captures: usize,
    pub n_cells: usize,
    pub commands: Vec<Command<'s>>,
}

impl<'s> MakeFunction<'s> {
    pub fn new(n_captures: usize, n_cells: usize, commands: Vec<Command<'s>>) -> Self {
        Self {
            n_captures,
            n_cells,
            commands,
        }
    }
}
