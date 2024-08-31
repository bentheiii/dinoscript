mod items_template;

use std::{collections::HashMap, fs::File, io::{BufRead, BufReader, Read, Write}};

use items_template::setup_items;
fn main(){
    let replacements = setup_items().replacements;
    
    let template_file = BufReader::new(File::open("items_template.rs").unwrap());
    let output_file = File::create("src/items.rs").unwrap();
    apply_template(output_file, template_file, &replacements);
}


fn apply_template<R: BufRead, W: Write>(mut write: W, read: R, replacements: &HashMap<&str, String>) {
    fn get_pragma_parts(pragma: &str) -> Option<Vec<&str>> {
        pragma.trim().strip_prefix("// pragma:").map(|p| p.split_whitespace().collect())
    }
    let mut lines = read.lines();
    while let Some(raw_line) = lines.next().transpose().unwrap() {
        if let Some(parts) = get_pragma_parts(&raw_line) {
            match parts[0] {
                "replace-start" => {
                    let initial_indent = raw_line.chars().take_while(|c| c.is_whitespace()).collect::<String>();
                    let mut replace_id = None;
                    loop {
                        let raw_line = lines.next().unwrap().unwrap();
                        if let Some(parts) = get_pragma_parts(&raw_line) {
                            match parts[0] {
                                "replace-end" => {
                                    assert!(replace_id.is_some());
                                    write.write_all(initial_indent.as_bytes()).unwrap();
                                    write.write_all(replacements[replace_id.as_deref().unwrap()].as_bytes()).unwrap();
                                    break;
                                },
                                "replace-id" => {
                                    assert!(replace_id.is_none());
                                    let replace_id_line = lines.next().unwrap().unwrap();
                                    replace_id = Some(replace_id_line.trim_matches(|c: char| !c.is_alphanumeric()).to_string());
                                },
                                "replace-with-raw" => {
                                    let first_line = lines.next().unwrap().unwrap();
                                    let first_raw_line = first_line.trim();
                                    if first_raw_line == "/*"{
                                        loop {
                                            let raw_line = lines.next().unwrap().unwrap();
                                            if raw_line.trim() == "*/"{
                                                assert_eq!(lines.next().unwrap().unwrap().trim(), "// pragma:replace-end");
                                                break;
                                            }
                                            write.write_all(raw_line.as_bytes()).unwrap();
                                            write.write_all(b"\n").unwrap();
                                        }
                                    } else if let Some(["replace-end"]) = get_pragma_parts(first_raw_line).as_deref(){
                                    } else {
                                        panic!("Expected a comment or a replace-end pragma, got: {first_raw_line}")
                                    }
                                    break;
                                }
                                _ => {panic!("Unknown pragma inside replace: {raw_line}")}
                            }
                        }
                    }
                }
                "skip" => {
                    let n = parts[1].parse::<usize>().unwrap();
                    for _ in 0..n {
                        lines.next().unwrap().unwrap();
                    }
                }
                "unwrap-start" => {
                    lines.next().unwrap().unwrap();
                    loop {
                        let raw_line = lines.next().unwrap().unwrap();
                        if let Some(parts) = get_pragma_parts(&raw_line) {
                            match parts[0] {
                                "unwrap-end" => {
                                    lines.next().unwrap().unwrap();
                                    break
                                },
                                _ => {panic!("Unknown pragma inside unwrap: {raw_line}")}
                            }
                        }
                        else {
                            let line_to_write = raw_line.strip_prefix("    ").unwrap();
                            write.write_all(line_to_write.as_bytes()).unwrap();
                            write.write_all(b"\n").unwrap();
                        }
                    }
                }
                _ => {panic!("Unknown pragma: {raw_line}")}
            }
        } else {
            write.write_all(raw_line.as_bytes()).unwrap();
            write.write_all(b"\n").unwrap();
        }

    };
}