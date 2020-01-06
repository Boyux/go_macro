use std::fs;
use std::env;
use std::collections::HashMap;
use std::env::Args;
use std::path::PathBuf;
use std::process::{self, Command};
use go_macro::{self, Invoker, Code, Concat, Str};

fn main() {
    let mut macros: HashMap<String, Box<dyn Invoker>> = HashMap::new();
    macros.insert("concat".to_string(), Box::new(Concat));
    macros.insert("str".to_string(), Box::new(Str));

    let opt = Opt::from(env::args());
    if !opt.target.exists() {
        fs::create_dir(&opt.target).expect("error create directory");
    }
    if !opt.target.is_dir() {
        panic!("target should be a directory.");
    }

    if opt.source.is_dir() {
        // directory process
        let mut codes: HashMap<PathBuf, Code> = HashMap::new();
        for entry in opt.source.read_dir().expect("error read directory") {
            let path = entry.expect("error read file").path();
            if !path.is_dir() {
                match path.extension() {
                    Some(s) if s == "go" => {
                        let mut stack = Vec::new();
                        stack.push(&path);
                        let code = go_macro::read_code(stack, &mut macros).expect("error read code");
                        codes.insert(path, code);
                    }
                    _ => ()
                }
            }
        }
        for (path, mut code) in codes.into_iter() {
            code = code.process(&macros).expect("error process macros");
            fs::write(opt.target.join(path.file_name().expect("unexpected file name")), code.to_string())
                .expect("error writing to target");
        }
    } else {
        // single file process
        if opt.source.extension().expect("unexpected extension") != "go" {
            panic!("the extension of file should be *.go.");
        }
        let mut stack = Vec::new();
        stack.push(&opt.source);
        let mut code = go_macro::read_code(stack, &mut macros).expect("error read code");
        code = code.process(&macros).expect("error process macros");
        fs::write(opt.target.join(opt.source.file_name().expect("unexpected file name")), code.to_string())
            .expect("error writing to target");
    }

    // format option
    if opt.format {
        let output = Command::new("go")
            .arg("fmt")
            .args(opt.target.to_str())
            .output()
            .expect("error go fmt");

        if output.status.success() {
            print!("{}", String::from_utf8(output.stdout).expect("unexpected stdout"));
        } else {
            print!("{}", String::from_utf8(output.stderr).expect("unexpected stderr"));
        }
    }
}

struct Opt {
    source: PathBuf,
    target: PathBuf,
    format: bool,
}

impl From<Args> for Opt {
    fn from(mut args: Args) -> Opt {
        let mut opt = Opt {
            source: "./".into(),
            target: "./go-target".into(),
            format: false,
        };

        loop {
            match args.next() {
                Some(arg) => {
                    match arg.as_str() {
                        "--target" => {
                            match args.next() {
                                Some(target) => opt.target = target.into(),
                                None => panic!("expect a string as --target argument.")
                            }
                        }
                        "--source" => {
                            match args.next() {
                                Some(source) => opt.source = source.into(),
                                None => panic!("expect a string as --source argument.")
                            }
                        }
                        "--format" => {
                            opt.format = true;
                        }
                        "--help" => {
                            let help = "commands:\n\
                                \t--source input file/directory\n\
                                \t--target output directory\n\
                                \t--format whether format output code(gofmt required)\n\
                                \t--help   see this help";
                            eprintln!("{}", help);
                            process::exit(0);
                        }
                        _ => (),
                    }
                }
                None => break
            }
        }

        opt
    }
}