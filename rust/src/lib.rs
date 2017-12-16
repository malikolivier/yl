use std::io;
use std::io::BufRead;
use std::io::Read;
use std::io::Write;
use std::fs;

mod interpreter;
mod parser;
use interpreter::YlVar;


pub fn is_interactive(args: &[String]) -> bool {
    args.len() <= 1
}

fn usage() {
    println!("yl [-e \"Inline code\"] [file]");
}

#[derive(Debug)]
pub enum YlError {
    Message(&'static str),
    Io(io::Error),
}


pub fn get_code(args: &[String]) -> Result<String, YlError> {
    debug_assert!(args.len() >= 2);
    match args[1].as_ref() {
        "-e" => {
            if args.len() <= 2 {
                usage();
                return Err(YlError::Message("Not enough arguments"))
            } else {
                Ok(args[2].clone())
            }
        },
        _ => {
            let mut f = try!(fs::File::open(&args[1]).map_err(YlError::Io));
            let mut code = String::new();
            try!(f.read_to_string(&mut code).map_err(YlError::Io));
            Ok(code)
        }
    }
}


fn write_prompt() -> Result<(), io::Error> {
    print!("> ");
    io::stdout().flush()
}

pub fn run_prompt() -> Result<(), Box<io::Error>> {
    let scope = interpreter::YlScope::new(None);
    write_prompt()?;
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let ast = parser::parse(&line?);
        println!("{:?}", ast);
        let ret = interpreter::evaluate_in_scope(&ast, &scope, false);
        interpreter::print(&ret);
        write_prompt()?;
    }
    Ok(())
}

pub fn evaluate_code_with_exit_status(code: String) -> i32 {
    let ast = parser::parse(&code);
    let ret = interpreter::evaluate(&ast);
    match ret {
        YlVar::Num(n) => n as i32,
        _ => 0,
    }
}


#[cfg(test)]
mod get_code {
    use super::get_code;
    #[test]
    fn from_cli() {
        let args = vec!["yl".to_string(),
                        "-e".to_string(),
                        "(code)".to_string()];
        assert_eq!(
            get_code(&args).unwrap(),
            "(code)"
        );
    }
    #[test]
    fn from_file() {
        let args = vec!["yl".to_string(),
                        "../test/argv.yl".to_string()];
        assert_eq!(
            get_code(&args).unwrap(),
            "(print (argv 0))\n"
        );
    }
    #[test]
    fn from_non_existing_file() {
        let args = vec!["yl".to_string(),
                        "I DON'T EXIST.yl".to_string()];
        assert!(
            match get_code(&args) {
                Ok(_) => false,
                Err(_) => true,
            }
        );
    }
}
