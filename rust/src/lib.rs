use std::io;
use std::io::BufRead;
use std::io::Read;
use std::io::Write;
use std::fs;


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
    write_prompt()?;
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        println!("{}", line?);
        write_prompt()?;
    }
    Ok(())
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
}
