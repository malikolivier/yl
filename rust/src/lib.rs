use std::io;
use std::io::BufRead;
use std::io::Write;


pub fn is_interactive(args: &[String]) -> bool {
    args.len() <= 1
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
