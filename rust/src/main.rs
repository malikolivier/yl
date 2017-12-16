extern crate yl;

use std::env;
use std::process;


fn main() {
    let args: Vec<String> = env::args().collect();
    if yl::is_interactive(&args) {
        if let Err(e) = yl::run_prompt() {
            eprintln!("Application error: {}", e);
            process::exit(1);
        }
    } else {
        let code = yl::get_code(&args).unwrap_or_else(|err| {
            eprintln!("Could not get code: {:?}", err);
            process::exit(1);
        });
        let ret = yl::evaluate_code_with_exit_status(code);
        process::exit(ret);
    }
}
