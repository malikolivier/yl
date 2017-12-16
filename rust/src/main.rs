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
    }
}
