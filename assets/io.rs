#![crate_type = "staticlib"]

use std::io;
use std::io::Write;

#[no_mangle]
pub extern "C" fn _i_write(arg: i64) {
    println!("Output int: {}", arg);
    io::stdout().flush().unwrap();
}

#[no_mangle]
pub extern "C" fn _b_write(arg: bool) {
    println!("Output bool: {}", arg);
    io::stdout().flush().unwrap();
}

#[no_mangle]
pub extern "C" fn read_bool() -> bool {
    loop {
        let mut buffer = String::new();
        
        print!("Input bool (t/f): ");
        io::stdout().flush().unwrap();
        
        match io::stdin().read_line(&mut buffer).map(|_| { buffer.trim().to_lowercase().chars().next() }) {
            Ok(Some('t')) => return true,
            Ok(Some('f')) => return false,
            _ => {
                print!("Invalid input, ");
                io::stdout().flush().unwrap();
            }
        };
    }
}

#[no_mangle]
pub extern "C" fn read_int() -> i64 {
    loop {
        let mut buffer = String::new();

        print!("Input int: ");
        io::stdout().flush().unwrap();

        match io::stdin().read_line(&mut buffer).map(|_| buffer.trim().parse().ok()) {
            Ok(Some(num)) => return num,
            _ => {
                print!("Invalid input, ");
                io::stdout().flush().unwrap();
            }
        };
    }
}