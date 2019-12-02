use std::io::{BufReader, BufRead};
use std::fs::File;

fn fuel(m: i32) -> i32 {
    m / 3 - 2
}

fn total_fuel(m: i32) -> i32 {
    let mut result = 0;
    let mut additional_m = m;
    loop {
        additional_m = fuel(additional_m);
        if additional_m > 0 {
            result += additional_m
        } else {
            break;
        }
    }
    result
}

fn main() {
    let mut result = 0;
    for line in BufReader::new(File::open("inputs/day1.txt").unwrap()).lines() {
        let m: i32 = line.unwrap().parse().unwrap();
        result += total_fuel(m);
    }
    println!("{:?}", result);
}
