use std::io::{BufReader, BufRead};
use std::fs::File;


fn main() {
    let mut result = 0;
    for line in BufReader::new(File::open("inputs/day1.txt").unwrap()).lines() {
        let m: i32 = line.unwrap().parse().unwrap();
        result += m / 3 - 2;
    }
    println!("{:?}", result);
}
