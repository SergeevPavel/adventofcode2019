use std::io::{BufReader, BufRead};
use std::fs::File;

fn run(program: &mut Vec<u64>) {
    let mut pc = 0;
    while pc < program.len() {
        match program[pc] {
            1 | 2 => {
                if let [op, a1, a2, r] = program[pc..pc + 4] {
                    match op {
                        1 => {
                            program[r as usize] = program[a1 as usize] + program[a2 as usize];
                        }
                        2 => {
                            program[r as usize] = program[a1 as usize] * program[a2 as usize];
                        }
                        _ => unreachable!()
                    }
                } else {
                    println!("Unexpected end of program, not enough arguments on {:?}", pc);
                }
            }
            99 => {
                break;
            }
            op@_ => {
                println!("Get {:?} at {:?}", op, pc);
                println!("Program: {:?}", program);
                break;
            }
        }
        pc += 4;
    }
}

fn main() {
    let mut program: Vec<_> = BufReader::new(File::open("inputs/day2.txt").unwrap()).split(b',')
        .map(|x| {
            String::from_utf8(x.unwrap()).unwrap().parse::<u64>().unwrap()
        })
        .collect();
    program[1] = 12;
    program[2] = 2;
    run(&mut program);
    println!("{:?}", program);
}