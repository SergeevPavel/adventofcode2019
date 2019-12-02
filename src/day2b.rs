use std::io::{BufReader, BufRead};
use std::fs::File;

fn run(mut program: Vec<u64>, noun: u64, verb: u64) -> Result<u64, String> {
    program[1] = noun;
    program[2] = verb;
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
                    return Err(format!("Unexpected end of program, not enough arguments on {:?}", pc));
                }
            }
            99 => {
                break;
            }
            op@_ => {
                return Err(format!("Unknown opcode {:?} at {:?}", op, pc));
            }
        }
        pc += 4;
    }
    return Ok(program[0]);
}

fn main() {
    let mut program: Vec<_> = BufReader::new(File::open("inputs/day2.txt").unwrap()).split(b',')
        .map(|x| {
            String::from_utf8(x.unwrap()).unwrap().parse::<u64>().unwrap()
        })
        .collect();
    for noun in 0..100 {
        for verb in 0..100 {
            if let Ok(19690720) = run(program.clone(), noun, verb) {
                println!();
                println!("noun: {} verb: {} result: {}", noun, verb, 100 * noun + verb);
                return;
            }
        }
        print!("*")
    }
}