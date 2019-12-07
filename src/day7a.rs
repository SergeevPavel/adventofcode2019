use std::fs::{read_to_string};
use std::ops::{Rem};
use std::collections::HashSet;

type Program = Vec<i32>;

fn read_param(program: &Program, instr: &[i32], arg_num: u8) -> Result<i32, String> {
    assert!((arg_num as usize) < instr.len());
    let op = instr[0];
    let mode = (op / i32::pow(10, 2 + (arg_num - 1) as u32)).rem(10);
    match mode {
        0 => Ok(program[instr[arg_num as usize] as usize]),
        1 => Ok(instr[arg_num as usize]),
        m => Err(format!("Unknown mode: {:?}", m))
    }
}

fn run(mut program: Vec<i32>, mut input: Vec<i32>) -> Result<Vec<i32>, String> {
    input.reverse();
    let mut output = Vec::new();
    let mut pc = 0;
    while pc < program.len() {
        match program[pc].rem(100) {
            1 | 2 => {
                if let instr@[_, _, _, _] = &program[pc..pc + 4] {
                    let op = instr[0].rem(100);
                    let p1 = read_param(&program, instr, 1)?;
                    let p2 = read_param(&program, instr, 2)?;
                    let r = instr[3];
//                    println!("p1 {:?} p2 {:?} r {:?}", p1, p2, r);
                    match op {
                        1 => {
                            program[r as usize] = p1 + p2;
                        }
                        2 => {
                            program[r as usize] = p1 * p2;
                        }
                        _ => unreachable!()
                    }
                } else {
                    return Err(format!("Unexpected end of program, not enough arguments on {:?}", pc));
                }
                pc += 4;
            }
            3 => {
                let p1 = program[pc + 1];
                program[p1 as usize] = input.pop().ok_or("Not enough input parameters")?;
                pc += 2;
            }
            4 => {
                let out = read_param(&program, &program[pc..pc + 2], 1)?;
                output.push(out);
                println!("Out: {}", out);
                pc += 2;
            }
            5 => {
                let instr = &program[pc..pc + 3];
                let cond = read_param(&program, instr, 1)?;
                let addr = read_param(&program, instr, 2)?;
                if cond != 0 {
                    pc = addr as usize;
                } else {
                    pc += 3;
                }
            }
            6 => {
                let instr = &program[pc..pc + 3];
                let cond = read_param(&program, instr, 1)?;
                let addr = read_param(&program, instr, 2)?;
                if cond == 0 {
                    pc = addr as usize;
                } else {
                    pc += 3;
                }
            }
            7 => {
                let instr = &program[pc..pc + 4];
                let p1 = read_param(&program, instr, 1);
                let p2 = read_param(&program, instr, 2);
                let r = instr[3] as usize;
                if p1 < p2 {
                    program[r] = 1;
                } else {
                    program[r] = 0;
                }
                pc += 4;
            }
            8 => {
                let instr = &program[pc..pc + 4];
                let p1 = read_param(&program, instr, 1);
                let p2 = read_param(&program, instr, 2);
                let r = instr[3] as usize;
                if p1 == p2 {
                    program[r] = 1;
                } else {
                    program[r] = 0;
                }
                pc += 4;
            }
            99 => {
                println!("Program halt");
                break;
            }
            op@_ => {
                return Err(format!("Unknown opcode {:?} at {:?}", op, pc));
            }
        }
    }
    return Ok(output);
}

fn amplifier(initial: i32, program: &Vec<i32>, config: Vec<i32>) -> i32 {
    let mut signal = initial;
    for c in config {
        signal = *run(program.clone(), vec![c, signal]).unwrap().first().unwrap();
    }
    return signal;
}

fn permutations(elements: &mut HashSet<i32>, perm: &mut Vec<i32>, result: &mut Vec<Vec<i32>>) {
    if elements.is_empty() {
        result.push(perm.clone());
    } else {
        for e in elements.clone().iter() {
            elements.remove(e);
            perm.push(*e);
            permutations(elements, perm, result);
            perm.pop();
            elements.insert(*e);
        }
    }
}

fn main() -> Result<(), String> {
    let program: Vec<i32> = read_to_string("inputs/day7.txt").unwrap()
        .split(",")
        .map(|s| s.parse().unwrap_or_else(|_e| {
            println!("Unknown shit: {:?}", s);
            panic!()
        }))
        .collect();
    let mut variants = vec![];
    permutations(&mut (0..5).collect::<HashSet<i32>>(), &mut vec![], &mut variants);
    let mut results = vec![];
    for v in variants {
        results.push(amplifier(0, &program, v));
    }
    println!("{:?}", results.iter().max().unwrap());
    Ok(())
}