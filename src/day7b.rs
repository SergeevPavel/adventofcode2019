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

struct Amplifier {
    program: Program,
    pc: usize,
    pub halted: bool,
}

impl Amplifier {
    fn new(program: Program) -> Self {
        Amplifier {
            program,
            pc: 0,
            halted: false,
        }
    }

    fn run(&mut self, mut input: Vec<i32>) -> Result<Vec<i32>, String> {
        input.reverse();
        let mut output = Vec::new();
        while self.pc < self.program.len() {
            match self.program[self.pc].rem(100) {
                1 | 2 => {
                    if let instr@[_, _, _, _] = &self.program[self.pc..self.pc + 4] {
                        let op = instr[0].rem(100);
                        let p1 = read_param(&self.program, instr, 1)?;
                        let p2 = read_param(&self.program, instr, 2)?;
                        let r = instr[3];
//                    println!("p1 {:?} p2 {:?} r {:?}", p1, p2, r);
                        match op {
                            1 => {
                                self.program[r as usize] = p1 + p2;
                            }
                            2 => {
                                self.program[r as usize] = p1 * p2;
                            }
                            _ => unreachable!()
                        }
                    } else {
                        return Err(format!("Unexpected end of program, not enough arguments on {:?}", self.pc));
                    }
                    self.pc += 4;
                }
                3 => {
                    if input.len() > 0 {
                        let p1 = self.program[self.pc + 1];
                        self.program[p1 as usize] = input.pop().unwrap();
                        self.pc += 2;
                    } else {
                        return Ok(output);
                    }
                }
                4 => {
                    let out = read_param(&self.program, &self.program[self.pc..self.pc + 2], 1)?;
                    output.push(out);
                    println!("Out: {}", out);
                    self.pc += 2;
                }
                5 => {
                    let instr = &self.program[self.pc..self.pc + 3];
                    let cond = read_param(&self.program, instr, 1)?;
                    let addr = read_param(&self.program, instr, 2)?;
                    if cond != 0 {
                        self.pc = addr as usize;
                    } else {
                        self.pc += 3;
                    }
                }
                6 => {
                    let instr = &self.program[self.pc..self.pc + 3];
                    let cond = read_param(&self.program, instr, 1)?;
                    let addr = read_param(&self.program, instr, 2)?;
                    if cond == 0 {
                        self.pc = addr as usize;
                    } else {
                        self.pc += 3;
                    }
                }
                7 => {
                    let instr = &self.program[self.pc..self.pc + 4];
                    let p1 = read_param(&self.program, instr, 1);
                    let p2 = read_param(&self.program, instr, 2);
                    let r = instr[3] as usize;
                    if p1 < p2 {
                        self.program[r] = 1;
                    } else {
                        self.program[r] = 0;
                    }
                    self.pc += 4;
                }
                8 => {
                    let instr = &self.program[self.pc..self.pc + 4];
                    let p1 = read_param(&self.program, instr, 1);
                    let p2 = read_param(&self.program, instr, 2);
                    let r = instr[3] as usize;
                    if p1 == p2 {
                        self.program[r] = 1;
                    } else {
                        self.program[r] = 0;
                    }
                    self.pc += 4;
                }
                99 => {
                    println!("Program halt");
                    self.halted = true;
                    return Ok(output);
                }
                op@_ => {
                    return Err(format!("Unknown opcode {:?} at {:?}", op, self.pc));
                }
            }
        }
        return Err("No halt command in the end of program".to_string());
    }
}

fn amplifier(initial: i32, program: &Program, config: Vec<i32>) -> i32 {
    let mut amplifiers = vec![];
    for c in config {
        let mut a = Amplifier::new(program.clone());
        let result = a.run(vec![c]).unwrap();
        println!("Initialize amplifier with {:?} result: {:?}", c, result);
        amplifiers.push(a);
    }
    let mut signal = initial;
    while amplifiers.iter().all(|a| !a.halted) {
        for a in &mut amplifiers {
            let output = a.run(vec![signal]).unwrap();
            signal = *output.first().expect("No input in amplifier");
        }
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
    let program: Program = read_to_string("inputs/day7.txt").unwrap()
        .split(",")
        .map(|s| s.parse().unwrap_or_else(|_e| {
            println!("Unknown shit: {:?}", s);
            panic!()
        }))
        .collect();
    let mut variants = vec![];
    permutations(&mut (5..10).collect::<HashSet<i32>>(), &mut vec![], &mut variants);
    let mut results = vec![];
    for v in variants {
        results.push(amplifier(0, &program, v));
    }
    println!("{:?}", results.iter().max().unwrap());
    Ok(())
}