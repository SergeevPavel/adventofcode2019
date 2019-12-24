use std::collections::{HashMap, VecDeque};
use std::ops::{Index, IndexMut, Rem};
use std::fs::read_to_string;
use std::convert::TryInto;
use std::mem::swap;

struct Memory {
    inner: HashMap<i64, i64>
}

impl Memory {
    fn new() -> Self {
        Memory {
            inner: HashMap::new()
        }
    }
}

impl Index<i64> for Memory {
    type Output = i64;

    fn index(&self, index: i64) -> &Self::Output {
        self.inner.get(&index).unwrap_or(&0)
    }
}

impl IndexMut<i64> for Memory {
    fn index_mut(&mut self, index: i64) -> &mut Self::Output {
        self.inner.entry(index).or_default()
    }
}

struct Computer {
    memory: Memory,
    pc: i64,
    relative_base: i64,
    pub halted: bool,
}

impl Computer {
    fn new(program: &Vec<i64>) -> Self {
        let mut memory = Memory::new();
        program.iter().enumerate().for_each(|(idx, w)| {
            memory[idx as i64] = *w as i64;
        });
        Computer {
            memory,
            pc: 0,
            relative_base: 0,
            halted: false,
        }
    }

    fn decode_param(&self, instr_offset: i64, arg_num: u8) -> Result<&i64, String> {
        let op = self.memory[instr_offset];
        let arg = &self.memory[instr_offset + arg_num as i64];
        let mode = (op / i32::pow(10, 2 + (arg_num - 1) as u32) as i64).rem(10);
        match mode {
            0 => Ok(&self.memory[*arg]),
            1 => Ok(&arg),
            2 => Ok(&self.memory[self.relative_base + *arg]),
            m => Err(format!("Unknown mode: {:?}", m))
        }
    }

    fn decode_param_mut(&mut self, instr_offset: i64, arg_num: u8) -> Result<&mut i64, String> {
        let op = self.memory[instr_offset];
        let arg = self.memory[instr_offset + arg_num as i64];
        let mode = (op / i32::pow(10, 2 + (arg_num - 1) as u32) as i64).rem(10);
        match mode {
            0 => Ok(&mut self.memory[arg]),
            1 => Err("Can't write to cmd param".to_string()),
            2 => Ok(&mut self.memory[self.relative_base + arg]),
            m => Err(format!("Unknown mode: {:?}", m))
        }
    }

    fn run(&mut self, mut input: Vec<i64>) -> Result<Vec<i64>, String> {
        input.reverse();
        let mut output = Vec::new();
        loop {
            match self.memory[self.pc].rem(100) {
                1 | 2 => {
                    let op = self.memory[self.pc];
                    let p1 = *self.decode_param(self.pc, 1)?;
                    let p2 = *self.decode_param(self.pc, 2)?;
                    let r = self.decode_param_mut(self.pc, 3)?;
                    match op.rem(100) {
                        1 => {
                            *r = p1 + p2;
                        }
                        2 => {
                            *r = p1 * p2;
                        }
                        _ => unreachable!()
                    }
                    self.pc += 4;
                }
                3 => {
                    if input.len() > 0 {
                        let p = self.decode_param_mut(self.pc, 1)?;
                        *p = input.pop().unwrap();
                        self.pc += 2;
                    } else {
                        return Ok(output);
                    }
                }
                4 => {
                    let out = *self.decode_param(self.pc, 1)?;
                    output.push(out);
                    self.pc += 2;
                }
                5 => {
                    let cond = *self.decode_param(self.pc, 1)?;
                    let addr = *self.decode_param(self.pc, 2)?;
                    if cond != 0 {
                        self.pc = addr;
                    } else {
                        self.pc += 3;
                    }
                }
                6 => {
                    let cond = *self.decode_param(self.pc, 1)?;
                    let addr = *self.decode_param(self.pc, 2)?;
                    if cond == 0 {
                        self.pc = addr;
                    } else {
                        self.pc += 3;
                    }
                }
                7 => {
                    let p1 = *self.decode_param(self.pc, 1)?;
                    let p2 = *self.decode_param(self.pc, 2)?;
                    let r = self.decode_param_mut(self.pc, 3)?;
                    if p1 < p2 {
                        *r = 1;
                    } else {
                        *r = 0;
                    }
                    self.pc += 4;
                }
                8 => {
                    let p1 = *self.decode_param(self.pc, 1)?;
                    let p2 = *self.decode_param(self.pc, 2)?;
                    let r = self.decode_param_mut(self.pc, 3)?;
                    if p1 == p2 {
                        *r = 1;
                    } else {
                        *r = 0;
                    }
                    self.pc += 4;
                }
                9 => {
                    let p = *self.decode_param(self.pc, 1)?;
                    self.relative_base += p;
                    self.pc += 2;
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
    }
}

fn main() {
    let program: Vec<i64> = read_to_string("inputs/day23.txt").unwrap()
        .split(",")
        .map(|s| {
            let v = s.parse().unwrap_or_else(|_e| {
                println!("Unknown shit: {:?}", s);
                panic!()
            });
            v
        })
        .collect();
    let count: i32 = 50;
    let mut inbox: Vec<Vec<i64>> = vec![vec![]; count as usize];
    let mut computers: Vec<_> =(0..count).map(|i| {
        let c = Computer::new(&program);
        inbox[i as usize].push(i.try_into().unwrap());
        return c;
    }).collect();

    let mut nat = None;
    loop {
        for i in 0..count {
            let c = &mut computers[i as usize];
            if !c.halted {
                let mut input = vec![];
                if !inbox[i as usize].is_empty() {
                    swap(&mut input, &mut inbox[i as usize]);
                } else {
                    input.push(-1);
                }
                let output = c.run(input).unwrap();
//                println!("Out: {:?}", output);
                let msgs = output.chunks_exact(3);
                for m in msgs {
                    if let &[dst, x, y] = m {
                        println!("Send from {:>3} at {:>5} X = {:>10} Y = {:>10}", i, dst, x, y);
                        if dst < count as i64 && dst >= 0 {
                            inbox[dst as usize].push(x);
                            inbox[dst as usize].push(y)
                        } else if dst == 255 {
                            nat = Some((x, y));
                        }
                    } else {
                        unreachable!();
                    }
                }
//                assert!(msgs.remainder().is_empty());
            }
        }
        if inbox.iter().all(|inb| inb.is_empty()) && nat.is_some() {
            let (x, y) = nat.take().unwrap();
            inbox[0].push(x);
            inbox[0].push(y);
            println!("===============================RELOAD===============================");
            println!("Y = {}", y);
        }
    }
}