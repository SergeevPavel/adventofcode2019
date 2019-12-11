use std::collections::{HashMap, HashSet};
use std::ops::{Index, IndexMut, Rem, RemAssign};
use std::fs::read_to_string;

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

struct Robot {
    pub x: i32,
    pub y: i32,
    direction: u8,
}

impl Robot {
    fn new() -> Self {
        Robot {
            x: 0,
            y: 0,
            direction: 0
        }
    }

    fn position(&self) -> (i32, i32) {
        (self.x, self.y)
    }

    fn step(&mut self) {
        match self.direction {
            // up
            0 => self.y += 1,
            // right
            1 => self.x += 1,
            // down
            2 => self.y -= 1,
            // left
            3 => self.x -= 1,
            _ => unreachable!()
        }
    }

    fn turn_left(&mut self) {
        self.direction += 3;
        self.direction %= 4;
    }

    fn turn_right(&mut self) {
        self.direction += 1;
        self.direction %= 4;
    }
}

fn show(white_panels: &HashSet<(i32, i32)>) {
    let x0 = white_panels.iter().map(|p| p.0).min().unwrap();
    let xm = white_panels.iter().map(|p| p.0).max().unwrap();
    let y0 = white_panels.iter().map(|p| p.1).min().unwrap();
    let ym = white_panels.iter().map(|p| p.1).max().unwrap();
    for y in (y0..ym + 1).rev() {
        for x in x0..xm + 1 {
            if white_panels.contains(&(x, y)) {
                print!("#");
            } else {
                print!(".");
            }
        }
        println!();
    }

}

fn main() {
    let program: Vec<i64> = read_to_string("inputs/day11.txt").unwrap()
        .split(",")
        .map(|s| {
            let v = s.parse().unwrap_or_else(|_e| {
                println!("Unknown shit: {:?}", s);
                panic!()
            });
            v
        })
        .collect();
    let mut white_panels = HashSet::new();
    white_panels.insert((0, 0));

    let mut r = Robot::new();
    let mut c = Computer::new(&program);
    let mut colored = HashSet::new();
    while !c.halted {
        let input = if white_panels.contains(&r.position()) {
            1
        } else {
            0
        };
        let output = c.run(vec![input]).unwrap();
        if let [color, turn] = output.as_slice() {
            match color {
                0 => white_panels.remove(&r.position()),
                1 => white_panels.insert(r.position()),
                _ => unreachable!()
            };
            colored.insert(r.position());
            match turn {
                0 => r.turn_left(),
                1 => r.turn_right(),
                _ => unreachable!()
            };
            r.step();
        } else {
            panic!(format!("Unexpected output: {:?}", output));
        }
    }
    println!("Total colored: {:?}", colored.len());
    show(&white_panels);
}
