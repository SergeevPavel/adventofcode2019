use std::collections::{HashMap, HashSet};
use std::ops::{Index, IndexMut, Rem, RemAssign};
use std::fs::read_to_string;
use std::time::Duration;
use std::io;
use termion::raw::IntoRawMode;
use termion::input::TermRead;
use std::io::Write;
use termion::event::Key;


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

struct Screen {
    pixels: HashMap<(u64, u64), u8>,
    max_x: u64,
    max_y: u64,
    score: u64
}

impl Screen {
    fn new() -> Self {
        Screen {
            pixels: Default::default(),
            max_x: 0,
            max_y: 0,
            score: 0
        }
    }

    fn update(&mut self, output: &Vec<i64>) {
        output.chunks(3).for_each(|t| {
//        println!("Tile: {:?}", t);
            if t[0] == -1 && t[1] == 0 {
                self.score = t[2] as u64;
            } else {
                self.max_x = self.max_x.max(t[0] as u64);
                self.max_y = self.max_y.max(t[1] as u64);
                self.pixels.insert((t[0] as u64, t[1] as u64), t[2] as u8);
            }
        });
    }

    fn draw(&self, out: &mut dyn Write) {
        write!(out, "{}{}", termion::clear::All, termion::cursor::Goto(1, 1));
        for y in 0..(self.max_y + 1) {
            write!(out, "{}", termion::cursor::Goto(1, y as u16));
            for x in 0..(self.max_x + 1) {
                match self.pixels.get(&(x, y)) {
                    Some(0) => write!(out, " ").unwrap(),
                    Some(1) => write!(out, "#").unwrap(),
                    Some(2) => write!(out, "X").unwrap(),
                    Some(3) => write!(out, "=").unwrap(),
                    Some(4) => write!(out, "0").unwrap(),
                    s => unreachable!(format!("S: {:?}", s))
                }
            }
        }
        write!(out, "Score: {:?}", self.score);
    }
}

fn main() {
    let mut program: Vec<i64> = read_to_string("inputs/day13.txt").unwrap()
        .split(",")
        .map(|s| {
            let v = s.parse().unwrap_or_else(|_e| {
                println!("Unknown shit: {:?}", s);
                panic!()
            });
            v
        })
        .collect();
    let mut c = Computer::new(&program);
    let mut screen = Screen::new();

    let mut stdout = io::stdout().into_raw_mode().unwrap();
    let mut stdin = termion::async_stdin().keys();

    while !c.halted {
        let input = match stdin.by_ref().filter_map(|ch| ch.ok()).last() {
            Some(Key::Left) => -1,
            Some(Key::Right) => 1,
            _ => 0
        };
        let output = c.run(vec![input]).unwrap();
        screen.update(&output);
        screen.draw(&mut stdout);
        stdout.flush().unwrap();
        std::thread::sleep(Duration::from_millis(1000));
    }
}
