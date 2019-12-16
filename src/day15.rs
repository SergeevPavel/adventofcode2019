use std::collections::{HashMap, HashSet, VecDeque};
use std::ops::{Index, IndexMut, Rem, RemAssign};
use std::fs::read_to_string;
use std::time::Duration;
use std::io;
use termion::raw::IntoRawMode;
use termion::input::TermRead;
use std::io::Write;
use termion::event::Key;
use regex::internal::Program;
use std::cmp::Ordering;

#[derive(Clone)]
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

#[derive(Clone)]
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

#[derive(Debug, Hash, Eq, PartialEq, Copy, Clone)]
struct Point {
    x: i64,
    y: i64
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum CellTypes {
    Start,
    Empty,
    Wall,
    OxSystem
}

struct Field {
    bot_position: Point,
    cells: HashMap<Point, CellTypes>
}

impl Field {
    fn new() -> Self {
        let mut cells: HashMap<Point, CellTypes> = Default::default();
        let robot = Point { x: 0, y: 0 };
        cells.insert(robot.clone(), CellTypes::Start);
        Field {
            bot_position: robot,
            cells
        }
    }
}

fn draw(field: &Field, out: &mut dyn Write) {
    let base_x = 64;
    let base_y = 32;
    let to_term = |p: &Point| {
        termion::cursor::Goto((base_x + p.x) as u16, (base_y + p.y) as u16)
    };
    write!(out, "{}", termion::clear::All);
    field.cells.iter().for_each(|(p, t)| {
        match t {
            CellTypes::Start => write!(out, "{}*.", to_term(p)),
            CellTypes::Empty => write!(out, "{}.", to_term(p)),
            CellTypes::Wall => write!(out, "{}#", to_term(p)),
            CellTypes::OxSystem => write!(out, "{}@", to_term(p)),
        };
    });
    write!(out, "{}D{}", to_term(&field.bot_position), termion::cursor::Goto(1, 1));
    out.flush().unwrap();
//    std::thread::sleep(Duration::from_millis(30));
}

struct Droid {
    field: Field,
    computer: Computer
}

#[derive(Debug, Copy, Clone)]
enum Directions {
    Up,
    Down,
    Left,
    Right
}

impl Directions {
    fn inv(&self) -> Self {
        match self {
            Up => Down,
            Down => Up,
            Left => Right,
            Right => Left,
        }
    }

    fn do_move(&self, mut p: Point) -> Point {
        match self {
            Directions::Up => {
                p.y -= 1;
            },
            Directions::Down => {
                p.y += 1;
            },
            Directions::Left => {
                p.x -= 1;
            },
            Directions::Right => {
                p.x += 1;
            },
        }
        p
    }
}

impl Droid {
    fn step(&mut self, direction: Directions) -> bool {
        if !self.computer.halted {
            let encoded_input = match direction {
                Directions::Up    => 1,
                Directions::Down  => 2,
                Directions::Left  => 3,
                Directions::Right => 4,
            };
            let next_point = self.next_point(direction);
            let output = self.computer.run(vec![encoded_input]).unwrap();
            match output[0] {
                0 => {
                    self.field.cells.entry(next_point).or_insert(CellTypes::Wall);
                    return false;
                }
                1 => {
                    self.field.cells.entry(next_point).or_insert(CellTypes::Empty);
                    self.field.bot_position = next_point;
                    return true;
                }
                2 => {
                    self.field.cells.entry(next_point).or_insert(CellTypes::OxSystem);
                    self.field.bot_position = next_point;
                    return true;
                }
                _ => {
                    return false;
                }
            }
        }
        return false;
    }

    fn next_point(&self, direction: Directions) -> Point {
        direction.do_move(self.field.bot_position)
    }
}

use Directions::*;
use crate::CellTypes::*;

fn ox_system_distance(field: &Field) -> i64 {
    let (start, _) = field.cells.iter().filter(|(p, ct)| **ct == Start).next().unwrap();
    let mut q = VecDeque::new();
    let mut visited = HashSet::new();
    visited.insert(*start);
    q.push_back((*start, 0));
    while !q.is_empty() {
        let (current, dst) = q.pop_front().unwrap();
        for dir in [Up, Down, Left, Right].iter() {
            let next = dir.do_move(current);
            match field.cells.get(&next).unwrap() {
                Empty => {
                    if !visited.contains(&next) {
                        q.push_back((next, dst + 1));
                        visited.insert(next);
                    }
                },
                OxSystem => {
                    return dst + 1;
                },
                _ => {}
            };
        }
    }
    return -1;
}

fn filling_time(field: &Field) -> i64 {
    let (start, _) = field.cells.iter().filter(|(p, ct)| **ct == OxSystem).next().unwrap();
    let mut q = VecDeque::new();
    let mut visited = HashSet::new();
    visited.insert(*start);
    q.push_back((*start, 0));
    let mut max_dist = 0;
    while !q.is_empty() {
        let (current, dst) = q.pop_front().unwrap();
        for dir in [Up, Down, Left, Right].iter() {
            let next = dir.do_move(current);
            match field.cells.get(&next).unwrap() {
                Empty | Start => {
                    if !visited.contains(&next) {
                        max_dist = max_dist.max(dst + 1);
                        q.push_back((next, dst + 1));
                        visited.insert(next);
                    }
                },
                _ => {}
            };
        }
    }
    return max_dist;
}

fn main() {
    let mut program: Vec<i64> = read_to_string("inputs/day15.txt").unwrap()
        .split(",")
        .map(|s| {
            let v = s.parse().unwrap_or_else(|_e| {
                println!("Unknown shit: {:?}", s);
                panic!()
            });
            v
        })
        .collect();

    let mut stdout = io::stdout().into_raw_mode().unwrap();
    let mut stdin = termion::async_stdin().keys();

    let mut droid = Droid {
        field: Field::new(),
        computer: Computer::new(&program)
    };

    let mut stack = vec![];
    let mut rest_directions = vec![Up, Down, Left, Right];

    loop {
        while !rest_directions.is_empty() {
            let dir = rest_directions.pop().unwrap();
            let next_point = droid.next_point(dir);
            if !droid.field.cells.contains_key(&next_point) {
                if droid.step(dir) {
//                    draw(&droid.field, &mut stdout);
                    stack.push((rest_directions.clone(), dir));
                    rest_directions = vec![Up, Down, Left, Right];
                }
            }
        }
        if let Some((back_rest_directions, dir)) = stack.pop() {
            rest_directions = back_rest_directions;
            droid.step(dir.inv());
//            draw(&droid.field, &mut stdout);
        } else {
            break;
        }
    }
    writeln!(stdout, "Distance: {:?}", ox_system_distance(&droid.field));
    writeln!(stdout, "Filling time: {:?}", filling_time(&droid.field));

//    loop {
//        match stdin.by_ref().filter_map(|ch| ch.ok()).last() {
//            Some(Key::Up)    => Some(Directions::Up),
//            Some(Key::Down)  => Some(Directions::Down),
//            Some(Key::Left)  => Some(Directions::Left),
//            Some(Key::Right) => Some(Directions::Right),
//            Some(Key::Esc)   => {
//                break;
//            },
//            _ => None
//        }.map(|i| {
//            droid.step(i);
//        });
//
//        draw(&droid.field, &mut stdout);
//        stdout.flush().unwrap();
//        std::thread::sleep(Duration::from_millis(60));
//    }
}
