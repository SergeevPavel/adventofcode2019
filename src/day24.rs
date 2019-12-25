use std::fs::read_to_string;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::fmt::{Debug, Formatter, Error};

#[derive(Clone)]
struct Field {
    cells: HashMap<(i32, i32, i32), bool>,
    width: i32,
    height: i32
}

impl Field {
    fn min_level(&self) -> i32 {
        *self.cells.iter().map(|((_, _, l), _)| l).min().unwrap()
    }

    fn max_level(&self) -> i32 {
        *self.cells.iter().map(|((_, _, l), _)| l).max().unwrap()
    }

    fn neighbors(&self, i: i32, j: i32, l: i32) -> i32 {
        let mut result = 0;
        for &(y, x) in &[(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)] {
            if y < 0 {
                result += self.ati(1, 2, l - 1);
            } else if y > 4 {
                result += self.ati(3, 2, l - 1);
            } else if x < 0 {
                result += self.ati(2, 1, l - 1);
            } else if x > 4 {
                result += self.ati(2, 3, l - 1);
            } else if x == 2 && y == 2 {
                match (i, j) {
                    (1, 2) => {
                        let y = 0;
                        for x in 0..5 {
                            result += self.ati(y, x, l + 1);
                        }
                    }
                    (3, 2) => {
                        let y = 4;
                        for x in 0..5 {
                            result += self.ati(y, x, l + 1);
                        }
                    }
                    (2, 1) => {
                        let x = 0;
                        for y in 0..5 {
                            result += self.ati(y, x, l + 1);
                        }
                    }
                    (2, 3) => {
                        let x = 4;
                        for y in 0..5 {
                            result += self.ati(y, x, l + 1);
                        }
                    }
                    _ => unreachable!()
                }
            } else if self.at(y, x, l) {
                result += 1;
            };
        }
        return result;
    }

    fn ati(&self, i: i32, j: i32, l: i32) -> i32 {
        if *self.cells.get(&(i, j, l)).unwrap_or(&false) {
            1
        } else {
            0
        }
    }

    fn at(&self, i: i32, j: i32, l: i32) -> bool {
        *self.cells.get(&(i, j, l)).unwrap_or(&false)
    }

    fn set(&mut self, i: i32, j: i32, l: i32, v: bool) {
        if !(i == 2 && j == 2) {
            self.cells.insert((i, j, l), v);
        }
    }
}

impl Debug for Field {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        for l in self.min_level()..(self.max_level() + 1) {
            println!("Level: {}", l);
            for i in 0..self.height {
                for j in 0..self.width {
                    if i == 2 && j == 2 {
                        write!(f, "?")?;
                    } else if self.at(i, j, l) {
                        write!(f, "#")?;
                    } else {
                        write!(f, ".")?;
                    }
                }
                writeln!(f, "")?;
            }
        }
        Ok(())
    }
}

fn read_field(s: String) -> Field {
    let input = read_to_string("inputs/day24.txt").unwrap();
    let height = input.lines().count().try_into().unwrap();
    let width = input.lines().next().unwrap().len().try_into().unwrap();
    let mut cells = HashMap::new();
    for (i, s) in input.lines().enumerate() {
        for (j, c) in s.chars().enumerate() {
            let i = i.try_into().unwrap();
            let j = j.try_into().unwrap();
            if c == '#' {
                cells.insert((i, j, 0), true);
            } else if c == '.' {
                cells.insert((i, j, 0), false);
            }
        }
    }
    Field {
        cells,
        width,
        height
    }
}

fn evolve(field: &Field) -> Field {
    let mut next_filed = field.clone();
    for l in (field.min_level() - 1)..(field.max_level() + 2) {
        for i in 0..field.height {
            for j in 0..field.width {
                if i == 2 && j == 2 {
                    continue;
                }
                if field.at(i, j, l) {
//                    println!("i = {:?} j = {} l = {} Ns = {}", i, j, l, field.neighbors(i, j, l));
                    if field.neighbors(i, j, l) != 1 {
                        next_filed.set(i, j, l, false);
                    }
                } else {
                    let neighbors = field.neighbors(i, j, l);
                    if neighbors == 1 || neighbors == 2 {
                        next_filed.set(i, j, l, true);
                    }
                }
            }
        }
    }
    next_filed
}

//fn rating(field: &Field) -> i64 {
//    let mut step = 1;
//    let mut rating = 0;
//    for i in 0..field.height {
//        for j in 0..field.width {
//            if field.at(i, j) {
//                rating += step;
//            }
//            step *= 2;
//        }
//    }
//    rating
//}

fn main() {
    let mut field = read_field("inputs/day24.txt".to_string());
    for m in 0..200 {
        field = evolve(&field);
//        println!("Minute: {:?}", m);
//        println!("{:?}", field);
//        println!("===============");
    }
    println!("{:?}", field.cells.iter().filter(|(_, v)| **v).count());
}