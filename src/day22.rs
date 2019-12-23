use std::fs::read_to_string;

#[derive(Debug, Copy, Clone)]
enum Techniques {
    NewStack,
    Cut(i32),
    Increment(i32)
}

impl Techniques {
    fn apply(&self, deck: &[i32], out: &mut [i32]) {
        match self {
            NewStack => {
                out.copy_from_slice(deck);
                out.reverse();
            },
            Cut(offset) => {
                let offset = if *offset >= 0 {
                    *offset
                } else {
                    *offset + deck.len() as i32
                };
                let (target_left, target_right) = out.split_at_mut(deck.len() - offset as usize);
                let (left, right) = deck.split_at(offset as usize);
                target_left.copy_from_slice(right);
                target_right.copy_from_slice(left);
            },
            Increment(step) => {
                let mut position = 0;
                let deck_len = deck.len();
                for x in deck {
                    out[position] = *x;
                    position += *step as usize;
                    position %= deck_len;
                }
            },
        }
    }
}

use Techniques::*;
use std::mem::swap;
use std::ops::Rem;
use std::convert::TryInto;
use std::collections::HashMap;

fn solve(deck_size: i32, input: &Vec<Techniques>) {
    let mut deck: Vec<_> = (0..deck_size).collect();
    println!("{:?}", deck);
    let mut out = vec![0; deck.len()];
    for i in input {
        i.apply(&deck, &mut out);
        swap(&mut deck, &mut out);
        println!("{:?}", deck);
    }
    println!("{:?}", deck.iter().position(|v| *v == 2019));
}

fn solve1(deck_size: i32, input: &Vec<Techniques>) {
    let mut x = 2019;
    for i in input {
        match i {
            NewStack => {
                x = (deck_size - 1) - x;
            },
            &Cut(k) => {
                x = (x - k + deck_size) % deck_size;
            },
            Increment(k) => {
                x = (k * x) % deck_size;
            },
        }
    }
    print!("{:?} ", x);
}

fn bezout(a: i64, b: i64) -> (i64, i64) {
    if a == 0 {
        return (0, 1);
    } else {
        let (x1, y1) = bezout(b % a, a);
        let x = y1 - (b / a) * x1;
        let y = x1;
        return (x, y)
    }
}

fn inv(k: i64, p: i64) -> i64 {
    let (a, _) = bezout(k, p);
    let result = (a + p) % p;
//    println!("For {:?} inv: {:?}", k, result);
    return result;
}

fn unshuffle(deck_size: i64, mut y: i64, input: &Vec<Techniques>) -> i64 {
    for i in input.iter().rev() {
        match i {
            NewStack => {
                y = (deck_size - 1) - y;
            },
            &Cut(k) => {
                y = (y + k as i64 + deck_size) % deck_size;
            },
            Increment(k) => {
                y = ((y as i128) * (inv(*k as i64, deck_size) as i128) % (deck_size as i128)).try_into().unwrap();
            },
        }
    }
    return y;
}

fn main() {
    let input: Vec<_> = read_to_string("inputs/day22.txt").unwrap()
        .lines()
        .map(|s| {
            if s.starts_with("deal into new stack") {
                NewStack
            } else if s.starts_with("cut") {
                Cut(s.split_at(3).1.trim().parse().unwrap())
            } else if s.starts_with("deal with increment") {
                Increment(s.split_at(19).1.trim().parse().unwrap())
            } else {
                unreachable!()
            }
        })
        .collect();
    let deck_size = 119315717514047;
    let mut y = 2020;
    let iteration_count: i64 = 101741582076661;
    let mut i = 0;
    loop {
        let x = unshuffle(deck_size, y, &input);
        if x == y {
            println!("X: {:?}", x);
            break;
        } else {
            if i % 1000000 == 0 {
                println!("I: {:?} Y: {:?}", i, y);
            }
        }
        y = x;
        i += 1;
        if i > iteration_count {
            break;
        }
    };
    println!("End of iteration: {:?}", y);
}

#[test]
fn test_bezout1() {
    let p = 119315717514047;
    let k = 42;
    let (a, b) = bezout(k, p);
    assert_eq!(a * k + b * p, 1);
}

#[test]
fn test_bezout2() {
    let p = 119315717514047;
    let k = 123;
    let (a, b) = bezout(k, p);
    assert_eq!(a * k + b * p, 1);
}

#[test]
fn test_inv1() {
    let p = 119315717514047;
    let k = 123;
    let ik = inv(k, p);
    assert_eq!(k * ik % p, 1);
}

#[test]
fn test_inv2() {
    let p = 119315717514047;
    let k = 345;
    let ik = inv(k, p);
    assert_eq!(k * ik % p, 1);
}

#[test]
fn test_inv3() {
    let p = 119315717514047;
    let k = 3;
    let ik = inv(k, p);
    assert_eq!(k * ik % p, 1);
}