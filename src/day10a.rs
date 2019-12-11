use std::fs::read_to_string;
use std::ops::{Rem, Div};
use std::collections::HashSet;

fn gcd(a: i32, b: i32) -> i32 {
    let mut x = a.max(b);
    let mut y = a.min(b);
    while y > 0 {
        let r = x.rem(y);
        x = y;
        y = r;
    }
    x
}

fn count_visible((ox, oy): (i32, i32), asteroids: &Vec<(i32, i32)>) -> u32 {
    let mut directions = HashSet::new();
    for &(ax, ay) in asteroids {
        let dx = ax - ox;
        let dy = ay - oy;
        if (dx, dy) != (0, 0) {
            let div = gcd(dx.abs(), dy.abs());
            directions.insert((dx.div(div), dy.div(div)));
        }
    }
    directions.len() as u32
}

fn main() {
    let input = read_to_string("inputs/day10.txt").unwrap();
    let mut asteroids = vec![];
    for (y, l) in input.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            if c == '#' {
                asteroids.push((x as i32, -(y as i32)));
            }
        }
    }
    let result = asteroids.iter().map(|a| {
        (*a, count_visible(*a, &asteroids))
    }).max_by_key(|(_, c)| *c);
    println!("{:?}", result);
}

#[test]
fn gcd_test() {
    assert_eq!(gcd(21, 7), 7);
    assert_eq!(gcd(30, 12), 6);
}