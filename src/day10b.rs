use std::fs::read_to_string;
use std::ops::{Rem, Div, AddAssign};
use std::collections::{HashMap};
use std::cmp::Ordering;

type Point = (i32, i32);

fn gcd(a: u32, b: u32) -> u32 {
    let mut x = a.max(b);
    let mut y = a.min(b);
    while y > 0 {
        let r = x.rem(y);
        x = y;
        y = r;
    }
    x
}

#[derive(Debug)]
struct Target {
    p: Point,
    distance: u32,
}

fn targets((ox, oy): Point, asteroids: &Vec<Point>) -> HashMap<Point, Vec<Target>> {
    let mut targets = HashMap::new();
    for &(ax, ay) in asteroids {
        let dx = ax - ox;
        let dy = ay - oy;
        if (dx, dy) != (0, 0) {
            let div = gcd(dx.abs() as u32, dy.abs() as u32);
            targets
                .entry((dx.div(div as i32), dy.div(div as i32)))
                .or_insert(vec![])
                .push(Target {
                    p: (ax, ay),
                    distance: div
                });
        }
    }
    targets.iter_mut().map(|(_, ts)| {
        ts.sort_by_key(|t| -(t.distance as i32))
    });
    targets
}

fn main() {
    let input = read_to_string("inputs/day10.txt").unwrap();
    let mut asteroids = vec![];
    for (y, l) in input.lines().enumerate() {
        for (x, c) in l.chars().enumerate() {
            if c == '#' {
                asteroids.push((x as i32, y as i32));
            }
        }
    }
    let x0 = 20;
    let y0 = 19;

    let mut targets: Vec<_> = targets((x0, y0), &asteroids)
        .drain()
        .map(|((x, y), ts)| {
            let x = x as f64;
            let y = y as f64;
            let o = if x >= 0.0 {
                (0, (-y / (x.powf(2.0) + y.powf(2.0)).sqrt()).acos())
            } else {
                (1, (y / (x.powf(2.0) + y.powf(2.0)).sqrt()).acos())
            };
            ((x, y), ts, o)
        })
        .collect();
    targets.sort_by(|(_, _, o1), (_, _, o2)| {
        if o1.0 < o2.0 {
            Ordering::Less
        } else if o1.0 > o2.0 {
            Ordering::Greater
        } else {
            o1.1.partial_cmp(&o2.1).unwrap()
        }
    });
    println!("{:?}", targets.iter().map(|(dir, _, _)| dir).collect::<Vec<_>>());
    let mut p = 0;
    let mut cnt = 0;
    while !targets.iter().all(|t| t.1.is_empty()) {
        if !targets[p].1.is_empty() {
            let killed = targets[p].1.pop().unwrap().p;
            p += 1;
            p %= targets.len();
            cnt += 1;
            println!("Cnt: {:?} Killed: {:?}", cnt, killed);
        } else {
            p += 1;
            p %= targets.len();
        }
    }
}

#[test]
fn gcd_test() {
    assert_eq!(gcd(21, 7), 7);
    assert_eq!(gcd(30, 12), 6);
}