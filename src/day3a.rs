use std::fs::File;
use std::io::{BufReader, BufRead};
use std::collections::{HashSet, HashMap};

#[derive(Debug, Copy, Clone)]
enum Direction {
    Up,
    Down,
    Left,
    Right
}

#[derive(Debug, Copy, Clone)]
struct Move {
    d: Direction,
    l: u64,
}

#[derive(Hash, PartialEq, Eq, Debug, Copy, Clone)]
struct Point {
    x: i64,
    y: i64
}

type Route = Vec<Move>;

fn pave_wire(route: &Vec<Move>) -> HashSet<Point> {
    let mut field = HashSet::new();
    let mut current_point = Point { x: 0, y: 0 };
    for m in route {
        for _i in 0..m.l {
            match m.d {
                Direction::Up => {
                    current_point.y += 1;
                },
                Direction::Down => {
                    current_point.y -= 1;
                },
                Direction::Left => {
                    current_point.x -= 1;
                },
                Direction::Right => {
                    current_point.x += 1;
                },
            }
            field.insert(current_point);
        }
    }
    return field;
}

fn read_routes() -> Vec<Route> {
    let mut routes = Vec::new();
    for line in BufReader::new(File::open("inputs/day3.txt").unwrap()).lines() {
        let line = line.unwrap();
        let route: Vec<_> = line.split(",").map(|s| {
            let direction = match &s[0..1] {
                "R" => Direction::Right,
                "L" => Direction::Left,
                "U" => Direction::Up,
                "D" => Direction::Down,
                _ => panic!("Unknown type of direction")
            };
            Move {
                d: direction,
                l: s[1..].parse().unwrap()
            }
        }).collect();
        routes.push(route);
    }
    routes
}

fn main() {
    if let [r1, r2] = &read_routes()[0..2] {
        let f1 = pave_wire(r1);
        let f2 = pave_wire(r2);
        let p = f1.intersection(&f2).min_by_key(|p| { p.x.abs() + p.y.abs() }).expect("No point");
        println!("{:?} Result: {:?}", p, p.x.abs() + p.y.abs());
    } else {
        panic!("Need more routes");
    }
}