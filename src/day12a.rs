use std::fs::read_to_string;
use regex::Regex;
use std::cmp::Ordering;

const DIMENSION: usize = 3;

#[derive(Debug, Copy, Clone)]
struct Moon {
    position: [i32; DIMENSION],
    velocity: [i32; DIMENSION]
}

fn main() {
    let re = Regex::new(r"<x=(.*), y=(.*), z=(.*)>").unwrap();
    let mut moons: Vec<_> = read_to_string("inputs/day12.txt")
        .unwrap()
        .lines()
        .map(|l| {
            let captures = re.captures(l).expect("Malformed input");
            let mut position = [0; DIMENSION];
            for c in 0..DIMENSION {
                position[c] = captures[1 + c].parse().unwrap()
            }
            Moon {
                position,
                velocity: [0; DIMENSION]
            }
        }).collect();
    for step in 0..1000 {
        // Update velocity
        for i in 0..moons.len() {
            for j in (i + 1)..moons.len() {
                for c in 0..DIMENSION {
                    match moons[i].position[c].cmp(&moons[j].position[c]) {
                        Ordering::Less => {
                            moons[i].velocity[c] += 1;
                            moons[j].velocity[c] -= 1;
                        },
                        Ordering::Greater => {
                            moons[i].velocity[c] -= 1;
                            moons[j].velocity[c] += 1;
                        },
                        Ordering::Equal => {},
                    }
                }

            }
        }
        // Update positions
        for i in 0..moons.len() {
            for c in 0..DIMENSION {
                moons[i].position[c] += moons[i].velocity[c];
            }
        }
    }
    // Calculate energy
    let result: i32 = moons.iter().map(|m| {
        m.velocity.iter().map(|x| x.abs()).sum::<i32>() * m.position.iter().map(|x| x.abs()).sum::<i32>()
    }).sum();
    println!("Result: {:?}", result);
}