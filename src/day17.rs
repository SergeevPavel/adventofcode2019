mod computer;
use computer::*;
use std::convert::TryInto;
use std::collections::HashMap;

fn solve1(field: HashMap<(i64, i64), char>) {
    let mut result = 0;
    for (&(x, y), &ch) in &field {
        if ch == '#' {
            let neighbours_count = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)].iter()
                .filter_map(|neighbour| field.get(neighbour))
                .filter(|ch| **ch == '#')
                .count();
            if neighbours_count == 4 {
                result += x * y;
            }
        }
    }
    println!("{:?}", result);
}

fn next_step(droid: char) ->  (i64, i64) {
    match droid {
        '^' => (0, -1),
        '>' => (1, 0),
        '<' => (-1, 0),
        'v' => (0, 1),
        _   => unreachable!()
    }
}

#[derive(Copy, Clone, Debug)]
enum Command {
    R,
    L,
    F(i64)
}

fn path(field: &Field) -> Vec<Command> {
    let mut result = vec![];
    loop {

    }
}

fn main() {
    let program = read_program("inputs/day17.txt");
    let mut computer = Computer::new(&program);
    let mut output = String::new();
    while !computer.halted {
        output += &to_ascii(computer.run(vec![]).unwrap());
    }
    println!("{}", output);
    let mut field = HashMap::new();
    for (y, l) in output.lines().enumerate() {
        for (x, ch) in l.chars().enumerate() {
            field.insert((x as i64, y as i64), ch);
        }
    }
}
