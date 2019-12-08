use std::fs::read_to_string;

fn main() {
    let width = 25;
    let height = 6;
    let input = read_to_string("inputs/day8.txt").unwrap();
    let digits: Vec<_> = input.chars().map(|c| c.to_digit(10).unwrap()).collect();
    let layers: Vec<_> = digits.chunks(width * height).collect();
    for y in 0..height {
        for x in 0..width {
            let color = layers.iter().map(|l| l[y * width + x]).find(|v| *v != 2).unwrap();
            if color == 0 {
                print!(" ");
            } else {
                print!("X")
            }
        }
        println!();
    }
}