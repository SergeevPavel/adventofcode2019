use std::fs::read_to_string;

fn main() {
    let width = 25;
    let height = 6;
    let input = read_to_string("inputs/day8.txt").unwrap();
    let digits: Vec<_> = input.chars().map(|c| c.to_digit(10).unwrap()).collect();
    let result = digits.chunks(width * height)
        .map(|ch| {
            let zeros_count = ch.iter().filter(|d| **d == 0).count();
            let ones_count = ch.iter().filter(|d| **d == 1).count();
            let twos_count = ch.iter().filter(|d| **d == 2).count();
            (zeros_count, ones_count, twos_count)
        })
        .min_by_key(|(zeros_count, _, _)| *zeros_count)
        .map(|(_, ones_count, twos_count)| ones_count * twos_count)
        .unwrap();
    println!("Result: {:?}", result);
}