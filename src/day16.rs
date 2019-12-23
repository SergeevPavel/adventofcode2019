use std::fs::read_to_string;
use std::iter;
use std::ops::Range;
use std::ops::Rem;

fn pattern(size: usize, offset: usize) -> Vec<i32> {
    let zeros1 = iter::repeat(0).take(offset);
    let ones = iter::repeat(1).take(offset);
    let zeros2 = iter::repeat(0).take(offset);
    let minus_ones = iter::repeat(-1).take(offset);
    let single_pattern: Vec<_> = zeros1.chain(ones.chain(zeros2.chain(minus_ones))).collect();
    let mut result = Vec::with_capacity(size);
    for i in 1..(size + 1) {
        result.push(single_pattern[i % single_pattern.len()]);
    }
//    println!("Pat for offset: {} is: {:?}", offset, result);
    return result;
}

fn phase(signal: Vec<i32>) -> Vec<i32> {
    (1..(signal.len() + 1)).map(|offset| {
        signal.iter()
            .zip(pattern(signal.len(), offset).iter())
            .map(|(x, p)| {
                x * p
            })
            .fold(0, |acc, v| {
                (acc + v)
            }).rem(10).abs()
    }).collect()
}

fn parse_phase(input: String) -> Vec<i32> {
    input.chars().map(|c| c.to_digit(10).unwrap() as i32).collect()
}

fn nth_phase(signal: Vec<i32>, n: u32) -> Vec<i32> {
    let mut result = signal;
    for p in 0..n {
        result = phase(result);
        println!("Phase: {:?} : {:?}", p + 1, result);
    }
    return result;
}

fn main() {
    let input = read_to_string("inputs/day16.txt").unwrap();
//    let signal = parse_phase(input);
    let signal = vec![1, 2, 3, 4, 5, 6, 7, 8];
    println!("Elements count: {}", signal.len());
//    println!("Signal: {:?}", signal);
    println!("{:?}", nth_phase(signal, 100));
}

#[test]
fn phase_test() {
    assert_eq!(phase(vec![1, 2, 3, 4, 5, 6, 7, 8]), vec![4, 8, 2, 2, 6, 1, 5, 8]);
}


#[test]
fn phase_nth_test() {
    assert_eq!(nth_phase(vec![1, 2, 3, 4, 5, 6, 7, 8], 1), vec![4, 8, 2, 2, 6, 1, 5, 8]);
    assert_eq!(nth_phase(vec![1, 2, 3, 4, 5, 6, 7, 8], 2), vec![3, 4, 0, 4, 0, 4, 3, 8]);
    assert_eq!(nth_phase(vec![1, 2, 3, 4, 5, 6, 7, 8], 3), vec![0, 3, 4, 1, 5, 5, 1, 8]);
    assert_eq!(nth_phase(vec![1, 2, 3, 4, 5, 6, 7, 8], 4), vec![0, 1, 0, 2, 9, 4, 9, 8]);
}