

fn check(x: u64) -> bool {
    let chars = x.to_string().chars().collect::<Vec<_>>();
    let has_same_adjacent_digits = chars.windows(2).any(|w| w[0] == w[1]);
    let digit_not_decrease = chars.windows(2).all(|ds| {
        ds[0].to_digit(10).unwrap() <=ds[1].to_digit(10).unwrap()
    });
    return has_same_adjacent_digits && digit_not_decrease;
}

fn main() {
    let from = 178416;
    let to   = 676461;
    let mut result = 0;
    for x in from..(to + 1) {
        if check(x) {
            result += 1;
        }
    }
    println!("Result: {:?}", result);
}

#[test]
fn test() {
    assert!(check(111111));
    assert!(!check(223450));
    assert!(!check(123789));
}