

fn partition(v: &Vec<char>) -> Vec<Vec<char>> {
    let mut resutl = Vec::new();
    if v.len() == 0 {
        return resutl;
    }
    let mut current = v[0];
    resutl.push(Vec::new());
    for x in v {
        if *x == current {
            resutl.last_mut().unwrap().push(*x);
        } else {
            current = *x;
            resutl.push(vec![*x]);
        }
    }
    return resutl;
}

fn check(x: u64) -> bool {
    let chars = x.to_string().chars().collect::<Vec<_>>();
    let mut partition_sizes = partition(&chars).iter().map(|p| p.len()).collect::<Vec<_>>();
    partition_sizes.sort();
    let digit_not_decrease = chars.windows(2).all(|ds| {
        ds[0].to_digit(10).unwrap() <=ds[1].to_digit(10).unwrap()
    });
    return partition_sizes.contains(&2) && digit_not_decrease;
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
    assert!(check(112233));
    assert!(!check(123444));
    assert!(check(111122));
}