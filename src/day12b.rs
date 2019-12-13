use std::collections::HashMap;
use std::ops::Rem;

fn dividers(mut x: u64) -> HashMap<u64, u64> {
    let mut dividers = HashMap::new();
    while x > 1 {
        for d in 2..(x + 1) {
            if x.rem(d) == 0 {
                x /= d;
                *dividers.entry(d).or_insert(0) += 1;
                break;
            }
        }
    }
    return dividers;
}

fn main() {
    println!("Dividers: {:?}", dividers(231614));
    println!("Dividers: {:?}", dividers(193052));
    println!("Dividers: {:?}", dividers(102356));
    println!("Result: {:?}", 2 * 2 * (115807 as u64) * 17 * 17 * 167 * 25589);
}