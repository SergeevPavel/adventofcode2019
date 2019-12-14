use std::fs::read_to_string;
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct Reaction {
    inputs: Vec<(i64, String)>,
    output: (i64, String)
}

fn div_up(req_count: i64, produce: i64) -> i64 {
    if req_count % produce == 0 {
        req_count / produce
    } else {
        req_count / produce + 1
    }
}

fn cook(recipes: &HashMap<String, Reaction>, req: i64) -> i64 {
    let mut ingredients: HashMap<String, i64> = HashMap::new();
    let mut ore = 0;
    ingredients.insert("FUEL".to_string(), -req);
    loop {
//        println!("{:?}", ingredients);
        let (req, req_count) = match ingredients.iter().filter(|v| *v.1 < 0).next() {
            None => break,
            Some(req) => req,
        };
        let reaction = recipes.get(req).unwrap();
        let produce = reaction.output.0;
        let reaction_mult = div_up(req_count.abs(), produce);
        *ingredients.entry(req.clone()).or_insert(0) += produce * reaction_mult;
        for (req_count, req) in &reaction.inputs {
            if req.clone() == "ORE".to_string() {
                ore += req_count * reaction_mult;
            } else {
                *ingredients.entry(req.clone()).or_insert(0) -= req_count * reaction_mult;
            }
        }
    }
    return ore;
}

fn main() {
    let reactions: Vec<_> = read_to_string("inputs/day14.txt").unwrap()
        .lines()
        .map(|l| {
            let mut parts = l.split("=>");
            let inputs: Vec<(i64, String)> = parts.next().unwrap()
                .split(",")
                .map(|is| {
                    let mut it = is.trim().split(" ");
                    (it.next().unwrap().trim().parse().unwrap(), it.next().unwrap().to_string())
                })
                .collect();
            let output = parts.next().unwrap();
            let mut it = output.trim().split(" ");
            Reaction {
                inputs,
                output: (it.next().unwrap().trim().parse().unwrap(), it.next().unwrap().to_string())
            }
        })
        .collect();
    let mut recipes = HashMap::new();
    reactions.iter().for_each(|r| {
        recipes.insert(r.output.1.clone(), r.clone());
    });
    let ore = 1000000000000;
    let mut max_fuel = 100000000;
    let mut min_fuel = 0;
    loop {
        let fuel_req = (min_fuel + max_fuel) / 2;
        let ore_res = cook(&recipes, fuel_req);
        if ore_res > ore {
            max_fuel = fuel_req;
        } else {
            min_fuel = fuel_req;
        }
        if max_fuel - min_fuel <= 1 {
            break;
        }
    }
    println!("Result: {:?}", min_fuel);
}