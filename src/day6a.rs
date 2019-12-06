use std::fs::read_to_string;
use std::collections::HashMap;

type Node = String;
type Graph = HashMap<Node, Vec<Node>>;

fn dfs(current: &Node, graph: &Graph, degrees: &mut HashMap<Node, u64>) {
    let mut degree = 0;
    for d in &graph[current] {
        dfs(d, graph, degrees);
        degree += degrees[d] + 1;
    }
    degrees.insert(current.clone(), degree);
}

fn main() {
    let input = read_to_string("inputs/day6.txt").unwrap();
    let edges: Vec<(_, _)> = input
        .split("\n")
        .map(|edge| {
            if let [from, to] = edge.split(")").collect::<Vec<_>>().as_slice() {
                (from.to_string(), to.to_string())
            } else {
                panic!(format!("Broken edge: {:?}", edge))
            }
        })
        .collect();
    let mut graph: Graph = HashMap::new();
    for (from, to) in edges {
        graph.entry(to.clone()).or_insert(vec![]);
        graph.entry(from.clone()).or_default().push(to);
    }
    let mut degrees: HashMap<Node, u64> = HashMap::new();
    for n in graph.keys() {
        if !degrees.contains_key(n) {
            dfs(n, &graph, &mut degrees);
        }
    }
//    println!("Degrees {:?}", degrees);
    let result = degrees.iter().fold(0, |acc, (n, d)| {
        acc + *d
    });
    println!("Result {:?}", result);
}