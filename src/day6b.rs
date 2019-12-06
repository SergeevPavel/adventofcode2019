use std::fs::read_to_string;
use std::collections::{HashMap, VecDeque};

type Node = String;
type Graph = HashMap<Node, Vec<Node>>;

fn bfs(start: &Node, graph: &Graph, distance: &mut HashMap<Node, u64>) {
    let mut q = VecDeque::new();
    q.push_back((start.clone(), 0));
    while !q.is_empty() {
        let (current, step) = q.pop_front().unwrap();
        distance.insert(current.clone(), step);
        for child in &graph[&current] {
            if !distance.contains_key(child) {
                q.push_back((child.clone(), step + 1));
            }
        }
    }
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
        graph.entry(from.clone()).or_default().push(to.clone());
        graph.entry(to).or_default().push(from);
    }
    let mut distance = HashMap::new();
    bfs(&"YOU".to_string(), &graph, &mut distance);
    println!("Distances: {:?}", distance);
    println!("Result {:?}", distance[&"SAN".to_string()] - 2);
}