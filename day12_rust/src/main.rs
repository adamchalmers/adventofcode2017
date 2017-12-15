/// Solution for https://adventofcode.com/2017/day/12

use std::collections::HashMap;
use std::collections::BTreeSet;
use std::iter::FromIterator;

/// Maps each house to a list of all houses it directly shares piping with.
fn parse_village_piping() -> HashMap<i32, Vec<i32>> {

	fn parse_row(row: &str) -> (i32, Vec<i32>) {
		let mut parts = row.split(" <-> ");
		let head = parts.next();
		let tail = parts.next();
		let house = head.unwrap().parse::<i32>().unwrap();
		let neighbours = tail.unwrap().split(", ").map(|x| x.parse::<i32>().unwrap()).collect();
		(house, neighbours)
	}
	include_str!("data.txt").split("\n").map(parse_row).collect()
}

/// Finds all houses which share piping (directly or indirectly) with houses in `ingroup`.
fn pipe_connections(village_piping: &HashMap<i32, Vec<i32>>, ingroup: BTreeSet<i32>) -> BTreeSet<i32> {
	let mut fringe: BTreeSet<i32> = BTreeSet::new();
	for house in &ingroup {
		let vec = village_piping.get(&house).unwrap();
		for neighbour in vec {
			fringe.insert(*neighbour);
		}
	}
	
	if fringe.is_subset(&ingroup) {
		return ingroup
	} 
	pipe_connections(village_piping, &ingroup | &fringe)
}

/// Create a set with one object.
fn singleton<T: Eq + Ord>(x: T) -> BTreeSet<T> where T: std::hash::Hash + std::clone::Clone {
   BTreeSet::from_iter(vec![x].iter().cloned())
}

/// A house's "group" is the set of all houses with piping that connects to it,
/// either directly or indirectly.
fn group_of(house: i32) -> BTreeSet<i32> {
	pipe_connections(&parse_village_piping(), singleton(house))
}

/// Find all distinct groups in this village
fn all_groups() -> BTreeSet<BTreeSet<i32>> {
	let village_piping = &parse_village_piping();
	let all_houses = village_piping.iter().map(|x| x.0);
	all_houses.map(|x| pipe_connections(village_piping, singleton(*x))).collect()
}

fn main() {
	print!("Part 1: {}\n", group_of(0).len());
	print!("Part 2: {}\n", all_groups().len());
}
