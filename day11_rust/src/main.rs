use std::ops::AddAssign;

fn main() {
    let input: Vec<Point> = include_str!("input.txt")
        .split(",")
        .map(|s| Point::new(s))
        .collect();
    let dists = all_distances_from_origin(&input);
    println!("Part 1: {}", dists.last().unwrap());
    println!("Part 2: {}", dists.iter().max().unwrap());
}

fn all_distances_from_origin(directions: &Vec<Point>) -> Vec<i32> {
    directions.iter()
        .scan(Point::origin(), |acc, x| { *acc += *x; Some(*acc) })
        .map(|d| d.dist_from_origin())
        .collect()
}

#[derive(Copy, Clone)]
struct Point {
    x: i32,
    y: i32,
    z: i32
}

impl AddAssign for Point {
    fn add_assign(&mut self, other: Point) {
        *self = Point {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        };
    }
}

impl Point {

    fn origin() -> Point {
        Point{x: 0, y: 0, z: 0}
    }

    fn new(direction: &str) -> Point {
        match direction {
            "s"  => Point{x: -1, y: 0, z: 1},
            "n"  => Point{x: 1, y: 0, z: -1},
            "sw" => Point{x: 0, y: -1, z: 1},
            "se" => Point{x: -1, y: 1, z: 0},
            "nw" => Point{x: 1, y: -1, z: 0},
            "ne" => Point{x: 0, y: 1, z: -1},
            _ => panic!("bad direction"),
        }
    }

    fn dist_from_origin(&self) -> i32 {
        *[self.x.abs(), self.y.abs(), self.z.abs()].iter().max().unwrap()
    }
}

