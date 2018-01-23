use std::ops::AddAssign;
use std::fmt::Display;
use std::fmt;
use std::cmp::Ordering;
use std::collections::HashMap;

const NUM_TICKS: i32 = 100000;
const COLLISIONS: bool = false;

fn main() {
    let input = include_str!("input.txt");
    let mut particles: Vec<Particle> = input.split("\n").enumerate().map(|(i,line)|Particle::new(i, line)).collect();

    print!("Running {} ticks on {} particles\n", NUM_TICKS, particles.len());
    for _ in 0..NUM_TICKS {

        for p in &mut particles {
            p.tick();
        }

        if !COLLISIONS {
            continue;
        }

        // Find how many particles exist at each position
        let mut position_freq: HashMap<Point, i32> = HashMap::new();
        for p in &particles {
            let freq = position_freq.entry(p.position).or_insert(0);
            *freq += 1;
        }

        // Remove all particles that collided with other particles
        let mut i = 0;
        while i != particles.len() {
            let freq = *position_freq.get(&particles[i].position).unwrap();
            if freq > 1 {
                particles.remove(i);
            } else {
                i += 1;
            }
        }
    }

    print!("There are {} particles\n", particles.len());
    let closest = particles.iter().min().unwrap();
    print!("The closest particle to the origin is {} at {}\n", closest.id, closest.position);
}

// POINT STUFF

#[derive(Copy, Clone, Debug, Eq, Hash)]
struct Point {
    x: i64,
    y: i64,
    z: i64,
}

impl Point {
    fn dist_from_origin(&self) -> i64 {
        return self.x.abs() + self.y.abs() + self.z.abs()
    }
}

impl Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {}, {})", self.x, self.y, self.z)
    }
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

impl PartialEq for Point {
    fn eq(&self, other: &Point) -> bool {
        self.x == other.x && self.y == other.y && self.z == other.z
    }
}

// PARTICLE STUFF

#[derive(Debug, Eq)]
struct Particle {
    id: i32,
    position: Point,
    velocity: Point,
    acceleration: Point,
}

impl Particle {
    fn new(i: usize, line: &str) -> Particle {
        let mut it = line.split(",").map(|s|s.parse::<i64>().unwrap());
        let px = it.next().unwrap();
        let py = it.next().unwrap();
        let pz = it.next().unwrap();
        let vx = it.next().unwrap();
        let vy = it.next().unwrap();
        let vz = it.next().unwrap();
        let ax = it.next().unwrap();
        let ay = it.next().unwrap();
        let az = it.next().unwrap();
        Particle {
            id: i as i32,
            position:     Point{x: px, y: py, z: pz},
            acceleration: Point{x: ax, y: ay, z: az},
            velocity:     Point{x: vx, y: vy, z: vz},
        }
    }

    fn tick(&mut self) {
        self.velocity += self.acceleration;
        self.position += self.velocity;
    }
}

impl Ord for Particle {
    fn cmp(&self, other: &Particle) -> Ordering {
        self.position.dist_from_origin().cmp(&other.position.dist_from_origin())
    }
}

impl PartialOrd for Particle {
    fn partial_cmp(&self, other: &Particle) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Particle {
    fn eq(&self, other: &Particle) -> bool {
        self.position.dist_from_origin() == other.position.dist_from_origin()
    }
}
