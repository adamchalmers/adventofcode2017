fn main() {
    let instrs: Vec<Instruction> = include_str!("input.txt")
        .split(",")
        .map(Instruction::new)
        .collect();

    let ring = String::from("abcdefghijklmnop");
    let ring_after_dance = instrs.iter().fold(ring.clone(), acc);

    print!("Solution 1: {}\n", ring_after_dance);

    let k = cycle_size(&ring, &instrs);
    print!("Cycle size: {}\n", k);
    let mut curr = ring.clone();
    for _ in 0..((10^9) % k) {
        // print!("{}\n", curr);
        curr = instrs.iter().fold(curr, acc);
    }
    print!("Solution 2: {}\n", curr);
}

fn cycle_size(goal: &str, instrs: &Vec<Instruction>) -> i32 {
    let mut i = 1;
    let mut curr = instrs.iter().fold(goal.to_string(), acc);
    while curr != goal {
        i += 1;
        curr = instrs.iter().fold(curr, acc);
    }
    i
}

fn acc(ring: String, instruction: &Instruction) -> String {
    instruction.apply(ring)
}

enum Instruction {
    Spin(usize),
    Exchange(usize, usize),
    Partner(char, char),
}

impl Instruction {
    fn new(line: &str) -> Instruction {
        let mut chars: Vec<char> = line.chars().collect();
        let head = chars.remove(0);
        let args: String = chars.into_iter().collect();
        let parts: Vec<&str> = args.split("/").collect();

        match head {
            's' =>
                Instruction::Spin(parts[0].parse().unwrap()),
            'x' =>
                Instruction::Exchange(parts[0].parse().unwrap(), parts[1].parse().unwrap()),
            'p' =>
                Instruction::Partner(parts[0].chars().next().unwrap(), parts[1].chars().next().unwrap()),
            other =>
                panic!(format!("Unexpected dance move start {}", other))
        }
    }

    fn apply(&self, ring: String) -> String {
        let mut chars: Vec<char> = ring.chars().collect();

        match *self {
            Instruction::Spin(n) => {
                for _ in 0..(chars.len() - n) {
                    let ch = chars.remove(0);
                    chars.push(ch);
                }
                chars.into_iter().collect()
            }

            Instruction::Exchange(i, j) => {
                let x = chars[i];
                let y = chars[j];
                chars[i] = y;
                chars[j] = x;
                chars.into_iter().collect()
            }

            Instruction::Partner(x, y) => {
                let i = chars.iter().position(|&elem| elem == x).unwrap();
                let j = chars.iter().position(|&elem| elem == y).unwrap();
                chars[i] = y;
                chars[j] = x;
                chars.into_iter().collect()
            }
        }
    }
}