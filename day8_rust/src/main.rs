use std::collections::HashMap;

fn main() {
    let mut registers: HashMap<String, i32> = HashMap::new();
    let instructions = include_str!("input.txt")
        .split("\n")
        .map(|line| Instruction::from(line));

    let max_during = instructions.map(|i| i.apply_to(&mut registers)).max();
    let max_final = registers.values().max();

    println!("Largest value stored after running program: {}", max_final.unwrap());
    println!("Largest value stored while running program: {}", max_during.unwrap());

}

struct Instruction {
    register_id: String,
    op: String,
    amount: i32,
    lhs: String,
    cmp_symbol: String,
    rhs: i32,
}

impl Instruction {

    fn from(line: &str) -> Instruction {
        let mut it = line.split(" ");
        let register_id = it.next().unwrap().to_string();
        let op = it.next().unwrap().to_string();
        let amount = it.next().unwrap().parse::<i32>().unwrap();
        it.next();
        let lhs = it.next().unwrap().to_string();
        let cmp_symbol = it.next().unwrap().to_string();
        let rhs = it.next().unwrap().parse::<i32>().unwrap();
        return Instruction {
            register_id,
            op,
            amount,
            lhs,
            cmp_symbol,
            rhs
        }
    }

    fn apply_to(&self, registers: &mut HashMap<String, i32>) -> i32 {
        let curr_val: i32;
        {
            let lhs: &str = self.lhs.as_ref();
            curr_val = *registers.get(lhs).unwrap_or(&0);
        }

        if !self.should_execute(&curr_val) {
            return i32::min_value();
        }

        let register_id = self.register_id.clone();
        let entry = registers.entry(register_id).or_insert(0);
        *entry += self.amount_added();
        *entry
    }

    fn should_execute(&self, curr_val: &i32) -> bool {
        match self.cmp_symbol.as_ref() {
            "<" => *curr_val < self.rhs,
            ">" => *curr_val > self.rhs,
            "<=" => *curr_val <= self.rhs,
            ">=" => *curr_val >= self.rhs,
            "==" => *curr_val == self.rhs,
            "!=" => *curr_val != self.rhs,
            _ => panic!("bad cmp_symbol"),
        }
    }

    fn amount_added(&self) -> i32 {
        self.amount * match self.op.as_ref() {
            "inc" => 1,
            "dec" => -1,
            _ => panic!("bad op"),
        }
    }
}