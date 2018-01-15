use std::collections::HashMap;

fn main() {
    let mut registers: HashMap<&str, i32> = HashMap::new();
    let mut max_during = 0;

    for line in include_str!("input.txt").split("\n") {
        let instruction = Instruction::from(line);
        let curr_val: i32;
        {
            curr_val = *registers.get(instruction.lhs).unwrap_or(&0);
        }

        if !instruction.should_execute(&curr_val) {
            continue;
        }

        let entry = registers.entry(instruction.register_id).or_insert(0);
        *entry += instruction.amount_added();
        if *entry > max_during {
            max_during = *entry;
        }
    }

    let mut max_final = 0;
    for val in registers.values() {
        if *val > max_final {
            max_final = *val;
        }
    }

    println!("Largest value after program: {}", max_final);
    println!("Largest value during program: {}", max_during);

}

struct Instruction<'a> {
    register_id: &'a str,
    op: &'a str,
    amount: i32,
    lhs: &'a str,
    cmp_symbol: &'a str,
    rhs: i32,
}

impl<'a> Instruction<'a> {

    fn from(line: &'a str) -> Instruction<'a> {
        let mut it = line.split(" ");
        let register_id = it.next().unwrap();
        let op = it.next().unwrap();
        let amount = it.next().unwrap().parse::<i32>().unwrap();
        it.next();
        let lhs = it.next().unwrap().clone();
        let cmp_symbol = it.next().unwrap();
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

    fn should_execute(&self, curr_val: &i32) -> bool {
        match self.cmp_symbol {
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
        self.amount * match self.op {
            "inc" => 1,
            "dec" => -1,
            _ => panic!("bad op"),
        }
    }
}