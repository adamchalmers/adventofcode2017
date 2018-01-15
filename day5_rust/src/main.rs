fn main() {
    let input = include_str!("input.txt").split("\n");
    let jumps: Vec<i32> = input.map(|x|x.parse::<i32>().unwrap()).collect();
    println!("Soln 1: {}", num_jumps_to_escape(&mut jumps.to_vec(), |x|x+1));
    let f = |x| if x >= 3 { x-1 } else { x+1 };
    println!("Soln 2: {}", num_jumps_to_escape(&mut jumps.to_vec(), f));
}

fn num_jumps_to_escape<F>(jumps: &mut Vec<i32>, offset: F) -> i32 
	where F: Fn(i32) -> i32 {
	let mut curr: i32 = 0;
	let mut t = 0;
	while curr < jumps.len() as i32 {
		let curr_v = jumps[curr as usize];
		jumps[curr as usize] = offset(jumps[curr as usize]);
		t += 1;
		curr += curr_v;
	}
	return t
}