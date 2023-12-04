use std::fs::read_to_string;

fn main() {
    let testinput = read_to_string("day3test.txt").expect("Cannot read from file");
    println!("{}", testinput);
}
