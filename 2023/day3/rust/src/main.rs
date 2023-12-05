use std::fs::read_to_string;

use regex::Regex;

struct EngPart {
    number: i32,
    coordnates: Vec<(i32, i32)>,
}

fn main() {
    let testinput = read_to_string("day3test.txt").expect("Cannot read from file");
    println!("{}", testinput);
}

fn find_all_numbers(fulltxt: &str) -> Vec<EngPart> {
    let numreg = Regex::new(r"\d+").expect("Regex failed to create");

    Vec::new()
}
