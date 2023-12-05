use std::fs::read_to_string;

use regex::Regex;
use ndarray::{prelude, Array2, ArrayBase, Data, Ix2};

#[derive(Debug)]
struct EngNumber {
    number: i32,
    x_range: (usize, usize),
    y: usize,
}

fn main() {
    let testinput = read_to_string("day3test.txt").expect("Cannot read from file");
    let all_numbers = find_all_numbers(&testinput);
    let txtgrid = convert_to_grid(&testinput);
    println!("{}", testinput);
    println!("{}", txtgrid);
    for engpart in &all_numbers {
        println!("{engpart:?}");
    }
}

fn find_all_numbers(fulltxt: &str) -> Vec<EngNumber> {
    let numreg = Regex::new(r"\d+").expect("Regex failed to create");

    fulltxt
        .lines()
        .enumerate()
        .flat_map(|(i, line)| {
            numreg.find_iter(line).map(move |value| EngNumber {
                number: value
                    .as_str()
                    .parse::<i32>()
                    .expect("Value not formatted correctly"),
                x_range: (value.start(), value.end()),
                y: i,
            })
        })
        .collect()
}

fn check_number_surroundings<T: Data<Elem = char>>(no: &EngNumber, txtgrid: ArrayBase<T, Ix2>) -> bool {
    // let x_start = if no.x_range.0 == 0 { 0 } else { no.x_range.0 - 1 };
    // let x_end = if no.x_range.1 == dim.0 { dim.0 } else { no.x_range.1 + 1 };
    // let mut lines = fulltxt.lines();

    // let mut all_iterators = Vec::new();

    // if no.y > 0 {
    //     all_iterators.push(lines.nth(no.y - 1).unwrap()[x_start..x_end].chars());
    // }

    // let middle = lines.next().unwrap();
    // let middle_result = true;

    // if no.y < dim.1 {
    //     all_iterators.push(lines.next().unwrap()[x_start..x_end].chars());
    // }

    // let above_and_below_result = all_iterators.into_iter().flatten().any(|elem| true);
    // middle_result || above_and_below_result
    true
}

fn get_dim(fulltxt: &str) -> (usize, usize) {
    let mut lines = fulltxt.lines();
    let x = lines.next().unwrap().len();
    let y = lines.count() + 1;
    (x, y)
}

fn convert_to_grid(fulltxt: &str) -> Array2<char> {
    ArrayBase::from_shape_vec(get_dim(fulltxt), fulltxt.lines().flat_map(|line| line.chars()).collect()).unwrap()
}
