use std::fs::read_to_string;

use ndarray::{prelude, s, Array2, ArrayBase, Data, Ix2};
use regex::Regex;

#[derive(Debug)]
struct EngNumber {
    number: i32,
    x_range: (usize, usize),
    y: usize,
}

fn main() {
    let testinput = read_to_string("day3input.txt").expect("Cannot read from file");
    let txtgrid = convert_to_grid(&testinput);
    let all_numbers = find_all_numbers(&testinput);

    let part_sum = sum_all_engine_numbers(&all_numbers, txtgrid.view());
    println!("Sum of all parts: {}", part_sum);

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

fn check_number_surroundings<T: Data<Elem = char>>(
    no: &EngNumber,
    txtgrid: ArrayBase<T, Ix2>,
) -> bool {
    let (xdim, ydim) = txtgrid.dim();
    let x_start = if no.x_range.0 == 0 {
        0
    } else {
        no.x_range.0 - 1
    };
    let x_end = if no.x_range.1 == xdim {
        xdim
    } else {
        no.x_range.1 + 1
    };

    // Check top row
    if no.y > 0
        && txtgrid
            .slice(s![no.y - 1, x_start..x_end])
            .iter()
            .any(|x| !(x.is_ascii_digit() || *x == '.'))
    {
        return true;
    }

    // Check middle row
    if no.x_range.0 > 0 {
        let fstc = txtgrid[[no.y, no.x_range.0 - 1]];
        if !(fstc.is_ascii_digit() || fstc == '.') {
            return true;
        }
    }

    if no.x_range.1 < xdim {
        let lstc = txtgrid[[no.y, no.x_range.1]];
        if !(lstc.is_ascii_digit() || lstc == '.') {
            return true;
        }
    }

    // Check bottom row
    if no.y < ydim - 1
        && txtgrid
            .slice(s![no.y + 1, x_start..x_end])
            .iter()
            .any(|x| !(x.is_ascii_digit() || *x == '.'))
    {
        return true;
    }
    false
}

fn sum_all_engine_numbers<T: Data<Elem = char>>(
    numbers: &[EngNumber],
    txtgrid: ArrayBase<T, Ix2>,
) -> i32 {
    numbers
        .iter()
        .filter(|no| check_number_surroundings(no, txtgrid.view()))
        .map(|no| no.number)
        .sum()
}

fn get_dim(fulltxt: &str) -> (usize, usize) {
    let mut lines = fulltxt.lines();
    let x = lines.next().unwrap().len();
    let y = lines.count() + 1;
    (x, y)
}

fn convert_to_grid(fulltxt: &str) -> Array2<char> {
    ArrayBase::from_shape_vec(
        get_dim(fulltxt),
        fulltxt.lines().flat_map(|line| line.chars()).collect(),
    )
    .unwrap()
}
