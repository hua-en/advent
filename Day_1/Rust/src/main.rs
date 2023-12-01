use std::fs::read_to_string;
use std::collections::HashMap;

use regex::Regex;

fn main() {
    let tot_val = naive_count("input.txt");
    let tot_val_2 = number_and_word_count("input.txt");
    println!("First Part: {}", tot_val);
    println!("Second Part: {}", tot_val_2);
}

fn naive_count(path: &str) -> i32 {
    let fullstr = read_to_string(path).unwrap();
    let lines = fullstr.lines();

    let mut tot_val = 0;

    for line in lines {
        // Find first and last digit
        let mut fst = ' ';
        let mut lst = ' ';
        let mut found_fst = false;
        for c in line.chars() {
            if !c.is_ascii_digit() {
                continue;
            }
            if !found_fst {
                fst = c;
                found_fst = true;
            }
            lst = c;
        }
        // Combine them together and add
        let fin_val = format!("{}{}", fst, lst).parse::<i32>().unwrap();
        tot_val += fin_val;
    }

    tot_val
}

fn number_and_word_count(path: &str) -> i32 {
    let numstr_to_num = HashMap::from([
        ("one".to_string(), '1'),
        ("two".to_string(), '2'),
        ("three".to_string(), '3'),
        ("four".to_string(), '4'),
        ("five".to_string(), '5'),
        ("six".to_string(), '6'),
        ("seven".to_string(), '7'),
        ("eight".to_string(), '8'),
        ("nine".to_string(), '9')
    ]);

    // Approach: Use regex to find the numbers in the string, then get the first and last number
    let numletreg = Regex::new(r"one|two|three|four|five|six|seven|eight|nine|\d").unwrap();

    let fullstr = read_to_string(path).unwrap();
    let lines = fullstr.lines();
    
    // Find the value in each line
    let tot_val = lines.map(|line| {
        // Look for first character
        let fst = convert_to_digit(numletreg.find(line).unwrap().as_str(), &numstr_to_num);
        
        // Look for last character
        let line_len = line.len();
        let lst = (0..line_len).rev()
        .find_map(|i| {
            let x = numletreg.find(&line[i..])?;
            Some(convert_to_digit(x.as_str(), &numstr_to_num))
        })
        .expect("nothing found: panicking");

        // Combine them together and add
        format!("{}{}", fst, lst).parse::<i32>().unwrap()
    }).sum(); // Sum up values in all lines

    tot_val
}

fn convert_to_digit(pos_str: &str, map: &HashMap<String, char>) -> char {
    if pos_str.len() > 1 {
        map[pos_str]
    } else {
        pos_str.chars().next().unwrap()
    }
}
