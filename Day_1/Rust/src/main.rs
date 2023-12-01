use std::fs::read_to_string;

fn main() {
    let tot_val = naive_count("input.txt");
    println!("{}", tot_val);
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