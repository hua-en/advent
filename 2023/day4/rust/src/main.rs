use std::collections::HashMap;
use std::fs::read_to_string;
use std::iter::zip;

fn main() {
    println!("Hello, world!");
    let input = read_to_string("day4input.txt").expect("Cannot read from file");
    println!("Score 1: {}", check_cards_1(&input));
    println!("Score 2: {}", check_cards_2(&input));
}

fn check_cards_1(fulltext: &str) -> i32 {
    winners_per_card(fulltext)
        .into_iter()
        .map(|v| calc_score(v.try_into().unwrap()))
        .sum()
}

fn check_cards_2(fulltext: &str) -> i32 {
    let winners = winners_per_card(fulltext);
    no_of_cards(&winners)
}

fn parse_line(line: &str) -> (String, String) {
    let mut values = line
        .split(": ")
        .nth(1)
        .expect("Line not parsed correctly")
        .split(" | ");
    let winningvalues = values
        .next()
        .expect("Winning values not parsed correctly")
        .to_string();
    let cardvalues = values
        .next()
        .expect("Card values not parsed correctly")
        .to_string();
    (winningvalues, cardvalues)
}

fn winners_in_line(winners: &str, cardvalues: &str) -> usize {
    let winningmap: HashMap<i32, i32> = winners
        .split_whitespace()
        .map(|no| (no.parse::<i32>().expect("Failed to parse value"), 1))
        .collect();
    cardvalues
        .split_whitespace()
        .filter(|no| winningmap.contains_key(&no.parse::<i32>().expect("Failed to parse value")))
        .count()
}

fn winners_per_card(fulltext: &str) -> Vec<usize> {
    fulltext
        .lines()
        .map(|line| {
            let (winningvalues, cardvalues) = parse_line(line);
            winners_in_line(&winningvalues, &cardvalues)
        })
        .collect()
}

fn calc_score(score: u32) -> i32 {
    match score {
        0 => 0,
        v => {
            let base: i32 = 2;
            base.pow(v - 1)
        }
    }
}

fn no_of_cards(winners: &[usize]) -> i32 {
    let card_cnt = winners.len();
    let mut win_per_card = Vec::from([0]);
    for (i, j) in zip((0..card_cnt - 1).rev(), 1..card_cnt) {
        let no_of_winners = winners[i];
        let tot_winners =
            no_of_winners + win_per_card[(j - no_of_winners)..j].iter().sum::<usize>();
        win_per_card.push(tot_winners)
    }
    (win_per_card.iter().sum::<usize>() + card_cnt)
        .try_into()
        .expect("Casting of usize to i32 failed")
}

// Doesn't work because Rust doesn't allow you to access a list while it is being mutated
// fn no_of_cards_functional(winners: &[usize]) -> i32 {
//     let mut new_winners = Vec::from(winners);
//     let win_per_card: Vec<_> = new_winners.iter_mut().enumerate().map(|(i, no_of_winners)| {
//         *no_of_winners + new_winners[(i-*no_of_winners)..i].iter().sum::<usize>()
//     }).collect();
//     (win_per_card.iter().sum::<usize>() + winners.len()).try_into().unwrap()
// }
