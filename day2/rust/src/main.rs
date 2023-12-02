use std::fs::read_to_string;

use regex::Regex;

#[derive(Debug)]
struct GameData {
    id: i32,
    games: Vec<Play>,
}

#[derive(Debug)]
struct Play {
    red: i32,
    green: i32,
    blue: i32,
}

fn main() {
    let bag = Play {red: 12, green: 13, blue: 14};
    let testinput = parse_input("test.txt");
    for i in &testinput {
        println!("{i:?}");
    }
    let testoutput = sum_all_valid(&testinput, &bag);
    println!("Test Output: {}", testoutput);

    let realinput = parse_input("input.txt");
    let realoutput = sum_all_valid(&realinput, &bag);
    println!("Real Output: {}", realoutput);
}

fn parse_input(path: &str) -> Vec<GameData> {
    let gameidreg = Regex::new(r"Game (\d+)").expect("Regex failed to create");
    let redreg = Regex::new(r"(\d+) red").expect("Regex failed to create");
    let greenreg = Regex::new(r"(\d+) green").expect("Regex failed to create");
    let bluereg = Regex::new(r"(\d+) blue").expect("Regex failed to create");

    let fullstr = read_to_string(path).expect("Cannot read from file");
    let lines = fullstr.lines();

    lines
        .map(|line| {
            let tmp: Vec<_> = line.split(':').collect();
            let gameidstr = tmp[0];
            let gameinfolst = tmp[1].split(';');

            // Parse game ID
            let id = gameidreg
                .captures(gameidstr)
                .expect("Heading format is wrong")[1]
                .parse::<i32>()
                .expect("Game id is not a digit");

            // Parse game data
            let gamedata: Vec<_> = gameinfolst.map(|play| {
                let red = match redreg.captures(play) {
                    None => 0,
                    Some(capt) => capt[1].parse::<i32>().expect("Red value is not a digit")
                };
                let green = match greenreg.captures(play) {
                    None => 0,
                    Some(capt) => capt[1].parse::<i32>().expect("Green value is not a digit")
                };
                let blue = match bluereg.captures(play) {
                    None => 0,
                    Some(capt) => capt[1].parse::<i32>().expect("Blue value is not a digit")
                };
                Play {red:red, green:green, blue:blue}
            }).collect();

            GameData {id: id, games: gamedata}
        })
        .collect()
}

fn check_if_valid(game: &GameData, contents: &Play) -> bool {
    game.games.iter().all(|play| {
        play.red <= contents.red && play.green <= contents.green && play.blue <= contents.blue
    })
}

fn sum_all_valid(allgames: &[GameData], contents: &Play) -> i32 {
    allgames
        .iter()
        .filter(|game| check_if_valid(game, contents))
        .map(|game| game.id)
        .sum()
}
