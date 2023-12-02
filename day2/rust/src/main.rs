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
    let bag = Play {
        red: 12,
        green: 13,
        blue: 14,
    };
    let testinput = parse_input("test.txt");
    for i in &testinput {
        println!("{i:?}");
    }
    let testoutput = sum_all_valid(&testinput, &bag);
    let testpowersum = powerset_sum_functional(&testinput);
    println!(
        "Test Output: {}, Test Powerset Sum: {}",
        testoutput, testpowersum
    );

    let realinput = parse_input("input.txt");
    let realoutput = sum_all_valid(&realinput, &bag);
    let realpowersum = powerset_sum_functional(&realinput);
    println!(
        "Real Output: {}, Real Powerset Sum: {}",
        realoutput, realpowersum
    );
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
            let gamedata: Vec<_> = gameinfolst
                .map(|play| {
                    let red = match redreg.captures(play) {
                        None => 0,
                        Some(capt) => capt[1].parse::<i32>().expect("Red value is not a digit"),
                    };
                    let green = match greenreg.captures(play) {
                        None => 0,
                        Some(capt) => capt[1].parse::<i32>().expect("Green value is not a digit"),
                    };
                    let blue = match bluereg.captures(play) {
                        None => 0,
                        Some(capt) => capt[1].parse::<i32>().expect("Blue value is not a digit"),
                    };
                    Play { red, green, blue }
                })
                .collect();

            GameData {
                id,
                games: gamedata,
            }
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

fn min_value(game: &GameData) -> Play {
    let mut min_red = 0;
    let mut min_green = 0;
    let mut min_blue = 0;

    for play in &game.games {
        if min_red < play.red {
            min_red = play.red;
        }
        if min_green < play.green {
            min_green = play.green;
        }
        if min_blue < play.blue {
            min_blue = play.blue;
        }
    }

    Play {
        red: min_red,
        green: min_green,
        blue: min_blue,
    }
}

fn powerset_sum(allgames: &[GameData]) -> i32 {
    allgames
        .iter()
        .map(|game| {
            let play = min_value(game);
            play.red * play.green * play.blue
        })
        .sum()
}

fn powerset_sum_functional(allgames: &[GameData]) -> i32 {
    allgames
        .iter()
        .map(|game| {
            let play = game.games.iter().fold(
                Play {
                    red: 0,
                    green: 0,
                    blue: 0,
                },
                |mut acc, e| {
                    if acc.red < e.red {
                        acc.red = e.red;
                    }
                    if acc.green < e.green {
                        acc.green = e.green
                    }
                    if acc.blue < e.blue {
                        acc.blue = e.blue;
                    }
                    acc
                },
            );
            play.red * play.green * play.blue
        })
        .sum()
}
