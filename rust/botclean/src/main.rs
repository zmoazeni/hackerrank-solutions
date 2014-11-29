use std::io;
use std::str;
use std::io::stdio;

const MAX: uint = 5;

#[deriving(Show)]
struct Matrix {
    internal: Vec<Vec<char>>
}

impl Matrix {
    fn get(&self, position: Position) -> Option<char> {
        let Position(x, y) = position;
        if y >= self.internal.len() { return None }

        let row = &self.internal[y];
        if x < row.len() { Some(row[x]) } else { None }
    }

    fn new(internal: Vec<Vec<char>>) -> Matrix {
        Matrix { internal: internal }
    }
}

#[deriving(Show)]
struct Position(uint, uint);

fn main() {
    let (bot_position, matrix) = build_bot_position_and_matrix();
    let current_value = matrix.get(bot_position).expect("cell doesn't exist");
    let next_move = decide_next_move(bot_position, current_value);
    stdio::println(next_move.as_slice());
}

fn decide_next_move(position: Position, value: char) -> String {
    let Position(x, y) = position;

    match value {
        'd' => "CLEAN",
        '-' | 'b' => {
            if x == MAX - 1 {
                "DOWN"
            } else if y % 2 == 0 {
                "RIGHT"
            } else if x == 0 {
                "DOWN"
            } else {
                "LEFT"
            }
        },
        _ => panic!("Unknown character: {}", value)
    }.to_string()
}

fn build_bot_position_and_matrix() -> (Position, Matrix) {
    let mut stdin = io::stdin();
    let first_line = stdin.read_line().unwrap();
    let bot_position = get_position_from_string(first_line);

    let input_vec: Vec<String> = stdin.lines().map(|line| {
        line.unwrap().trim().to_string()
    }).collect();

    let mut internal: Vec<Vec<char>> = Vec::new();
    for line in input_vec.iter() {
        let line_vec = line.chars().collect();
        internal.push(line_vec);
    }

    (bot_position, Matrix::new(internal))
}

fn get_position_from_string(line: String) -> Position {
    let splits: Vec<&str> = line.split(|c: char| { c == ' '}).map(|l| l.trim()).collect();
    match splits.as_slice() {
        [y, x, ..] => Position(str::from_str(x).expect("y is not a uint"), str::from_str(y).expect("x is not a uint")),
        _ => panic!("the first line wasn't formatted as:\"Y X\"")
    }
}
