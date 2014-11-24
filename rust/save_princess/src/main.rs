use std::io;
use std::num::SignedInt;
use std::ascii::AsciiStr;

#[deriving(Show)]
struct Matrix {
    max_size: uint,
    internal: Vec<Vec<char>>
}

impl Matrix {
    #[allow(dead_code)]
    fn get(&self, x: uint, y: uint) -> Option<char> {
        if x >= self.internal.len() { return None }

        let row = &self.internal[x];
        if y < row.len() { Some(row[y]) } else { None }
    }

    fn search(&self, look_for: char) -> Option<Position> {
        for (y, row) in self.internal.iter().enumerate() {
            for (x, char) in row.iter().enumerate() {
                if look_for == *char {
                    return Some(Position(x, y));
                }
            }
        }

        return None
    }

    fn new(max_size: uint, internal: Vec<Vec<char>>) -> Matrix {
        Matrix { max_size: max_size, internal: internal }
    }
}

#[deriving(Show)]
struct Position(uint, uint);

#[deriving(Show)]
struct Path {
    directions: Vec<PathSegment>
}

#[deriving(Show)]
struct PathSegment {
    steps:     uint,
    direction: Direction
}

impl PathSegment {
    fn formatted_steps(&self) -> Vec<String> {
        // let mut vec = Vec::with_capacity(self.steps);
        let output_direction: String = self.direction.to_string().chars().map(|c| { c.to_uppercase() }).collect();
        range(0, self.steps).map(|_| { output_direction.clone() }).collect()
    }
}

#[deriving(Show)]
enum Direction {
    Left,
    Right,
    Up,
    Down
}

fn main() {
    let matrix        = get_matrix();
    let mario_pos     = get_mario_position(&matrix);
    let peach_pos     = get_peach_position(&matrix);
    let path_to_peach = calculate_path(mario_pos, peach_pos);

    for path_segment in path_to_peach.directions.iter() {
        for step in path_segment.formatted_steps().iter() {
            println!("{}", step);
        }
    }
}

fn calculate_path(from: Position, to: Position) -> Path {
    let Position(from_x, from_y) = from;
    let Position(to_x, to_y) = to;

    let x_direction = if from_x > to_x { Left} else { Right };
    let y_direction = if from_y > to_y { Up }  else { Down };

    let get_steps = |n1: uint, n2: uint| -> uint { (n1.to_int().unwrap() - n2.to_int().unwrap()).abs().to_uint().unwrap() };

    let mut directions = Vec::new();
    let x_path_segment = PathSegment { direction: x_direction, steps: get_steps(from_x, to_x) };
    let y_path_segment = PathSegment { direction: y_direction, steps: get_steps(from_y, to_y) };
    directions.push(x_path_segment);
    directions.push(y_path_segment);

    Path { directions: directions }
}

fn get_matrix() -> Matrix {
    let mut input_vec: Vec<String> = io::stdin().lines().map(|line| {
        line.unwrap().trim().to_string()
    }).collect();

    let first_line = input_vec.remove(0).expect("the input was blank");
    let max_size: uint = from_str(first_line.as_slice()).expect("first line was not an integer");

    let mut internal: Vec<Vec<char>> = Vec::new();
    for line in input_vec.iter() {
        let line_vec = line.chars().collect();
        internal.push(line_vec);
    }

    Matrix::new(max_size, internal)
}

fn get_mario_position(matrix: &Matrix) -> Position {
    matrix.search('m').expect("Mario was not found!")
}

fn get_peach_position(matrix: &Matrix) -> Position {
    matrix.search('p').expect("Peach was not found!")
}
