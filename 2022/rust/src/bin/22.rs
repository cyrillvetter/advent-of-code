use itertools::Itertools;
use advent_of_code::Solution;

pub fn part_one(input: &str) -> Solution {
    let (grid_part, instructions_part) = input.split_once("\n\n").unwrap();
    let instructions = get_instructions(instructions_part);
    let grid = grid_part.lines().map(|l| l.as_bytes()).collect_vec();

    let start_x = grid[0].iter().position(|c| *c == b'.').unwrap();
    let mut location: (usize, usize) = (start_x, 0);
    let mut direction = Direction::Right;

    for instruction in instructions {
        match instruction {
            Instruction::Move(tiles) => {
                match direction {
                    Direction::Right => {
                        let row_start = grid.iter().
                    }
                    Direction::Down => {}
                    Direction::Left => {}
                    Direction::Up => {}
                }
            }
            rotation => direction = direction.turn(rotation)
        }
    }

    Solution::Empty
}

pub fn part_two(input: &str) -> Solution {
    Solution::Empty
}

fn get_instructions(s: &str) -> Vec<Instruction> {
    let mut instructions: Vec<Instruction> = Vec::new();
    let bytes = s.as_bytes();
    let mut last_num = 0;

    for i in 0..s.len() {
        if bytes[i] == b'R' || bytes[i] == b'L' {
            let parsed = s[last_num..i].parse().unwrap();
            instructions.push(Instruction::Move(parsed));
            instructions.push(if bytes[i] == b'R' { Instruction::R } else { Instruction::L });
            last_num = i + 1;
        }
    }

    instructions
}

#[derive(Debug)]
enum Instruction {
    Move(usize),
    R,
    L
}

enum Direction {
    Right, Down, Left, Up
}

impl Direction {
    fn turn(self, instr: Instruction) -> Self {
        use Instruction::*;
        use Direction::*;

        if instr == L  {
            match self {
                Right => Up,
                Down => Right,
                Left => Down,
                Up => Left,
            }
        } else {
            match self {
                Right => Down,
                Down => Left,
                Left => Up,
                Up => Right
            }
        }
    }
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 22);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let input = advent_of_code::read_file("inputs", 22);
        assert_eq!(part_one(&input), Solution::Empty);
        assert_eq!(part_two(&input), Solution::Empty);
    }
}
