use std::cmp::max;
use regex::Regex;
use advent_of_code::Solution;

pub fn part_one(input: &str) -> Solution {
    let mut grid = vec![vec![false; 1000]; 1000];

    let instr_regex = Regex::new("(turn off|turn on|toggle)\\s(\\d+),(\\d+)\\sthrough\\s(\\d+),(\\d+)").unwrap();
    let mut counter = 0;

    for l in input.lines() {
        let captures = instr_regex.captures(l).unwrap();

        let action = &captures[1];
        let from_x = captures[2].parse::<usize>().unwrap();
        let from_y = captures[3].parse::<usize>().unwrap();
        let to_x = captures[4].parse::<usize>().unwrap();
        let to_y = captures[5].parse::<usize>().unwrap();

        for y in grid.iter_mut().skip(from_y).take(to_y - from_y + 1) {
            for x in y.iter_mut().skip(from_x).take(to_x - from_x + 1) {
                let prev = *x;
                let next = match action {
                    "turn on" => true,
                    "turn off" => false,
                    _ => !prev,
                };

                *x = next;

                if !prev && next {
                    counter += 1;
                } else if prev && !next {
                    counter -= 1;
                }
            }
        }
    }

    Solution::I32(counter)
}

pub fn part_two(input: &str) -> Solution {
    let mut grid = vec![vec![0i32; 1000]; 1000];

    let instr_regex = Regex::new("(turn off|turn on|toggle)\\s(\\d+),(\\d+)\\sthrough\\s(\\d+),(\\d+)").unwrap();
    let mut counter = 0;

    for l in input.lines() {
        let captures = instr_regex.captures(l).unwrap();

        let action = &captures[1];
        let from_x = captures[2].parse::<usize>().unwrap();
        let from_y = captures[3].parse::<usize>().unwrap();
        let to_x = captures[4].parse::<usize>().unwrap();
        let to_y = captures[5].parse::<usize>().unwrap();

        for y in grid.iter_mut().skip(from_y).take(to_y - from_y + 1) {
            for x in y.iter_mut().skip(from_x).take(to_x - from_x + 1) {
                let prev = *x;
                let next: i32 = match action {
                    "turn on" => prev + 1,
                    "turn off" => max(0, prev - 1),
                    _ => prev + 2,
                };

                *x = next;

                if next != prev {
                    counter += next - prev;
                }
            }
        }
    }

    Solution::I32(counter)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 6);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let input = advent_of_code::read_file("inputs", 6);
        assert_eq!(part_one(&input), Solution::I32(569999));
        assert_eq!(part_two(&input), Solution::I32(17836115));
    }
}
