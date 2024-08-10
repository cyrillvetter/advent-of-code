use std::collections::HashMap;
use regex::Regex;
use advent_of_code::Solution;

pub fn part_one(input: &str) -> Solution {
    const ROOT_MONKEY: &str = "root";
    let monkeys = parse(input);
    let res = calculate(ROOT_MONKEY, &monkeys);

    Solution::I64(res)
}

pub fn part_two(_input: &str) -> Solution {
    Solution::Empty
}

fn calculate(name: &str, monkeys: &HashMap<String, Job>) -> i64 {
    let monkey = monkeys.get(name).unwrap();
    match monkey {
        Job::Num(num) => *num,
        Job::Term(a, op, b) => {
            let res_a = calculate(a, monkeys);
            let res_b = calculate(b, monkeys);
            match op {
                '+' => res_a + res_b,
                '-' => res_a - res_b,
                '*' => res_a * res_b,
                '/' => res_a / res_b,
                _ => unreachable!(),
            }
        }
    }
}

fn parse(input: &str) -> HashMap<String, Job> {
    let pattern = Regex::new("^(\\w+)\\s([*+/-])\\s(\\w+)$").unwrap();

    input
        .lines()
        .map(|l| l.split_once(": ").unwrap())
        .map(|s| {
            let job = match s.1.parse::<i64>() {
                Ok(num) => Job::Num(num),
                _ => {
                    let captures = pattern.captures(s.1).unwrap();
                    let a = captures[1].to_owned();
                    let op = captures[2].chars().next().unwrap();
                    let b = captures[3].to_owned();

                    Job::Term(a, op, b)
                },
            };

            (s.0.to_owned(), job)
        })
        .collect()
}

enum Job {
    Num(i64),
    Term(String, char, String),
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 21);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let input = advent_of_code::read_file("inputs", 21);
        assert_eq!(part_one(&input), Solution::I64(81075092088442));
        assert_eq!(part_two(&input), Solution::Empty);
    }
}
