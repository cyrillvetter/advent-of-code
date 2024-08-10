use itertools::Itertools;
use advent_of_code::Solution;

pub fn part_one(input: &str) -> Solution {
    let result = input
        .split("\n\n")
        .map(|v| v.lines().map(|i| i.parse::<i32>().unwrap()).sum::<i32>())
        .max().unwrap();

    Solution::I32(result)
}

pub fn part_two(input: &str) -> Solution {
    let result = input
        .split("\n\n")
        .map(|v| v.lines().map(|i| i.parse::<i32>().unwrap()).sum::<i32>())
        .sorted_by(|a, b| Ord::cmp(&b, &a)) // Sort in descending order
        .take(3)
        .sum::<i32>();

    Solution::I32(result)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 1);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let input = advent_of_code::read_file("inputs", 1);
        assert_eq!(part_one(&input), Solution::I32(70613));
        assert_eq!(part_two(&input), Solution::I32(205805));
    }
}
