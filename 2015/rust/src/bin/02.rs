use itertools::Itertools;
use advent_of_code::Solution;

pub fn part_one(input: &str) -> Solution {
    let result = input
        .lines()
        .map(|p| p.split('x').map(|s| s.parse().unwrap()).collect::<Vec<i32>>())
        .map(|mut s| {
            let result = 2 * s[0] * s[1] + 2 * s[1] * s[2] + 2 * s[2] * s[0];
            s.sort();
            result + s[0] * s[1]
        })
        .sum::<i32>();

    Solution::I32(result)
}

pub fn part_two(input: &str) -> Solution {
    let result = input
        .lines()
        .map(|p| p.split('x').map(|s| s.parse().unwrap()).sorted().collect::<Vec<i32>>())
        .map(|s| 2 * s[0] + 2 * s[1] + s[0] * s[1] * s[2])
        .sum::<i32>();

    Solution::I32(result)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 2);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let input = advent_of_code::read_file("inputs", 2);
        assert_eq!(part_one(&input), Solution::I32(1606483));
        assert_eq!(part_two(&input), Solution::I32(3842356));
    }
}
