use advent_of_code::Solution;
use itertools::Itertools;

pub fn part_one(input: &str) -> Solution {
    let nums = input
        .lines()
        .map(|l| l.parse::<u32>().unwrap())
        .collect_vec();

    let mut smallest_qe = u32::MAX;

    for i in 2.. {
        for c in nums.iter().combinations(i) {
            let filtered = nums.iter().filter(|n| !c.contains(n)).collect_vec();

            for inner_combinations in (1..filtered.len()) {
                let sum_2 = 
            }
        }
    }

    Solution::Empty
}

pub fn part_two(input: &str) -> Solution {
    Solution::Empty
}

fn main() {
    let input = &advent_of_code::read_file("examples", 24);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let input = advent_of_code::read_file("inputs", 24);
        assert_eq!(part_one(&input), Solution::Empty);
        assert_eq!(part_two(&input), Solution::Empty);
    }
}
