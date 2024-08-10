use itertools::Itertools;
use advent_of_code::Solution;

pub fn part_one(input: &str) -> Solution {
    Solution::USize(get_message_start(input, 4))
}

pub fn part_two(input: &str) -> Solution {
    Solution::USize(get_message_start(input, 14))
}

fn get_message_start(input: &str, size: usize) -> usize {
    for (i, w) in input.as_bytes().windows(size).enumerate() {
        if w.iter().all_unique() {
            return i + size;
        }
    }

    unreachable!();
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
        assert_eq!(part_one(&input), Solution::USize(1896));
        assert_eq!(part_two(&input), Solution::USize(3452));
    }
}
