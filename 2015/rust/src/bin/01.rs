use advent_of_code::Solution;

pub fn part_one(input: &str) -> Solution {
    let result = input
        .as_bytes()
        .iter()
        .map(|c| if *c == b'(' { 1 } else { -1 })
        .sum::<i32>();

    Solution::I32(result)
}

pub fn part_two(input: &str) -> Solution {
    let mut floor: i32 = 0;
    for (i, n) in input.as_bytes().iter().map(|c| if *c == b'(' { 1i32 } else { -1i32 }).enumerate() {
        floor += n;

        if floor < 0 {
            return Solution::USize(i + 1);
        }
    };

    unreachable!();
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
        assert_eq!(part_one(&input), Solution::I32(74));
        assert_eq!(part_two(&input), Solution::USize(1795));
    }
}
