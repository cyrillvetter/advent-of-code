use advent_of_code::Solution;

const SIZE: usize = 5;

pub fn part_one(input: &str) -> Solution {
    let mut elements: [usize; SIZE] = [0; SIZE];

    for (index, value) in elements.iter_mut().enumerate() {
        *value = index;
    }

    let mut pos: usize = 0;
    let mut skip_size: usize = 0;
    for v in input.split(",") {
        skip_size += 1;
    }

    println!("{:?}", elements);
    Solution::Empty
}

pub fn part_two(input: &str) -> Solution {
    Solution::Empty
}

fn main() {
    let input = &advent_of_code::read_file("examples", 10);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let input = advent_of_code::read_file("inputs", 10);
        assert_eq!(part_one(&input), Solution::Empty);
        assert_eq!(part_two(&input), Solution::Empty);
    }
}
