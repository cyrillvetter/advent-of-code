use itertools::Itertools;
use advent_of_code::Solution;

pub fn part_one(input: &str) -> Solution {
    let mut cycles = 1;
    let mut x = 1;
    let mut signal_strength = 0;

    for l in input.lines() {
        let mut local_cycles = 1;
        let mut v = 0;

        if l.starts_with("addx") {
            local_cycles = 2;
            v = l.split_once(' ').unwrap().1.parse::<i32>().unwrap();
        }

        for c in cycles..cycles + local_cycles {
            if (c - 20) % 40 == 0 {
                signal_strength += cycles * x;
            }

            cycles += 1;
        }

        x += v;
    }

    Solution::I32(signal_strength)
}

pub fn part_two(input: &str) -> Solution {
    let mut cycles = 1;
    let mut x = 1;
    let mut crt: Vec<char> = Vec::new();

    for l in input.lines() {
        let mut local_cycles = 1;
        let mut v = 0;

        if l.starts_with("addx") {
            local_cycles = 2;
            v = l.split_once(' ').unwrap().1.parse::<i32>().unwrap();
        }

        for c in cycles..cycles + local_cycles {
            let sprite_pos = x + (((c - 1) / 40) * 40);
            if sprite_pos >= c - 2 && sprite_pos <= c {
                crt.push('#');
            } else {
                crt.push('.');
            }

            cycles += 1;
        }

        x += v;
    }

    let mut output = crt.chunks(40).map(|c| c.iter().collect::<String>()).join("\n");
    output.insert(0, '\n');
    Solution::Str(output)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 10);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let input = advent_of_code::read_file("inputs", 10);
        assert_eq!(part_one(&input), Solution::I32(14320));
        let expected_part2 = "\n\
            ###...##..###..###..#..#..##..###....##.\n\
            #..#.#..#.#..#.#..#.#.#..#..#.#..#....#.\n\
            #..#.#....#..#.###..##...#..#.#..#....#.\n\
            ###..#....###..#..#.#.#..####.###.....#.\n\
            #....#..#.#....#..#.#.#..#..#.#....#..#.\n\
            #.....##..#....###..#..#.#..#.#.....##..";
        assert_eq!(part_two(&input), Solution::Str(String::from(expected_part2)));
    }
}
