use std::collections::HashMap;
use regex::Regex;
use advent_of_code::Solution;

pub fn part_one(input: &str) -> Solution {
    const WIRE_TO_SEARCH: &str = "a";
    let mut computed_wires: HashMap<&str, u16> = HashMap::new();

    let wires: HashMap<_, _> = input
        .lines()
        .map(|l| l.split_once(" -> ").unwrap())
        .map(|(value, wire_name)| (wire_name, value))
        .collect();

    let regex = Regex::new("\\w+( RSHIFT | LSHIFT | OR | AND )\\w+").unwrap();
    let result = trace_wire(WIRE_TO_SEARCH, &wires, &mut computed_wires, &regex);
    Solution::U16(result)
}

pub fn part_two(input: &str) -> Solution {
    const WIRE_TO_SEARCH: &str = "a";
    const WIRE_TO_REPLACE: &str = "b";

    let mut wires: HashMap<_, _> = input
        .lines()
        .map(|l| l.split_once(" -> ").unwrap())
        .map(|(value, wire_name)| (wire_name, value))
        .collect();

    let regex = Regex::new("\\w+( RSHIFT | LSHIFT | OR | AND )\\w+").unwrap();
    let first_result = trace_wire(WIRE_TO_SEARCH, &wires, &mut HashMap::new(), &regex).to_string();

    *wires.get_mut(WIRE_TO_REPLACE).unwrap() = &first_result;
    let result = trace_wire(WIRE_TO_SEARCH, &wires, &mut HashMap::new(), &regex);

    Solution::U16(result)
}

// TODO: Improve performance.
fn trace_wire<'a>(name: &'a str, wires: &HashMap<&'a str, &'a str>, computed_wires: &mut HashMap<&'a str, u16>, regex: &Regex) -> u16 {
    if computed_wires.contains_key(name) {
        return *computed_wires.get(name).unwrap();
    }

    let path = wires.get(name).unwrap();

    let result: u16;

    if regex.is_match(path) {
        let captures = regex.captures(path).unwrap();
        let operator = &captures[1];

        let (left, right) = path.split_once(operator).unwrap();

        let left_number = match left.parse::<u16>() {
            Ok(n) => n,
            Err(_) => trace_wire(left, wires, computed_wires, regex),
        };

        let right_number = match right.parse::<u16>() {
            Ok(n) => n,
            Err(_) => trace_wire(right, wires, computed_wires, regex),
        };

        result = match operator.trim() {
            "AND" => left_number & right_number,
            "LSHIFT" => left_number << right_number,
            "RSHIFT" => left_number >> right_number,
            "OR" => left_number | right_number,
            _ => panic!(),
        }
    } else if path.starts_with("NOT") {
        let (_, o) = path.split_once(" ").unwrap();
        result = match o.parse::<u16>() {
            Ok(n) => !n,
            Err(_) => !trace_wire(o, wires, computed_wires, regex),
        }
    } else {
        result = match path.parse::<u16>() {
            Ok(n) => n,
            Err(_) => trace_wire(path, wires, computed_wires, regex),
        }
    }

    computed_wires.insert(name, result);
    result
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 7);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let input = advent_of_code::read_file("inputs", 7);
        assert_eq!(part_one(&input), Solution::U16(46065));
        assert_eq!(part_two(&input), Solution::U16(14134));
    }
}
