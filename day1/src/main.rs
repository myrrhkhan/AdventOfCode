use std::collections::HashMap;
use std::fs::File;
use std::io::{self, BufRead};

fn setup(file_name: &str) -> (Vec<i32>, Vec<i32>) {
    let mut list1: Vec<i32> = Vec::new();
    let mut list2: Vec<i32> = Vec::new();

    // Open the file
    if let Ok(file) = File::open(file_name) {
        let reader = io::BufReader::new(file);

        // Iterate over lines in the file
        for content in reader.lines().map_while(Result::ok) {
            // Split each line into two parts
            let parts: Vec<&str> = content.split_whitespace().collect();
            if parts.len() == 2 {
                // Parse the integers and add them to respective lists
                if let (Ok(num1), Ok(num2)) = (parts[0].parse::<i32>(), parts[1].parse::<i32>()) {
                    list1.push(num1);
                    list2.push(num2);
                }
            }
        }
    } else {
        eprintln!("Failed to open the file: {}", file_name);
    }

    list1.sort();
    list2.sort();

    (list1, list2)
}

fn part1(list1: &[i32], list2: &[i32]) -> i32 {
    let mut sum = 0;

    for (a, b) in list1.iter().zip(list2.iter()) {
        sum += (a - b).abs();
    }

    sum
}

fn _part2_attempt1(list1: &[i32], list2: &[i32]) -> i32 {
    // O(nlogn)--logn to find bc binary search, but i have to repeat for every element in list1
    let mut sum = 0;
    let mut counts = HashMap::new();
    for int in list1 {
        if !counts.contains_key(int) {
            println!("calculating {}", int);
            counts.insert(int, _search(*int, list2));
        }
        println!("adding {}", int);
        sum += int * counts.get(int).unwrap();
    }
    sum
}

fn part2_attempt2(list1: &Vec<i32>, list2: &Vec<i32>) -> i32 {
    let mut sum = 0;
    let mut counts = HashMap::new();
    for int in list2 {
        if !counts.contains_key(int) {
            counts.insert(int, 1);
        } else {
            counts.insert(int, counts.get(int).unwrap() + 1);
        }
    }
    for int in list1 {
        match counts.get(int) {
            Some(count) => sum += int * count,
            None => continue,
        }
    }
    sum
}

fn _search(find: i32, list2: &[i32]) -> i32 {
    let mut count = 0;

    let mut lbound: usize = 0;
    let mut rbound: usize = list2.len();
    let mut idx: usize = 0;
    while rbound > lbound {
        idx = (rbound + lbound) / 2;
        if list2[idx] == find {
            count = 1;
            break;
        }
        if list2[idx] > find {
            // idx is too high
            rbound = idx;
            continue;
        }
        if list2[idx] < find {
            // idx is too low
            lbound = idx;
            continue;
        }
    }
    if count != 0 {
        lbound = idx - 1;
        rbound = idx + 1;
        while list2[lbound] == find {
            count += 1;
            if lbound == 0 {
                break;
            }
            lbound -= 1;
        }
        while list2[rbound] == find {
            count += 1;
            if rbound == list2.len() - 1 {
                break;
            }
            rbound += 1;
        }
    }

    count
}

fn main() {
    // let file_name = "test.txt"; // Replace with your file name
    let file_name = "input";
    let (list1, list2) = setup(file_name);
    let result1 = part1(&list1, &list2);
    println!("Function result: {}", result1);
    let result2 = part2_attempt2(&list1, &list2);
    println!("Function result: {}", result2);
}
