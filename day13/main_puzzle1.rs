use std::fs::File;
use std::io::{BufRead, BufReader};

const NO_BUS: i64 = -1;

struct Schedule {
    earliest_departure: i64,
    busses: Vec<i64>,
}

fn parse_file(filename: String) -> Schedule {
    // Open the file in read-only mode (ignoring errors).
    let file = File::open(filename).unwrap();
    let mut reader = BufReader::new(file);
    let mut schedule = Schedule {
        earliest_departure: 0,
        busses: Vec::new(),
    };

    let mut line1 = String::new();
    let mut line2 = String::new();

    // Read the two lines
    let _ = reader.read_line(&mut line1);
    let _ = reader.read_line(&mut line2);

    schedule.earliest_departure = line1.trim().parse::<i64>().unwrap();
    // println!("{}", schedule.earliest_departure);

    for s in line2.trim().split(",") {
        if s == "x" {
            schedule.busses.push(NO_BUS);
            continue;
        }

        schedule.busses.push(s.parse().unwrap());
    }
    return schedule;
}

fn print_schedule(s: &Schedule) {
    println!("Schedule:");
    println!("    Earliest time: {}:", s.earliest_departure);
    println!("    Busses:");
    for bus in s.busses.iter() {
        println!("        {}", bus);
    }
}

fn get_next_bus_time(bus: i64, now: i64) -> i64 {
    let mut t = 0;

    loop {
        if t > now {
            return t;
        }
        t += bus;
    }
}

fn print_fastest_bus(schedule: &Schedule) {
    let mut min_time = -1;
    let mut min_bus = 0;

    for bus in schedule.busses.iter() {
        if *bus == NO_BUS {
            continue;
        }

        let t = get_next_bus_time(*bus, schedule.earliest_departure);

        if (t < min_time) || (min_time < 0) {
            min_time = t;
            min_bus = *bus;
        }

        println!("Bus {}, earliest departure: {}", bus, t)
    }

    println!("Fastest bus is {}. Departs after only {} minutes.", min_bus, min_time-schedule.earliest_departure);
    println!("bus ID * wait time = {}", min_bus*(min_time-schedule.earliest_departure));
}

// fn bus_min(busses: &Vec<i64>) -> i64 {
//     let mut min = i64::MAX;
//
//     for bus in busses.iter() {
//         if *bus == NO_BUS {
//             continue;
//         }
//         if *bus < min {
//             min = *bus;
//         }
//     }
//     return min;
// }
//
// fn bus_max(busses: &Vec<i64>) -> i64 {
//     let mut max = i64::MIN;
//
//     for bus in busses.iter() {
//         if *bus == NO_BUS {
//             continue;
//         }
//         if *bus > max {
//             max = *bus;
//         }
//     }
//     return max;
// }
//
// fn bus_idx_max(busses: &Vec<i64>) -> i64 {
//     let target = bus_max(&busses);
//     let mut result: i64 = 0;
//
//     for (i, bus) in busses.iter().enumerate() {
//         if target == *bus {
//             result = i as i64;
//             break;
//         }
//     }
//     return result;
// }
//
// fn print_one_minute_departure_schedule_naive(busses: &Vec<i64>) {
//     let t_step = 1;
//     let mut t: i64 = 0;
//     let mut print_counter: i64 = 0;
//
//     loop {
//         if print_counter == 0 {
//             println!("t = {}", t);
//             print_counter = 100000000;
//         }
//         else {
//             print_counter -= 1;
//         }
//
//         // Check that for this timestamp, the busses depart at one minute intervals
//         let mut discard = false;
//
//         // t += k_max;
//         for (i, bus) in busses.iter().enumerate() {
//             let t_offset = i as i64;
//
//             if *bus == -1 {
//                 continue
//             }
//
//             if ((t + t_offset) % *bus) != 0 {
//                 discard = true;
//                 break
//             }
//         }
//
//         if !discard {
//             println!("Found 1-minute schedule at t = {}", t);
//             return
//         }
//
//         // t += t_step;
//         t += 7;
//     }
// }
//
// fn print_one_minute_departure_schedule(busses: &Vec<i64>) {
//
// }
//

fn main() {
    // let schedule = parse_file(String::from("test_input1.txt"));
    let schedule = parse_file(String::from("puzzle_input.txt"));
    print_schedule(&schedule);
    print_fastest_bus(&schedule);
    // print_one_minute_departure_schedule_naive(&schedule.busses);
    // print_one_minute_departure_schedule(&schedule.busses);
}
