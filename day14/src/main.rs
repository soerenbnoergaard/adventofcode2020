use std::fs;
use std::collections::HashMap;
use std::collections::HashSet;
use regex::Regex;

const MASK_SIZE: usize = 36;

#[derive(PartialEq)]
#[derive(Debug)]
enum Opcode {
    NOP,
    MASK,
    MEM,
}

struct Instruction {
    opcode: Opcode,
    parameter: [i64; 3],
}

type Program = Vec<Instruction>;

fn parse_instruction_mask(line: &str) -> Instruction {
    let re = Regex::new(r#"mask = ([X10]+)"#).unwrap();
    let mut set_mask: i64 = 0;
    let mut clear_mask: i64 = 0;
    let mut float_mask: i64 = 0;

    // Mask instruction
    if re.is_match(line) {
        let caps = re.captures(line).unwrap();

        for (i, value) in (&caps[1]).chars().enumerate() {
            let bit = MASK_SIZE - 1 - i;

            if value == 'X' {
                float_mask |= 1 << bit;
            }
            else if value == '1' {
                set_mask |= 1 << bit;
            }
            else if value == '0' {
                clear_mask |= 1 << bit;
            }
        }

        return Instruction{
            opcode: Opcode::MASK,
            parameter: [set_mask, clear_mask, float_mask],
        };
    }
    else {
        return Instruction{
            opcode: Opcode::NOP,
            parameter: [0, 0, 0],
        };
    }
}

fn parse_instruction_mem(line: &str) -> Instruction {
    let re = Regex::new(r#"mem\[(\d+)\] = (\d+)"#).unwrap();

    // Mask instruction
    if re.is_match(line) {
        let caps = re.captures(line).unwrap();

        let address = caps[1].parse::<i64>().unwrap();
        let value = caps[2].parse::<i64>().unwrap();

        return Instruction{
            opcode: Opcode::MEM,
            parameter: [address, value, 0],
        };
    }
    else {
        return Instruction{
            opcode: Opcode::NOP,
            parameter: [0, 0, 0],
        };
    }
}

fn print_program(program: &Program) {
    for inst in program.iter() {
        println!("{:?}, {}, {}", inst.opcode, inst.parameter[0], inst.parameter[1]);
    }
}

fn execute_program_version1(program: &Program) {
    let mut mem = HashMap::new();
    let mut set_mask = 0;
    let mut clear_mask = 0;

    for inst in program.iter() {
        if inst.opcode == Opcode::MASK {
            set_mask = inst.parameter[0];
            clear_mask = inst.parameter[1];
        }
        else if inst.opcode == Opcode::MEM {
            let address = inst.parameter[0];
            let mut value = inst.parameter[1];
            value |= set_mask;
            value &= !clear_mask;
            mem.insert(address, value);
        }
    };

    let mut sum = 0;
    for (_key, value) in &mem {
        sum += value;
    }
    println!("Sum of all memory: {}", sum);
}

fn execute_program_version2(program: &Program) {
    let mut mem = HashMap::new();
    let mut set_mask = 0;
    // let mut clear_mask = 0;
    let mut float_mask = 0;

    for inst in program.iter() {
        if inst.opcode == Opcode::MASK {
            set_mask = inst.parameter[0];
            // clear_mask = inst.parameter[1];
            float_mask = inst.parameter[2];
        }
        else if inst.opcode == Opcode::MEM {
            let mut address = inst.parameter[0];
            let value = inst.parameter[1];

            // Apply overwriting "set" mask
            address |= set_mask;

            // The "clear" mask means the address is unchanged, so it is not used

            // Given the floating bits, find all valid address variations
            let mut address_variations = HashSet::<i64>::new();
            address_variations.insert(address);

            for bit in 0..36 {
                let here: i64 = 1 << bit;

                if (here & float_mask) != 0 {
                    let mut new_addresses = HashSet::<i64>::new();
                    for addr in address_variations.iter() {
                        new_addresses.insert(*addr & (!here));
                        new_addresses.insert(*addr | here);
                    }
                    address_variations.extend(new_addresses);
                }
            }

            // Insert the value in all address variations
            // println!("Base address = {}", address);
            for addr in address_variations.iter() {
                // println!("    Float address = {}", *addr);
                mem.insert(*addr, value);
            }
        }
    };

    let mut sum = 0;
    for (_key, value) in &mem {
        sum += value;
    }
    println!("Sum of all memory: {}", sum);
}

fn parse_file(filename: String) -> Program {
    let mut program = Program::new();
    let content = fs::read_to_string(filename).expect("Unable to read file");

    for line in content.split("\n") {

        let inst = parse_instruction_mask(&line);
        if inst.opcode != Opcode::NOP {
            program.push(inst);
        }

        let inst = parse_instruction_mem(&line);
        if inst.opcode != Opcode::NOP {
            program.push(inst);
        }
    }
    return program;
}

fn main() {
    // let program = parse_file(String::from("test_input1.txt"));
    // let program = parse_file(String::from("test_input2.txt"));
    let program = parse_file(String::from("puzzle_input.txt"));

    print_program(&program);

    execute_program_version1(&program);
    execute_program_version2(&program);
}
