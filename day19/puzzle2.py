import re
from copy import deepcopy
from pprint import pprint

# INFILE = "test_input1.txt"
INFILE = "test_input2.txt"
# INFILE = "puzzle_input.txt"

def main():
    rules, messages = parse(INFILE)
    rules = update(rules)
    print(f"{len(rules)=}")
    print(f"{len(messages)=}")

    pattern = expand(rules, "0")
    print(pattern)
    num_match = 0
    for message in messages:
        if pattern.match(message):
            # print(message)
            num_match += 1
    print(f"{num_match=}")

def update(rules):
    """Adjust the rules to contain loops"""
    rules["8"] = "42 | 42 8"
    rules["11"] = "42 31 | 42 11 31"
    return rules

def parse(filename):
    rules = {}
    messages = []
    state = 0
    with open(filename, "r") as fh:
        for line in fh:
            line = line.strip()
            if line == "":
                state = 1
            elif state == 0:
                idx, rule = line.split(":")
                rules[idx.strip()] = rule.strip()
            elif state == 1:
                messages.append(line.strip())

    return rules, messages

def expand(rules, index):
    """Return a regex object to match against messages"""

    # Remove citation marks
    for idx in rules:
        rules[idx] = rules[idx].replace('"', '')

    # Search-and-replace all indexes until all decimal values are eliminated.
    rule = " " + rules[index] + " "
    pattern = re.compile(r"(\d+)")
    while m := pattern.search(rule):
        idx = m.group(1)
        old = idx
        new = rules[idx]
        if old in new:
            # Recursion!
            # breakpoint()
            continue
        rule = rule.replace(" "+old+" ", " ( "+new+" ) ")

    pattern = "^" + rule.replace(" ", "") + "$"
    return re.compile(pattern)

if __name__ == "__main__":
    main()

