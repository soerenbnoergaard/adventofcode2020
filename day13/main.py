from collections import namedtuple
from math import gcd

# For each bus, determine the time to next departure
def calculate_wait_time(bus_id, time_now):
    return bus_id - (time_now % bus_id)

def puzzle1(input_file):
    with open(input_file, "r") as fh:
        time_now = int(fh.readline())
        busses = []
        for bus_id in fh.readline().split(","):
            if bus_id == "x":
                continue
            busses.append(int(bus_id))

    wait_time = {X: calculate_wait_time(X, time_now) for X in busses}
    fastest_bus = min([(t, b) for b, t in wait_time.items()])
    print(f"{fastest_bus=}")
    print(f"Puzzle solution: {fastest_bus[0]*fastest_bus[1]}")

Bus = namedtuple("Bus", ["id", "offset"])
def check_schedule_offsets_ok(busses, t0):
    for bus in busses:
        if ((t0 + bus.offset) % bus.id) != 0:
            return False
    return True

def puzzle2(input_file):
    """This requires the use of the Chinese Remainder Theorem. Online solvers
    exist for this type of problem:

    https://www.dcode.fr/chinese-remainder

    I will format the puzzle input to match that of the solver.
    """

    with open(input_file, "r") as fh:
        time_now = int(fh.readline())
        busses = []
        for n, value in enumerate(fh.readline().split(",")):
            if value == "x":
                continue
            busses.append(Bus(id=int(value), offset=n))

    # Formatting the input for a CRT solver
    print("Type this into a Chinese Remainder Theorem solver:")
    print("")
    p = lambda s1, s2: print("{:>12s} | {:>9s}".format(s1, s2))
    p("Remainders A", "Modulos B")
    p("------------", "---------")
    for bus in busses:
        p(str(-bus.offset), str(bus.id))
    print("")

def main():
    puzzle1("puzzle_input.txt")
    puzzle2("puzzle_input.txt") # Solution: 1010182346291467

if __name__ == "__main__":
    main()

