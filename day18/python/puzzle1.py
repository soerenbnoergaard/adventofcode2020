"""
No precedence between + and *. Whichever comes first, gets evaluated first.
"""
import re

def main():
    input_file = "../puzzle_input.txt" # 209335026987
    # input_file = "../test_input1.txt" # 2245406496
    # input_file = "../test_input2.txt" # 51
    # input_file = "../test_input3.txt" # 26335

    total = 0
    with open(input_file, "r") as fh:
        for expr in fh:
            total += parse(expr)

    print(f"Total: {total}")

class Num(int):
    """Make add and multiply have same precedence by changing their operators
    to some that have equal precedence and overwriting their functionality.
    """

    def __floordiv__(self, other):
        return Num(super().__add__(other))

    def __mul__(self, other):
        return Num(super().__mul__(other))

    @classmethod
    def evaluate(cls, expr):
        new = expr.replace("+", "//").replace("*", "*")
        new = re.sub(r"(\d+)", r"{:s}(\1)".format(cls.__name__), new)
        return eval(new)

def parse(expr):
    return Num.evaluate(expr)

if __name__ == "__main__":
    main()
