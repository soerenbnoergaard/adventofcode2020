"""
Reverse precedence between * and + (add before multiply).
"""
import re

def main():
    input_file = "../puzzle_input.txt" # 33331817392479
    # input_file = "../test_input1.txt"
    # input_file = "../test_input2.txt"
    # input_file = "../test_input3.txt"
    # input_file = "../test_input4.txt" # 693942
    total = 0
    with open(input_file, "r") as fh:
        for expr in fh:
            total += parse(expr.strip())

    print(f"Total: {total}")

class Num(int):
    """Make the add and multiply operators have inverse precedence by swapping
    the '+' and '*' operators and overwriting their functionality.
    """

    def __add__(self, other):
        return Num(super().__mul__(other))

    def __mul__(self, other):
        return Num(super().__add__(other))

    @classmethod
    def evaluate(cls, expr):
        new = expr.replace("+", "_").replace("*", "+").replace("_", "*")
        new = re.sub(r"(\d+)", r"{:s}(\1)".format(cls.__name__), new)
        return eval(new)

def parse(expr):
    return Num.evaluate(expr)

if __name__ == "__main__":
    main()
