import re
from pprint import pprint

def load(infile):
    with open(infile, "r") as fh:
        content = fh.read()
    pattern = re.compile(r"Tile\s(\d+):\n(.*?)\n^$", re.X|re.M|re.S)
    tiles = {int(X): Y for X, Y in pattern.findall(content)}
    return tiles

def rotate(tile):
    """Rotate 90 degrees counter-clockwise."""
    old = tile.split("\n")
    N = len(old)
    new = [["." for n in range(N)] for m in range(N)]

    for m, row in enumerate(old):
        for n, cell in enumerate(row):
            new[N-1-n][m] = cell

    new = "\n".join(["".join(row) for row in new])
    return new

def mirror(tile):
    """Mirror across the y-axis."""
    old = tile.split("\n")
    N = len(old)
    new = [["." for n in range(N)] for m in range(N)]

    for m, row in enumerate(old):
        for n, cell in enumerate(row):
            new[m][N-1-n] = cell

    new = "\n".join(["".join(row) for row in new])
    return new

def main():
    test_tile = """\
a12345678b
0123456789
0123456789
0123456789
0123456789
0123456789
0123456789
0123456789
0123456789
c12345678d"""

    tiles = load("test_input1.txt")
    # tiles = load("puzzle_input1.txt")

    print(f"Number of tiles: {len(tiles)}")

    # print(tiles[2311])
    # print("")
    # print(rotate(tiles[2311]))
    pass

if __name__ == "__main__":
    main()


