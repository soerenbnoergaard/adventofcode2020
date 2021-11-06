package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

const (
	EMPTY = 'L'
	OCCUPIED = '#'
	FLOOR = '.'
)

type layout struct {
	data [][]rune
}

func newLayout(filename string) layout {
	var l layout

	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		var row []rune
		for _, c := range scanner.Text() {
			row = append(row, c)
		}
		l.data = append(l.data, row)
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}


	return l
}

func (l layout) show() {
	for _, row := range l.data {
		for _, seat := range row {
			fmt.Printf("%c", seat)
		}
		fmt.Println("")
	}
	fmt.Println("")
}

func (l1 layout) deepCopy() layout {
	var l2 layout
	for _, row1 := range l1.data {
		var row2 []rune
		for _, seat := range row1 {
			row2 = append(row2, seat)
		}
		l2.data = append(l2.data, row2)
	}
	return l2
}

func (l1 *layout) getNumOccupiedAdjacentSeats(row, col int) int {
	n := 0

	lastRow := len(l1.data)-1
	lastCol := len(l1.data[0])-1

	for _, i := range []int{-1, 0, 1} {
		for _, j := range []int{-1, 0, 1} {
			if (row+i < 0) || (row+i > lastRow) {
				continue
			}
			if (col+j < 0) || (col+j > lastCol) {
				continue
			}
			if (i == 0) && (j == 0) {
				continue
			}
			if (l1.data[row+i][col+j] == OCCUPIED) {
				n += 1
			}
		}
	}
	return n
}

func (l1 *layout) getNumOccupiedLineOfSightSeats(row, col int) int {
	n := 0

	firstRow := 0
	firstCol := 0
	lastRow := len(l1.data)-1
	lastCol := len(l1.data[0])-1

	// Look if the first (non-floor) seat is occupied
	for _, xDir := range []int{-1, 0, 1} {
		for _, yDir := range []int{-1, 0, 1} {
			if (xDir == 0) && (yDir == 0) {
				continue
			}

			x := col
			y := row

			for {
				x = x + xDir
				y = y + yDir

				if (x < firstCol) || (x > lastCol) {
					break
				}
				if (y < firstRow) || (y > lastRow) {
					break
				}

				seat := l1.data[y][x]
				if seat == FLOOR {
					continue
				} else if seat == EMPTY {
					break
				} else if seat == OCCUPIED {
					n += 1
					break
				}
			}
		}
	}

	return n
}

func (l1 *layout) iterate() layout {
	l2 := l1.deepCopy()

	for i, row := range l1.data {
		for j, seat := range row {
			// n := l1.getNumOccupiedAdjacentSeats(i, j)
			// threshold := 4

			n := l1.getNumOccupiedLineOfSightSeats(i, j)
			threshold := 5

			if seat == FLOOR {
				l2.data[i][j] = l1.data[i][j]
			} else if (seat == EMPTY) && (n == 0) {
				l2.data[i][j] = OCCUPIED
			} else if (seat == OCCUPIED) && (n >= threshold) {
				l2.data[i][j] = EMPTY
			} else {
				l2.data[i][j] = l1.data[i][j]
			}
		}
	}
	return l2
}

func (l1 *layout) isEqual(l2 layout) bool {
	for i, row := range l1.data {
		for j := range row {
			if l1.data[i][j] != l2.data[i][j] {
				return false
			}
		}
	}
	return true
}

func (l *layout) getOccupiedSeats() int {
	n := 0
	for _, row := range l.data {
		for _, seat := range row {
			if seat == OCCUPIED {
				n += 1
			}
		}
	}
	return n
}

func main() {
	// l1 := newLayout("test_input1.txt")
	// l1 := newLayout("test_input2.txt")
	// l1 := newLayout("test_input3.txt")
	// l1 := newLayout("test_input4.txt")
	l1 := newLayout("puzzle_input.txt")
	l1.show()

	i := 1
	for {
		fmt.Printf("\nIteration %d\n", i)
		l2 := l1.iterate()
		// l2.show()
		fmt.Printf("Number of occupied seats: %d\n", l2.getOccupiedSeats())

		if l1.isEqual(l2) {
			fmt.Println("Steady state reached!")
			break;
		}
		l1 = l2
		i += 1
	}

}
