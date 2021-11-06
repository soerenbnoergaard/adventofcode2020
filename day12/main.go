package main

import (
	"bufio"
	"fmt"
	"log"
	"regexp"
	"os"
	"strconv"
)

type Instruction struct {
	action byte
	value int
}

type Program []Instruction

type Position struct {
	direction byte
	xPosition int
	yPosition int
	xWaypoint int
	yWaypoint int
}

func NewProgram(filename string) Program {
	var p Program
	pattern := regexp.MustCompile(`([NSEWLRF])(\d+)`)

	file, err := os.Open(filename)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		m := pattern.FindStringSubmatch(line)

		var inst Instruction
		inst.action = byte(m[1][0])
		inst.value, _ = strconv.Atoi(m[2])
		p = append(p, inst)
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	return p
}

func (p Position) Move(inst Instruction) Position {
	pos := p
	switch inst.action {
	case 'N':
		pos.yPosition += inst.value
	case 'S':
		pos.yPosition -= inst.value
	case 'E':
		pos.xPosition += inst.value
	case 'W':
		pos.xPosition -= inst.value
	case 'L':
		switch pos.direction {
		case 'N':
			switch inst.value {
			case 90:
				pos.direction = 'W'
			case 180:
				pos.direction = 'S'
			case 270:
				pos.direction = 'E'
			}
		case 'S':
			switch inst.value {
			case 90:
				pos.direction = 'E'
			case 180:
				pos.direction = 'N'
			case 270:
				pos.direction = 'W'
			}
		case 'E':
			switch inst.value {
			case 90:
				pos.direction = 'N'
			case 180:
				pos.direction = 'W'
			case 270:
				pos.direction = 'S'
			}
		case 'W':
			switch inst.value {
			case 90:
				pos.direction = 'S'
			case 180:
				pos.direction = 'E'
			case 270:
				pos.direction = 'N'
			}
		}
	case 'R':
		switch pos.direction {
		case 'N':
			switch inst.value {
			case 90:
				pos.direction = 'E'
			case 180:
				pos.direction = 'S'
			case 270:
				pos.direction = 'W'
			}
		case 'S':
			switch inst.value {
			case 90:
				pos.direction = 'W'
			case 180:
				pos.direction = 'N'
			case 270:
				pos.direction = 'E'
			}
		case 'E':
			switch inst.value {
			case 90:
				pos.direction = 'S'
			case 180:
				pos.direction = 'W'
			case 270:
				pos.direction = 'N'
			}
		case 'W':
			switch inst.value {
			case 90:
				pos.direction = 'N'
			case 180:
				pos.direction = 'E'
			case 270:
				pos.direction = 'S'
			}
		}
	case 'F':
		switch pos.direction {
		case 'N':
			pos.yPosition += inst.value
		case 'S':
			pos.yPosition -= inst.value
		case 'E':
			pos.xPosition += inst.value
		case 'W':
			pos.xPosition -= inst.value
		}
	}
	return pos
}

func (p Position) MoveWaypoint(inst Instruction) Position {
	pos := p
	switch inst.action {
	case 'N':
		pos.yWaypoint = p.yWaypoint + inst.value
	case 'S':
		pos.yWaypoint = p.yWaypoint - inst.value
	case 'E':
		pos.xWaypoint = p.xWaypoint + inst.value
	case 'W':
		pos.xWaypoint = p.xWaypoint - inst.value
	case 'L':
		switch inst.value {
		case 90:
			pos.xWaypoint = -p.yWaypoint
			pos.yWaypoint = p.xWaypoint
		case 180:
			pos.xWaypoint = -p.xWaypoint
			pos.yWaypoint = -p.yWaypoint
		case 270:
			pos.xWaypoint = p.yWaypoint
			pos.yWaypoint = -p.xWaypoint
		}
	case 'R':
		switch inst.value {
		case 90:
			pos.xWaypoint = p.yWaypoint
			pos.yWaypoint = -p.xWaypoint
		case 180:
			pos.xWaypoint = -p.xWaypoint
			pos.yWaypoint = -p.yWaypoint
		case 270:
			pos.xWaypoint = -p.yWaypoint
			pos.yWaypoint = p.xWaypoint
		}
	case 'F':
		pos.xPosition += inst.value * p.xWaypoint
		pos.yPosition += inst.value * p.yWaypoint
	}
	return pos
}

func (p Program) Execute(start Position) Position {
	pos := start

	for _, inst := range p {
		pos = pos.Move(inst)
	}
	return pos
}

func (p Program) ExecuteWaypoint(start Position) Position {
	pos := start

	for _, inst := range p {
		pos = pos.MoveWaypoint(inst)
	}

	return pos
}

func abs(x int) int {
	if (x < 0) {
		return -x
	} else {
		return x
	}
}

func (p Position) ManhattanDistance(ref Position) int {
	return abs(p.xPosition - ref.xPosition) + abs(p.yPosition - ref.yPosition)
}

func (p Program) Show() {
	fmt.Println("Program:")
	for _, v := range p {
		fmt.Printf("    %c: %5d\n", v.action, v.value)
	}
}

func (p Position) Show() {
	fmt.Println("Position:")
	fmt.Printf("    xPosition: %5d\n", p.xPosition)
	fmt.Printf("    yPosition: %5d\n", p.yPosition)
	fmt.Printf("    direction: %5c\n", p.direction)
	fmt.Printf("    xWaypoint: %5d\n", p.xWaypoint)
	fmt.Printf("    yWaypoint: %5d\n", p.yWaypoint)
}

func main() {
	fmt.Println("Exercise 12")

	// program := NewProgram("test_input1.txt")
	program := NewProgram("puzzle_input.txt")

	start := Position{
		direction: 'E',
		xPosition: 0,
		yPosition: 0,
		xWaypoint: 10,
		yWaypoint: 1,
	}
	start.Show()

	// stop := program.Execute(start)
	stop := program.ExecuteWaypoint(start)

	stop.Show()

	fmt.Printf("Manhattan distance: %d\n", stop.ManhattanDistance(start))
}
