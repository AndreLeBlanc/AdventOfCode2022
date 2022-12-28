import pathlib
import sys

from typing import TypeAlias

# Type aliases
Coordinate: TypeAlias = tuple[int, int]
Instruction: TypeAlias = tuple[str, int]


def parse(puzzle_input: str) -> list[Instruction]:
    """Parse input"""
    return [(line[0], int(line[2:])) for line in puzzle_input.splitlines()]


def move_head(curr_pos: Coordinate, direction: Instruction) -> list[Coordinate]:
    """ Move head knot based on instruction given """
    positions = []
    x, y = curr_pos
    d, a = direction
    for n in range(a):
        match d:
            case "R":
                x += 1
            case "U":
                y += 1
            case "L":
                x -= 1
            case "D":
                y -= 1
        positions.append((x, y))
    return positions


def neighbours(pos: Coordinate) -> list[Coordinate]:
    """Return coords of neighbours (including diagonals and pos)"""
    x0, y0 = pos
    # candidates = [
    return [
        (x0, y0),
        (x0 - 1, y0),
        (x0 + 1, y0),
        (x0, y0 - 1),
        (x0, y0 + 1),
        (x0 - 1, y0 - 1),
        (x0 + 1, y0 + 1),
        (x0 - 1, y0 + 1),
        (x0 + 1, y0 - 1),
    ]


def part1(data: list[Instruction]) -> int:
    """Solve part 1"""
    grid: dict[Coordinate, int] = {}
    head_pos: Coordinate = (0, 0)
    tail_pos: Coordinate = (0, 0)
    # Set initial position of tail in grid
    grid.setdefault(tail_pos, 1)
    for instruct in data:
        grid.setdefault(head_pos, 0)
        moves = move_head(head_pos, instruct)
        for move in moves:
            grid.setdefault(move, 0)
            prev_move: Coordinate = head_pos
            head_pos = move
            if tail_pos not in neighbours(head_pos):
                tail_pos = prev_move
                grid[tail_pos] = 1
    return sum(v for v in grid.values() if v == 1)


def move_knot(head: Coordinate, knot: Coordinate) -> Coordinate:
    """ Move knot releative to "head" """
    hr, hc = head
    kr, kc = knot
    if hr == kr:
        kc = kc - 1 if hc < kc else kc + 1
    elif hc == kc:
        kr = kr - 1 if hr < kr else kr + 1
    else:
        # Deal with diagonal movement
        if hc > kc and hr > kr:
            kr += 1
            kc += 1
        elif hc < kc and hr < kr:
            kr -= 1
            kc -= 1
        elif hc > kc and hr < kr:
            kr -= 1
            kc += 1
        elif hc < kc and hr > kr:
            kr += 1
            kc -= 1
    return (kr, kc)


def update_knots(
    knots: dict[int, Coordinate], head: Coordinate, grid: dict[Coordinate, int]
) -> None:
    """ Update position of all knots """
    previous: Coordinate = head
    tail: int = len(knots) - 1
    for n in range(len(knots)):
        if knots[n] not in neighbours(previous):
            knots[n] = move_knot(previous, knots[n])
        previous = knots[n]
        if n == tail:
            grid.setdefault(knots[n], 1)


def part2(data: list[Instruction]) -> int:
    """Solve part 2"""
    grid: dict[Coordinate, int] = {}
    head_pos: Coordinate = (0, 0)
    knot_pos: dict[int, Coordinate] = {n: (0, 0) for n in range(9)}
    for instruct in data:
        moves: list[Coordinate] = move_head(head_pos, instruct)
        for move in moves:
            head_pos = move
            update_knots(knot_pos, head_pos, grid)
    return len(grid)


def solve(puzzle_input: str) -> tuple[int, int]:
    """Solve the puzzle for the given input"""
    data: list[Instruction] = parse(puzzle_input)
    solution1: int = part1(data)  # Correct answer was 6486 (with my data)
    solution2: int = part2(data)  # Correct answer was 2678 (with my data)

    return solution1, solution2


if __name__ == "__main__":
    for path in sys.argv[1:]:
        print(f"{path}:")
        puzzle_input = pathlib.Path(path).read_text().strip()
        solutions = solve(puzzle_input)
        print("\n".join(str(solution) for solution in solutions))
