with open("inputs/day3.txt") as file:
    lines = file.read().splitlines()
get_priority = lambda x: ord(x)-38 if x.isupper() else ord(x)-96
print("Part 1\n", len ([get_priority(list(set(line[:int(len(line)/2)]).intersection(set(line[int(len(line)/2):])))[0]) for line in lines]), "\n", "Part 2\n", sum([get_priority(list(set.intersection(*[set(x) for x in lines[n*3:n*3+3]]))[0]) for n in range(int(len(lines)/3))]))