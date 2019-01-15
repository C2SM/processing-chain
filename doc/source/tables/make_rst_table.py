"""Making an rst-table can be cumbersome. This script does it for you, given
a csv-file.

Usage: $ python make_rst_table.py example.csv
"""
import csv, sys

from itertools import repeat


def read_csv(filename):
    """Read csv, return list of lists corresponding to rows, columns"""
    with open(filename) as f:
        reader = csv.reader(f, skipinitialspace=True, delimiter=',')
        contents = []
        for row in reader:
            contents.append([str(e) for e in row])

    return contents


def maketable(contents):
    """Take a list of lists corresponding to rows, columns, return
    a string containing the .rst-table.

    >>> maketable([['a', 'b'],['c', 'ddd']])
    <<< "+---+-----+\n| a | b   |\n+---+-----+\n| c | ddd |\n+---+-----+"
    """
    # find longest entry for each column
    max_lengths = [0 for _ in contents[0]]
    for i, row in enumerate(contents):
        assert len(row) == len(max_lengths), ("Row {} has different # of "
                                              "from previous rows".format(i))
        for j, entry in enumerate(row):
            if len(entry) > max_lengths[j]:
                max_lengths[j] = len(entry) + 8

    # assemble contents into list of lines
    lines = []
    i = 0
    for row in contents:
        line = '| '
        for entry, length in zip(row, max_lengths):
            # format variable name
            if i > 0 and i % 3 == 0:
                entry = '``' + entry + '``'
            line += str(entry)
            line += ' ' * (length - len(entry))
            line += ' | '
            i += 1
        lines.append(line)

    # add in row seperator
    seperator = '+'
    for length in max_lengths:
        seperator += (length + 2) * '-' + '+'

    table = [seperator]
    for line in lines:
        table.extend([line, seperator])

    return '\n'.join(table)


if __name__=='__main__':
    contents = read_csv(sys.argv[1])
    table = maketable(contents)
    print(table)
