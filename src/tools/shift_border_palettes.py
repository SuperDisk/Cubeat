#!/usr/bin/env python3

from sys import argv,stderr


if len(argv) < 3:
    print("Usage: {} input_file output_file".format(argv[0]))


i = 0
with open(argv[1], "rb") as in_file:
    with open(argv[2], "wb") as out_file:
        while in_file.peek(2) != bytes():
            entry = in_file.read(2)
            palette = (entry[1] >> 2) & 7
            # SuperFamiconv begins at palette 0, we need to begin at 4
            palette += 4
            assert palette < 8, "Tile {}: SGB border cannot use more than 4 color palettes! Please reduce the amount of colors.".format(hex(i))
            if palette == 7:
                print("** WARNING: Tile {}: palette #7 is risky to use, SGB features may use it.".format(hex(i)), file=stderr)
            out_file.write(bytes(( entry[0], (entry[1] & ~(7 << 2)) | palette << 2 )))

            i += 2
