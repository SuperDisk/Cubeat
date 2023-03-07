# translated by chatgpt from capcom.c

import json
from itertools import chain
from collections import namedtuple
import sys
from pathlib import Path

MIN_LENGTH  = 3           # minimum reference size
MAX_LENGTH  = 0x7F        # maximum literals / reference size
WINDOW_SIZE = 0xFFF       # sliding window size
FLAG_LTRLS  = 0           # indicates literals
FLAG_PAIR   = 0x80        # indicates length,offset pair
FLAG_EOF    = 0           # end of compressed data

SEQ = namedtuple('SEQ', ('offset', 'length'))

def find(data, offset, size, maxlen):
    """
    find longest sequence of repeated bytes in window
    """
    match = SEQ(offset=0, length=0)
    length = 0
    window = 0

    # initialize sliding window position and loop count
    if offset < WINDOW_SIZE:
        window = 0
    else:
        window = offset - WINDOW_SIZE

    # scan the window
    while window < offset:
        # start new sequence
        length = 0
        # if first bytes match
        if data[window] == data[offset]:
            length += 1
            # check and count others
            try:
                while (data[window + length] == data[offset + length]):
                    # next byte
                    length += 1
                    # avoid match size overflow
                    if length == MAX_LENGTH:
                        break
                    # avoid generating too many bytes
                    if length == maxlen:
                        break
                    # stay in bounds
                    if window + length > size:
                        break
            except IndexError:
                break
        # update if match is found and it's better then previous one
        if (length <= maxlen) and (length >= MIN_LENGTH) and (length > match.length):
            match = SEQ(offset=window, length=length)
        # advance to next byte in window
        window += 1

    return match

def compress():
    outfile = sys.argv[1]

    frames = json.load(sys.stdin)
    framelens = [len(q) for q in frames]
    framedata = list(chain(*frames))
    print(len(frames), "frames in total")

    d = 0
    b = 0
    best = SEQ(offset=0, length=0)

    buffer = bytearray(MAX_LENGTH)
    data = framedata
    size = len(data)
    print("Original file size", len(data), 'bytes')

    frame_need = framelens.pop(0)
    this_bank = 0

    def flush_buffer():
        nonlocal b
        writebytes(bytes([FLAG_LTRLS | b]))
        writebytes(buffer[:b])
        # reset buffer offset
        b = 0

    out_banks = []
    cur_bank = bytearray()

    def writebytes(b):
        nonlocal this_bank, cur_bank
        this_bank += len(b)
        cur_bank.extend(b)

    while d < size:
        if not frame_need:
            if b:
                flush_buffer()

            if not framelens:
                break

            writebytes(bytes([FLAG_EOF]))
            frame_need = framelens.pop(0)

            if this_bank+frame_need >= 0x4000:
                print("splitting bank at", hex(this_bank))
                this_bank = 0
                cur_bank.append(FLAG_EOF) # Double EOF means bank switch
                out_banks.append(cur_bank)
                cur_bank = bytearray()

        # find best match
        best = find(data, d, size, frame_need)

        if best.length > 0:
            # write literals, size and buffer content if its not empty
            if b:
                flush_buffer()
            # write token and offset pair
            writebytes(bytes([FLAG_PAIR | best.length]))
            # stored as negative value
            offset = (best.offset - d) & 0xFF
            writebytes(bytes([offset]))
            offset = ((best.offset - d) & 0xFF00) >> 8
            writebytes(bytes([offset]))
            # adjust data offset
            d += best.length
            frame_need -= best.length
        else:
            # copy one byte to buffer
            buffer[b] = data[d]
            b += 1
            d += 1
            frame_need -= 1
            # write literals if buffer is full or end of data reached
            if b == MAX_LENGTH or d == size:
                flush_buffer()

    # mark end of compressed data
    cur_bank.append(FLAG_EOF)
    out_banks.append(cur_bank)

    with open(outfile, 'w') as f:
        stem = Path(outfile).stem
        for idx, bank in enumerate(out_banks):
            print(f'SECTION "{stem}{idx}", ROMX[$4000]', file=f)
            print(f"{stem}{idx}::", file=f)
            print('db', ','.join(str(x) for x in bank), file=f)
            next_bank = idx+1 if idx != len(out_banks)-1 else 0
            print(f'db BANK({stem}{next_bank})', file=f)

        print("Compressed to", sum(len(x) for x in out_banks), "bytes")

if __name__=="__main__":
    compress()
