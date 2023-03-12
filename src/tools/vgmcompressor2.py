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
FLAG_MEGAFRAME = 0x80
FLAG_RESET_DECOMPRESSOR = 0x81

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

    # frames = json.load(sys.stdin)
    loopframe, frames = [json.loads(x) for x in sys.stdin]

    framelens = [len(q)-1 for q in frames]
    framedata = list(chain(*[frame[:-1] for frame in frames]))
    print(len(frames), "frames in total")

    d = 0
    b = 0
    best = SEQ(offset=0, length=0)

    buffer = bytearray(MAX_LENGTH)
    data = framedata
    size = len(data)
    print("Original file size", len(data), 'bytes')

    frame_need = 0
    this_bank = 0

    def flush_buffer():
        nonlocal b
        writebytes(bytes([FLAG_LTRLS | b]))
        writebytes(buffer[:b])
        # reset buffer offset
        b = 0

    out_banks = []
    cur_bank = []

    def writebytes(b):
        nonlocal this_bank, cur_bank
        this_bank += len(b)
        cur_bank.extend(b)

    def write_megaframe_ref(mf):
        nonlocal this_bank, cur_bank
        this_bank += 2
        cur_bank.append((mf,))

    megaframes = []

    loopbank = None
    loopbyte = None

    fprocessed = -1
    while d < size:
        if not frame_need:
            fprocessed += 1
            if b:
                flush_buffer()

            if not framelens:
                break

            if cur_bank and not isinstance(cur_bank[-1], tuple):
                writebytes(bytes([FLAG_EOF]))

            frame_need = framelens.pop(0)

            if this_bank+frame_need >= 0x4000-20:
                print("splitting bank at", hex(this_bank))
                this_bank = 0
                cur_bank.append(FLAG_EOF) # Double EOF means bank switch
                out_banks.append(cur_bank)
                cur_bank = []

            if fprocessed == loopframe:
                print("resetting compressor")
                del data[:d]
                size -= d
                d = 0
                writebytes(bytes([FLAG_RESET_DECOMPRESSOR]))

                loopbank = len(out_banks)
                loopbyte = len(cur_bank)-1 # include the reset command

            if frames[fprocessed][-1] == "mega":
                print("Making megaframe",frame_need)
                writebytes(bytes([FLAG_MEGAFRAME]))
                megaframes.append(data[d:d+frame_need])
                del data[d:d+frame_need]
                size -= frame_need
                write_megaframe_ref(len(megaframes)-1)
                this_bank += len(megaframes[-1])
                frame_need = 0
                continue

            elif frames[fprocessed][-1] == "regular":
                pass
                # print('beginning normal frame', fprocessed, frames[fprocessed])
                # print("db", ','.join(str(x) for x in data[d:d+frame_need]))
                # if data[d+1]!=100: input()
                # if data[d] not in [17,201]:input("seriously wrong 2")
            else:
                input("seriously wrong")

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

    # print(len(megaframes), 'megaframes')
    # print(d,size)

    # mark end of compressed data
    if not isinstance(cur_bank[-1], tuple):
        cur_bank.append(FLAG_EOF)
    cur_bank.append(FLAG_EOF)
    out_banks.append(cur_bank)

    def asm(el):
        if isinstance(el, tuple):
            (x,) = el
            return f"LOW(megaframe{x}),HIGH(megaframe{x})"
        else:
            return str(el)

    with open(outfile, 'w') as f:
        stem = Path(outfile).stem
        for idx, bank in enumerate(out_banks):
            print(f'SECTION "{stem}{idx}", ROMX[$4000]', file=f)
            print(f"{stem}{idx}::", file=f)
            print('db', ','.join(asm(x) for x in bank), file=f)
            last = idx == len(out_banks)-1
            next_bank = loopbank if last else idx+1
            next_byte = 0x4000+loopbyte if last else 0x4000
            print(f'db BANK({stem}{next_bank})', file=f)
            print(f'dw {next_byte}', file=f)


            for el in bank:
                if isinstance(el, tuple):
                    (x,) = el
                    print(f"megaframe{x}:", file=f)
                    print('db', ','.join(str(q) for q in megaframes[x]), file=f)

        print("Compressed to", sum(len(x) for x in out_banks+megaframes), "bytes")

    # with open('/home/npfaro/dump.dat', 'wb') as f:
    #     for bank in out_banks:
    #         f.write(bytes(bank))

if __name__=="__main__":
    compress()