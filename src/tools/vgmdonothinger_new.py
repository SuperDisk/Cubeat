import sys
import json
from random import randint

lines = sys.stdin.readlines()
loopframe = json.loads(lines[0])
frames = json.loads(lines[1])
# flat = flatten(frames)

numframes = len(frames)

total = 0
this_bank = 0
frameno = 0

while frames:
    print(f'SECTION "music__music{randint(0,9999999999)}", ROMX[$4000]')

    while frames and (this_bank+len(frames[0])+200 < 0x3FFF):
        frame = frames.pop(0)
        p1, p0 = frame
        print(f"music{frameno}::")
        frameno += 1
        if p1: print("db "+','.join(str(x) for x in p1))
        print("db 0")
        if p0: print("db "+','.join(str(x) for x in p0))

        this_bank += 2 + len(p0)+len(p1)

        if frames and (this_bank+len(frames[0])+2 < 0x3FFF):
            print("db 0")
        else:
            print("db 1")

    print(f"db LOW(BANK(music{frameno % numframes}))")
    total += this_bank
    this_bank = 0

print("total bytes:",total,file=sys.stderr)
