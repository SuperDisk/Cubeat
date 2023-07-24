import sys
import json

lines = sys.stdin.readlines()
loopframe = json.loads(lines[0])
frames = json.loads(lines[1])
# flat = flatten(frames)

total = 0
for idx, frame in enumerate(frames):
    print(f'SECTION "music__music{idx}", ROMX')
    p1, p0 = frame
    print(f"music{idx}::")
    if p1: print("db "+','.join(str(x) for x in p1))
    print("db 0")
    if p0: print("db "+','.join(str(x) for x in p0))
    print("db 0")
    print(f"db LOW(BANK(music{(idx+1) % len(frames)}))")
    print(f"dw music{(idx+1) % len(frames)}")

    total += 2 + len(p0)+len(p1)+1+2

print("total bytes:",total,file=sys.stderr)
