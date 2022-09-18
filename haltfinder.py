import sys
x=sys.argv[1]

with open(x) as f:
    lines = [q.strip() for q in f]

best = 0
current = 0

i = 0
while True:
    #print(current)
    try:
        line = lines[i]
    except:
        if current > best:
            best = current
        break
        
    if 'SECTION' in line:
        if current > best:
            best = current
        current = 0

    if 'halt' in line:
        current += 1

    i += 1
print(best)
