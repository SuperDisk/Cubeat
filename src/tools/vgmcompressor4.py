import sys
import json
from collections import Counter, defaultdict, OrderedDict
from pprint import pprint
from copy import deepcopy
from itertools import takewhile

def flatten(lst):
    def wack(ls):
        for el in ls:
            if isinstance(el, list):
                yield from wack(el)
            else:
                yield el
    return list(wack(lst))

def loaddata(fname):
    with open(fname, 'rb') as f:
        lines = f.readlines()
    loopframe = json.loads(lines[0])
    data = json.loads(lines[1])
    return data, flatten(data)

def loaddata2():
    lines = sys.stdin.readlines()
    loopframe = json.loads(lines[0])
    data = json.loads(lines[1])
    return data, flatten(data)

def count_substrings3(s, n):
    count_dict = defaultdict(int)
    i = 0
    while i <= len(s) - n:  # loop while there is space for a substring of length n
        substr = tuple(s[i:i+n])
        count_dict[substr] += 1
        i += n  # move to the end of the current substring
    return dict(sorted(count_dict.items(), key=lambda item: item[1], reverse=True)[:300])

def comp(data, table):
    data=deepcopy(data)
    out = []
    diduse = set()
    emitted = 0
    while data:
        if (emitted % 2) == 0:
            for n in [8,6,4]:
                idx = table.get(tuple(data[:n]), None)
                if idx:
                    emitted += n
                    diduse.add(tuple(data[:n]))
                    del data[:n]
                    out.append((idx,))
                    break
            else:
                emitted += 1
                out.append(data[0])
                del data[0]
        else:
            emitted += 1
            out.append(data[0])
            del data[0]
    return out, diduse

def decomp(data, table):
    table = {v:k for k,v in table.items()}
    # pprint(table)
    out = []
    for el in data:
        if isinstance(el, tuple):
            out += table[el[0]]
        else:
            out.append(el)
    return out

def sizeof(q):
    return sum(2 if isinstance(el,tuple) else 1 for el in q)

def go():
    # frames, flat = loaddata('../res/music/zen.cooked2')
    frames, flat = loaddata2()

    big = {}
    for n in [4,6,8]:
        big |= count_substrings3(flat, n)

    for k in list(big):
        if big[k] < 2:
            del big[k]

    table = {}
    ctr = 0
    for k in big.keys():
        table[k] = ctr
        ctr += 1

    compd, diduse = comp(flat, table)
    for k in list(big):
        if k not in diduse:
            del big[k]
    bytz=sum(len(x) for x in big)

    crunched = 0
    smallframes = []
    for p1,p0 in frames:
        cframe1,_ = comp(p1, table)
        cframe0,_ = comp(p0, table)
        crunched += sizeof(cframe1)+sizeof(cframe0)
        smallframes.append((cframe1, cframe0))

    t_inv = {v:k for k,v in table.items()}
    print('SECTION "music__dictionary", ROMX')
    print("music_dictionary::")
    for k in sorted(t_inv.keys()):
        v = t_inv[k]
        print('db', ','.join(str(x) for x in v))

    def divvy(ls):
        out = []
        while ls:
            if isinstance(ls[0],tuple):
                out.append(ls.pop(0))
            else:
                nums = list(takewhile(lambda x:isinstance(x,int), ls))
                ls=ls[len(nums):]
                out.append(nums)
        return out

    emitted = 0
    bankno = 0
    frameno = 0
    while True:
        print(f'SECTION "music__music{bankno}", ROMX')
        literals = []
        while emitted < 0xFFFF:
            frame = smallframes.pop(0)
            p1, p0 = frame

            p1 = divvy(p1)
            p2 = divvy(p2)

            def process(ls):
                for chunk in ls:
                    if instanceof(chunk,tuple):


            print(f"music{idx}::")
            if p1: print("db "+','.join(str(x) for x in p1))
            print("db 0")
            if p0: print("db "+','.join(str(x) for x in p0))
            print("db 0")
            print(f"db LOW(BANK(music{(idx+1) % len(frames)}))")
            print(f"dw music{(idx+1) % len(frames)}")

    # csize = sizeof(compd)
    # print(csize, bytz, len(table))
    # print(csize+bytz, len(flat), ((csize+bytz)/len(flat)) * 100)
    # print(crunched+bytz, len(flat), ((crunched+bytz)/len(flat)) * 100)
    # print(crunched+bytz+len(frames), len(flat), ((crunched+bytz+len(frames))/len(flat)) * 100)

    # pprint(smallframes[100:120])
    # pprint(table)

    # assert decomp(compd, table) == flat

go()
