import json
from collections import Counter, defaultdict, OrderedDict
from pprint import pprint
from copy import deepcopy

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
    while data:
        for n in [8,6,4]:
            idx = table.get(tuple(data[:n]), None)
            if idx:
                diduse.add(tuple(data[:n]))
                del data[:n]
                out.append((idx,))
                break
        else:
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
    frames, flat = loaddata('../res/music/zen.cooked2')

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

    csize = sizeof(compd)
    print(csize, bytz, len(table))
    print(csize+bytz, len(flat), ((csize+bytz)/len(flat)) * 100)
    print(crunched+bytz, len(flat), ((crunched+bytz)/len(flat)) * 100)
    print(crunched+bytz+len(frames), len(flat), ((crunched+bytz+len(frames))/len(flat)) * 100)

    # assert decomp(compd, table) == flat

go()
