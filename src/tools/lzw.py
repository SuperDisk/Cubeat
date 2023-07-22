import json
from collections import Counter, defaultdict, OrderedDict
from pprint import pprint
from copy import deepcopy

def flatten(lst):
    if not isinstance(lst, list):
        return [lst]
    if lst == []:
        return lst
    return flatten(lst[0]) + flatten(lst[1:])

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
    return dict(sorted(count_dict.items(), key=lambda item: item[1], reverse=True)[:1350])

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

def go():
    frames, flattened = loaddata('../res/music/sxtnt.cooked2')

    big = {}
    for n in [4,6,8]:
        big |= count_substrings3(zendata, n)

    for k in list(big):
        if big[k] < 2:
            del big[k]

    table = {}
    ctr = 0
    for k in big.keys():
        table[k] = ctr
        ctr += 1

    compd, diduse = comp(zendata, table)
    for k in list(big):
        if k not in diduse:
            del big[k]
    bytz=sum(len(x) for x in big)

    csize = sum(2 if isinstance(el,tuple) else 1 for el in compd)
    print(csize, bytz)
    print(csize+bytz, len(zendata), ((csize+bytz)/len(zendata)) * 100)
    print(compd[:1000])

    assert decomp(compd, table) == zendata

go()
