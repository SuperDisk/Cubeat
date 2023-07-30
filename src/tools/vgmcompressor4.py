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
    return dict(sorted(count_dict.items(), key=lambda item: item[1], reverse=True)[:2000])

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

    smallframes = []
    for p1,p0 in frames:
        cf1,_ = comp(p1, table)
        cf2,_ = comp(p0, table)
        smallframes.append((cf1,cf2))

    def divvy(ls):
        out = []
        while ls:
            if isinstance(ls[0],tuple):
                out.append(ls.pop(0))
            else:
                nums = list(takewhile(lambda x:isinstance(x,int), ls))
                if not nums:print(ls)
                ls=ls[len(nums):]
                out.append(nums)
        return out

    total = 0
    usedshit = set()
    for el in divvy(compd):
        if isinstance(el,list):
            total += len(el)+1
        else:
            usedshit.add(el)
            # total += 2
        total+=2

    t_inv = {v:k for k,v in table.items()}
    justdict=0
    for (key,) in usedshit:
        v = t_inv[key]
        # total += (len(v)/2) * 6
        total += len(v)+6
        justdict += len(v)+6


    emitted = 0
    this_bank = 0
    frameno = 0

    strbuf = []
    usedper = []
    used = set()
    bankno = 0

    print('include "music_macros.inc"')
    while smallframes:
        print(f'SECTION "music__music{bankno}", ROMX[$4000]')

        while smallframes and (this_bank+2000 < 0x3FFF):
            f1,f2 = smallframes.pop(0)
            p1 = divvy(f1)
            p0 = divvy(f2)

            strbuf.append("; BEGIN FRAME")

            def writeframe(frame, only_part):
                nonlocal this_bank, emitted
                for idx, el in enumerate(frame):
                    if isinstance(el, tuple):
                        (phraseno,) = el
                        if idx==len(frame)-1:
                            strbuf.append(f" dwbe phrase_{bankno}_{phraseno} | (1<<15)")
                        else:
                            strbuf.append(f" dwbe phrase_{bankno}_{phraseno}")
                        this_bank += 2

                        if el not in used:
                            used.add(el)
                            this_bank += len(t_inv[el[0]])+1

                    else:
                        if idx==len(frame)-1:
                            strbuf.append(f"db f_vgm_literals_end, {len(el)//2}")
                        else:
                            strbuf.append(f"db f_vgm_literals, {len(el)//2}")
                        for a,b in zip(el[::2], el[1::2]):
                            strbuf.append(f"db ${format(a,'x')},${format(b,'x')}")
                        this_bank += 1+len(el)

            if (not p1) and (not p0):
                strbuf.append("db f_empty_frame")
                this_bank += 1
            else:
                if not p1:
                    strbuf.append("db f_switch_port")
                    this_bank += 1
                else:
                    writeframe(p1, not p0)

                if not p0:
                    strbuf.append("db f_switch_port")
                    this_bank += 1
                else:
                    writeframe(p0, not p1)

        # print("Bankswitch")
        print(f"music{bankno}::")
        for s in (strbuf):
            print(s)

        for phrase in used:
            (phraseno,) = phrase
            print(f"phrase_{bankno}_{phraseno}: db ", ','.join(str(x) for x in t_inv[phraseno]), ",0", sep='')

        usedper.append(used)
        used = set()

        strbuf = []
        emitted += this_bank
        this_bank = 0
        bankno += 1

    # print("total:",total)
    print("emitted:",emitted,file=sys.stderr)
    # print("dict:",justdict,"bytes",len(usedshit),"entries")
    # print([len(x) for x in usedper])
    # for used in usedper:
        # print(sum(len(t_inv[key[0]]) for key in used))

go()
