import json
from collections import Counter, defaultdict, OrderedDict
from pprint import pprint
from copy import deepcopy

def lzw_compress(uncompressed, dictionary=None):
    """Compress a string to a list of output symbols."""

    if not dictionary:
        make_dict = True
        # Build the dictionary.
        dict_size = 256
        dictionary = {(i,): i for i in range(dict_size)}
    else:
        make_dict = False

    w = ()
    result = []
    for c in uncompressed:
        c = (c,)
        wc = w + c
        if wc in dictionary:
            w = wc
        else:
            if w not in dictionary: print(wc)
            result.append(dictionary[w])

            if make_dict:
                # Add wc to the dictionary.
                dictionary[wc] = dict_size
                dict_size += 1

            w = c

    # Output the code for w.
    if w != ():
        result.append(dictionary[w])
    return result, dictionary

def lzw_decompress(compressed, dictionary):
    """Decompress a list of output ks to a string."""

    fc = compressed.pop(0)
    w = dictionary[fc]
    result = w
    for k in compressed:
        if k in dictionary:
            entry = dictionary[k]
        else:
            raise ValueError('Bad compressed k: %s' % k)
        result += entry

        w = entry
    return list(result)

def loaddata(fname):
    with open(fname, 'rb') as f:
        lines = f.readlines()
    loopframe = json.loads(lines[0])
    data = json.loads(lines[1])
    data = [x for x in data]
    data = [item for sublist in data for item in sublist]
    return data

def docomp(fname, d=None):
    data = loaddata(fname)
    return (data, *lzw_compress(data, d))

def count_substrings(s, n):
    count_dict = defaultdict(int)
    for i in range(len(s) - n + 1):
        substr = tuple(s[i:i+n])
        count_dict[substr] += 1
    return sorted(count_dict.items(), key=lambda item: item[1], reverse=True)[:200]

def count_substrings2(s, n):
    count_dict = defaultdict(int)
    for i in range(0, len(s) - n + 1, n):  # Note the third parameter to range
        substr = tuple(s[i:i+n])
        count_dict[substr] += 1
    return sorted(count_dict.items(), key=lambda item: item[1], reverse=True)[:200]

def count_substrings3(s, n):
    count_dict = defaultdict(int)
    i = 0
    while i <= len(s) - n:  # loop while there is space for a substring of length n
        substr = tuple(s[i:i+n])
        count_dict[substr] += 1
        i += n  # move to the end of the current substring
    return dict(sorted(count_dict.items(), key=lambda item: item[1], reverse=True)[:600])

def comp(data, table):
    data=deepcopy(data)
    out = []
    while data:
        for n in [8,6,4]:
            idx = table.get(tuple(data[:n]), None)
            if idx:
                del data[:n]
                out.append((idx,))
                break
        else:
            out.append(data[0])
            del data[0]
    return out

def decomp(data, table):
    table = {v:k for k,v in table.items()}
    out = []
    for el in data:
        if isinstance(el, tuple):
            out += table[el[0]]
        else:
            out.append(el)
    return out

def go():
    # data = loaddata('../res/music/zen.cooked2')
    # compressed, dictionary = lzw_compress(data)
    # sc = set(compressed)
    # pruned = {v:k for k,v in dictionary.items() if v in sc}

    zendata = loaddata('../res/music/sxtnt.cooked2')
    sxdata = loaddata('../res/music/sxtnt.cooked2')
    nrvdata = loaddata('../res/music/nerve.cooked2')

    # n=4
    big = {}
    for n in [4,6,8]:
        big |= count_substrings3(zendata, n)

    for k in list(big):
        if big[k] < 2:
            del big[k]

    bytz=sum(len(x) for x in big)

    table = {}
    ctr = 0
    for k in big.keys():
        table[k] = ctr
        ctr += 1

    compd = comp(zendata, table)
    csize = sum(2 if isinstance(el,tuple) else 1 for el in compd)
    print(csize, bytz)
    print(csize+bytz, len(zendata), ((csize+bytz)/len(zendata)) * 100)

        # zzz=[y for x,y in substrs.items()]
    # print("dictionary:",len(zzz)*n)
    # print(substrs)
    # print("bytes saved:",sum(q*n for q in zzz))

    # bigdata = zendata+sxdata+nrvdata
    # compr,d = lzw_compress(bigdata)

    # print(len(bigdata),'->',len(compr),'=',(len(compr)/len(bigdata))*100)

    # compr2,d2 = lzw_compress(bigdata, d)
    # print(len(bigdata),'->',len(compr2),'=',(len(compr2)/len(bigdata))*100)

    # d_inv = {v:k for k,v in d2.items()}
    # ctr = Counter(compr2)
    # newdict = {}
    # for v in ctr:
    #     newdict[d_inv[v]] = v
    # newdict |= {(i,):i for i in range(256)}

    # compr3,d3 = lzw_compress(bigdata, newdict)
    # print(len(bigdata),'->',len(compr3),'=',(len(compr3)/len(bigdata))*100)
    # print(sum(len(x) for x in newdict.keys()))

    # decompressed = lzw_decompress(compressed2, {v:k for k,v in dictionary.items()})
    # assert decompressed == data
go()
