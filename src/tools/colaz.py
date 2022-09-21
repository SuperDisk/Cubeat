#!/usr/bin/env python3
from PIL import Image
import sys

x = Image.open(sys.argv[1])
y = x.copy()

d = x.getdata()
new = [(3 if pix == 3 else 0) for pix in d]
new2 = [(pix if pix != 3 else 0) for pix in d]
x.putdata(new2)
y.putdata(new)

n = sys.argv[2].replace('.sep2.png','').replace('.sep1.png', '')
print('saving', n+'.sep2.png', n+'.sep1.png')
y.save(n+'.sep2.png')
x.save(n+'.sep1.png')
