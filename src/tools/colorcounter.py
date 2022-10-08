from PIL import Image
im = Image.open('SuperGameBoyFrame.png')
from collections import defaultdict
by_color = defaultdict(int)
for pixel in im.getdata():
    by_color[pixel] += 1
print(by_color)
print(len(by_color))
print(list(by_color))

from itertools import product

def tile(img):
    w, h = img.size
    d = 8
    grid = product(range(0, h-h%d, d), range(0, w-w%d, d))
    for i, j in grid:
        box = (j, i, j+d, i+d)
        yield img.crop(box)

x = []
for t in tile(im):
    x.append(sorted(list(set(t.getdata()))))
print(x)
print('yeah')

