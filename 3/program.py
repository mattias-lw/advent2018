import re

def items(xstart, ystart, length, height):
    for x in range(xstart,xstart+length):
        for y in range(ystart,ystart+height): yield (x, y)

def check(seen, items):
    for item in items:
        if item in seen and seen[item] > 1: return False
    return True

with open("data.txt","r") as inf: 
    seen = dict()
    data = [list(map(int, re.findall(r'\d+', line))) for line in inf.read().splitlines()]
    for d in data: 
        for item in items(*d[1:]):
            seen[item] = seen.get(item,0) + 1
    print(len([i for i in seen if seen[i] > 1]))
    for nr in [d[0] for d in data if check(seen,items(*d[1:]))]: 
        print(nr)
    




