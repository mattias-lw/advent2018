import re

def do_char(stack,c):
    if len(stack) == 0:
        stack.append(c)
        return
    last = stack[-1]
    if last.lower() == c.lower() and ((last.isupper() and c.islower()) or (last.islower() and c.isupper())):
        stack.pop()
    else:
        stack.append(c)

def do(d,filt=r'@'):
    stack = []
    for c in d:
        if re.match(r'[a-zA-Z]',c) and not re.match(filt,c,re.IGNORECASE): do_char(stack,c)
    return "".join(stack)

with open('data.txt') as inf:
    data =  inf.read()
    print(len(do(data)))
    print(sorted(map(lambda c: (c,len(do(data,c))), 'abcdefghijklmnopqrstuvwxyz'),key=lambda i:i[1])[0])
