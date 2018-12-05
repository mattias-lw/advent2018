import re

with open("data.txt","r") as inf: 
    data = [list(map(int, re.findall(r'\d+', line))) for line in sorted(inf.read().splitlines())
]
    current_guard=None
    asleep_time=None
    guards_time=dict()
    guards_minutes=dict()
    for d in data:
        if len(d) == 6: 
            current_guard=d[5]
        elif asleep_time == None:
            asleep_time=d[4]
        else:
            guards_time[current_guard] = guards_time.get(current_guard,0) + (d[4] - asleep_time)
            for m in range(asleep_time,d[4]):
                guards_minutes[current_guard] = guards_minutes.get(current_guard,dict())
                guards_minutes[current_guard][m] = guards_minutes[current_guard].get(m,0) + 1 
            asleep_time=None
    sorted_guards = sorted(list(guards_time.items()),key=lambda x: x[1], reverse=True)
    minutes = sorted(list(guards_minutes[sorted_guards[0][0]].items()),key=lambda x: x[1],reverse=True)
    print(sorted_guards[0][0]*minutes[0][0])
    # Part 2
    minutes2 = list(map(lambda g: list(map(lambda h: (g[0],)+h,list(g[1].items()))), list(guards_minutes.items())))
    flat_minutes2 = [item for sublist in minutes2 for item in sublist]
    sorted_minutes2 = sorted(flat_minutes2,key=lambda x:x[2],reverse=True)
    print(sorted_minutes2[0][0] * sorted_minutes2[0][1])
