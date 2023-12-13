import numpy as np
def part_2(content):
    times = content[0][content[0].index(':')+1:].split()
    distances = content[1][content[1].index(':')+1:].split()
    times = int("".join(times))
    distances = int("".join(distances))
    
    t = times
    temp = np.arange(t+1)
    f = lambda x: (t-x)*x
    temp = np.array(f(temp))
    return np.count_nonzero(np.where(temp>distances))

n = part_2(["Time:        38     94     79     70", "Distance:   241   1549   1074   1091"])
print(n)
