import fileinput
import sys

arr_addr = []
list_hd = 0
list_next = 0
for line in fileinput.input():
    # [PSan : list_length] => addr [a1, a2, a3, ..., an] format
    # i.g. [PSan : list_length] => 1234 [1, 2, 3]
    line = line.strip()
    if line.startswith("[PSan : list_length] => "):
        data = line[len("[PSan : list_length] => "):]
        addr = data.split(" ")[0]
        arr = []
        for a in data.split("[")[1].split("]")[0].split(","):
            arr.append(int(a))
        arr_addr.append((arr, addr))
    elif line.startswith("[PSan : list_hd] called"):
        list_hd += 1
    elif line.startswith("[PSan : list_next] called"):
        list_next += 1

    if list_next > (list_hd + 10):
        score = list_next / (list_hd + 1) / 20
        print(f"[PSan]: You seem to be doing random access to a list, you should be using an array instead of a list. Confidence: {score}", file=sys.stderr)

for i in range(len(arr_addr) - 1):
    if arr_addr[i][0] == arr_addr[i + 1][0]:
        print(f"[PSan]: You should not call list_length against the same list! -> {arr_addr[i][0]} / {arr_addr[i+1][0]}", file=sys.stderr)
