import fileinput
import sys

arr_addr = []
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

for i in range(len(arr_addr) - 1):
    if arr_addr[i][0] == arr_addr[i + 1][0]:
        print(f"[PSan]: You should not call list_length against the same list! -> {arr_addr[i][0]} / {arr_addr[i+1][0]}", file=sys.stderr)
