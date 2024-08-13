import fileinput
import sys

arrs = []
for line in fileinput.input():
    # [PSan : list_length] => [a1, a2, a3, ..., an] format
    line = line.strip()
    if line.startswith("[PSan : list_length] => ["):
        arr = []
        for i in line.split(" => [")[1].split(", "):
            arr.append(int(i.replace("]", "")))
        arrs.append(arr)

for i in range(len(arrs) - 1):
    if arrs[i] == arrs[i + 1]:
        print(f"[PSan]: You should not call list_length against the same list! -> {arrs[i]} / {arrs[i+1]}", file=sys.stderr)
