import fileinput
import sys

arr_addr = []
list_hd = 0
list_next = 0
right_arr_counts = []
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
    # [PSan : append] => [a1, a2, a3, ..., an] @ [b1, b2, b3, ..., bn] format
    elif line.startswith("[PSan : append] => "):
        left = line[len("[PSan : append] => "):].split(" @ ")[0]
        right = line[len("[PSan : append] => "):].split(" @ ")[1]
        left_arr = []
        right_arr = []
        for a in left.split("[")[1].split("]")[0].split(","):
            left_arr.append(int(a))
        for b in right.split("[")[1].split("]")[0].split(","):
            right_arr.append(int(b))
        right_arr_counts.append(len(right_arr))

    if list_next > (list_hd + 10):
        score = list_next / (list_hd + 1) / 20
        print(f"[PSan]: You seem to be doing random access to a list, you should be using an array instead of a list. Confidence: {score}", file=sys.stderr)

for i in range(len(arr_addr) - 1):
    if arr_addr[i][0] == arr_addr[i + 1][0]:
        print(f"[PSan]: You should not call list_length against the same list! -> {arr_addr[i][0]} / {arr_addr[i+1][0]}", file=sys.stderr)

right_arr_only_one_item = True
for i in range(len(right_arr_counts)):
    if right_arr_counts[i] > 1:
        right_arr_only_one_item = False
        break
if right_arr_only_one_item:
    print("[PSan]: You seem to be appending only one item to the list, you should be using a doubly linked list or memorize the pointer to the last element of the list.", file=sys.stderr)
