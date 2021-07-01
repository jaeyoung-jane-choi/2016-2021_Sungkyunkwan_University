#DAC_sort_algorithm 2016312411 jaeyoung choi

#합병 정렬

def merge(items, temp, low, mid, high):
    i = low
    j = mid+ 1
    for k in range(low, high+1):
        if i > mid:
            temp[k] = items[j]
            j += 1
        elif j > high:
            temp[k] = items[i]
            i += 1

        elif items[j] <items[i]:
            temp[k] = items[j]
            j += 1
        else:
            temp[k] = items[i]
            i += 1

    for k in range(low, high+1):
        items[k] = temp[k]



def merge_sort(items, temp, low, high):
    if high <= low:
        return None
    mid = low + (high - low )//2
    merge_sort(items, temp, low, mid)
    merge_sort(items, temp, mid+1, high)
    merge(items, temp, low,mid,high)



items = [54,88,77,26,93,17,49,10,17,77,11,31,22,44,17,20]

temp = [None] * len(items)
print('before sort:\t', end='')
print(items)

print('after sort:\t', end ='')
merge_sort(items, temp, 0, len(items)-1)
print(items)


#퀵 정렬


def partition(items,pivot, high):
    i = pivot +1
    j = high
    while True:
        while i < high and items[i] < items[pivot]:
            i += 1
        while j > pivot and items[j] > items[pivot]:
            j -=1
        if j <= i:
            break
        items[i], items[j] = items[j] , items[i]
        i += 1
        j -= 1
    items[pivot], items[j] = items[j], items[pivot]
    return j




def quick_sort(items,low,high):
    if low <high:
        pivot = partition(items,low,high)
        quick_sort(items,low,pivot-1)
        quick_sort(items,pivot+1, high)




items = [54,88,77,26,93,17,49,10,17,77,11,31,22,44,17,20]

print('before sort:\t', end='')
print(items)


print('after sort:\t', end ='')
quick_sort(items, 0, len(items)-1)
print(items)
