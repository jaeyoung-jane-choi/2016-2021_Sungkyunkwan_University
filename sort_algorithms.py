

#선택 정렬

def selection_sort(items):
    for i in range(0, len(items)-1): #don't need to sort last item since organize from front
        minimum= i
        for j in range(i, len(items)):
            if items[minimum] > items[j]:
                minimum = j #save the index which has minimum value

        items[i], items[minimum] = items[minimum], items[i]
        # change location by index after all checking items in the list

items = [40,70,10,30,60,50]
print('정렬 전', end='')
print(items)
selection_sort(items) #returns None
print('정렬 후',end='')
print(items)

print('------------------------------')


#삽입 정렬

def insertion_sort(items):
    for i in range(1, len(items)): #thinks that index 0 is already sorted, so start at the index 1 element

        for j in range(i,0,-1): #compare with left hand side (starts with index , minus 1 .. until before 0 (until 1)

            if items[j-1]> items[j]: #if left item is larger than item, change index location
                items[j], items[j-1] = items[j-1], items[j]
            #if not break and get out of loop since more left is smaller than the just left element
            else:
                break


items = [40, 70, 10, 30, 60, 50]
print('정렬 전', end='')
print(items)
insertion_sort(items)
print('정렬 후',end='')

print(items)

print('------------------------------')

#쉘 정렬 : similar to insertion sort

def shell_sort(items):
    h = len(items)//2 #h is N//2

    while h>= 1 : #loop until h is under 1

        for i in range(h, len(items)): #starts as h until last item in list
            j=i
            #while j is big,same as h  and second item in h-list is smaller than first item...
            while j >= h and items[j] < items[j-h]:
                items[j] , items[j-h] = items[j-h], items[j] #change the items' index
                # compare right->left so j becomes a smaller value
                j-=h

        print('{}-정렬 결과: '.format(h), items)
        h//= 2


items = [39,23,15,47,11,56,61,16,12,19,21,41]
print('정렬 전', end='')
print(items)
shell_sort(items)
print('정렬 후',end='')
print(items)


print('------------------------------')

#힙 정렬

def downheap(i,size):
    while 2*i +1 <= size:
        k = 2*i +1
        if k < size-1 and items[k] <items[k+1]:
            k +=1
        if items[i] >= items[k]:
            break
        items[i], items[k] = items[k], items[i]
        i=k

def heapify(items):
    hsize= len(items)
    for i in range(hsize//2-1, -1,-1):
        downheap(i,hsize)


def heap_sort(items):
    N = len(items) 
    for i in range(N):
        items[0], items[N-1] = items[N-1], items[0]
        downheap(0,N-2)
        N-=1

items = [39,23,15,47,11,56,61,16,12,19,21,41]
print('정렬 전', end='')
print(items)
heapify(items)
print('최대 힙', end='')
print(items)
heap_sort(items)
print('정렬 후',end='')
print(items)

