class BinaryHeap:

    def __init__(self,array = []):
        self.items = array

    def size(self):
        return len(self.items)

    def swap(self,i,j):
        self.items[i], self.items[j]= self.items[j], self.items[i]

    def insert(self,key):
        self.items.append(key) #append to last
        self.upheap(self.size()-1) #use upheap to change the priorities starting at last element

    def upheap(self,i):
        while i>0 and self.items[(i-1)//2] > self.items[i]: #compare with parent node
            self.swap(i,(i-1)//2) #if parent is bigger than comparing child node swap
            i=(i-1)//2  #update the index too since swapped

                #deleting smallest num(priority 1)
    def extract_min(self):
        if self.size() == 0: #empty heap
            print('Heap is empty. ')
            return None
        #select minimum = priority num 1
        minimum = self.items[0]
        self.swap(0,-1) #swap last and first
        del self.items[-1] #delete last (priority num 1 )
        self.downheap(0) #do downheap to the swapped element
        return minimum

    def downheap(self,i):#i is start point
        # 2i+1 is the left child of i : compare with the last left-node of heap
        while 2*i +1 <=self.size()-1:
            #if left child of i is smaller/same with last left node
            k = 2*i + 1 #k is the left child

            #when k is not the last left child
            if k < self.size()-1 and self.items[k] > self.items[k+1]:
                #if left child of i is not last node compare left, right

                k += 1 #if right is smaller : winner is right ; update to right

            if self.items[i] < self.items[k]: #if the item of i is smaller than k's item then can stop
                break
            #if not swap i and k
            self.swap(i,k)
            i=k #and do index update

    def build_heap(self,array):
        for i in range(len(array)//2 -1,-1,-1): #start at last node without child's index until before -1
            self.downheap(i)

    def print_heap(self):
        for i in range(0,self.size()):
            print(self.items[i], end = ' ') #start from root
        print('\nSize of Heap = ' , self.size())


if __name__ == '__main__':
    array = [3,2,4,5,6,7]
    bheap = BinaryHeap(array)
    bheap.build_heap(array)
    bheap.print_heap()
    bheap.insert(1)
    bheap.insert(9)
    bheap.insert(11)
    bheap.insert(19)
    bheap.print_heap()
    print(bheap.extract_min())
    bheap.print_heap()
    print(bheap.extract_min())
    bheap.print_heap()
