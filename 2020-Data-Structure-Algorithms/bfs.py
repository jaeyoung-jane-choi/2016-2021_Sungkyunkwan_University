

# from clqueue import Queue #큐를 한 과제가 없어서,  큐를 새로 정의했습니다.
#first in first out
class Queue:
    def __init__(self):

        self.item = [] #empty list

    def is_empty(self):
        return self.items == []

    def enqueue(self,item):
        self.item.append(item) #append at first
    def dequeue(self):
        if self.is_empty():
            return None
        else:
            self.item.pop(0)


adj_list =[[2,1],[3,0],[3,0],[9,8,2,1],[5],[7,6,4],[7,5],[6,5],[3],[3]]

N=len(adj_list)
visited = [False]*N #make false *n list



def bfs(v):
    queue = Queue()
    visited[v] =True
    queue.enqueue(v)
    while not queue.is_empty():
        v= queue.dequeue()
        print(v,' ', end='')
        for i in adj_list[v]:
            if not visited[i]:
                visited[i] = True
                queue.enqueue(i)



print('BFT 방문 순서: ')
for i in range(N):
    if not visited[i]:
        bfs(i)
