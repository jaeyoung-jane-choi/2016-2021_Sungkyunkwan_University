
from Node import Node
class CList:
    def __init__(self):
        self.head = None

    def is_empty(self):
        return self.head == None

    def add(self, item): #add at first position
        temp = Node(item)
        if self.is_empty():
            temp.set_next(temp)
            self.head= temp
        else:
            temp.set_next(self.head.get_next())
            self.head.set_next(temp)

    def append(self,item): #add at last position
        temp = Node(item)
        if self.is_empty():
            temp.set_next(temp)
            self.head = temp
        else:
            temp.set_next(self.head.get_next()) #link with first node
            self.head.set_next(temp) #set last node's next to temp
            self.head = temp

    def pop_first(self): #pop first item out
        if self.head == None: #has no items
            print('The List is empty.')
        else:
            temp = self.head.get_next() #first node

            if temp == temp.get_next(): #if the list has only one node
                self.head = None #set the head to None
            else:
                self.head.set_next(temp.get_next()) #if not link the next node with head

    def search(self,item):

        if self.head == None:
            print('The List is empty.')
            found = None #since we return found , we need to set the varaible to something to not get an error

        else: #not empty list
            temp = self.head.get_next() #temp is the first node

            #1 when list has only one node
            if self.head == temp:
                if self.head.get_item() == item:
                    return True
                else: #when node's item is not item we are searching for
                    return False

            #2 when list has more than one node
            found = False
            current = temp

            while True:
                if current.get_item() == item:
                    found = True
                else:
                    current = current.get_next()
                #current == temp or found == True breaks while loop
                if current != temp and not found:
                    continue
                else:
                    break

        return found



clist = CList()
print('New Circular Linked list is empty, there is no nodes inside ')
print('Is the list empty..?  ' + str(clist.is_empty()))


print('Lets add new items to the list')
clist.add('hi') #last item
clist.add('hello')
clist.add('bonjour')
clist.add('good morning') #first item


print('Is the list empty..? '+ str(clist.is_empty()))
print('The first item of the list is.. ' +str(clist.head.get_next().get_item()))
print('The last item of the list is.. ' +str(clist.head.get_item()))


print('Lets append an item to the list')
clist.append('Good night')
print('Now.. The first item of the list is.. ' +str(clist.head.get_next().get_item()))
print('Now..The last item of the list is.. ' +str(clist.head.get_item()))

print('Lets pop the first item of the list')
clist.pop_first()
print('Now.. The first item of the list is.. ' +str(clist.head.get_next().get_item()))

print('Lets search for the item.. chao')
print('Is the word chao in the list..?  '+ str(clist.search('chao')))

print('Lets search for the item.. hello')
print('Is the word hello in the list..?  '+ str(clist.search('hello')))

print('Lets try a new list')
nlist =CList()
print('Is the list empty..?  ' + str(nlist.is_empty()))

print('Try popping out first item.. ' + str(nlist.pop_first()))
print('Try searching item 10' )
nlist.search(10)

print('Lets add an item to the list')
nlist.add(100)
print('The first item of the list is.. ' +str(nlist.head.get_next().get_item()))


print('Lets pop the first item out')
nlist.pop_first()
print('Is the list empty..?  ' + str(nlist.is_empty()))

print('Lets append an item to the list')
nlist.append(50)
print('Is the list empty..?  ' + str(nlist.is_empty()))

print('Lets search for item 100 in the list')
print('Is 100 in the list..?  '+str(nlist.search(100)))
