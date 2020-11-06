
from Node import Node

#NODE has item(item "data") /next(linked field info)
#get_next() :return next item

class SList:
    def __init__(self):
        self.head = None

    def is_empty(self):
        return self.head == None #if the list is empty returns True

    def add(self, item): #add at head
        temp = Node(item) #create new Node
        temp.set_next(self.head) #sets the link field to head
        #if there is another item as head, the other item becomes the next data to the new added item
        self.head = temp #list's head is now temp node

    def size(self):
        current = self.head
        count = 0
        while current != None:
            count = count + 1
            current = current.get_next() #returns the next node (linked field)

            # if current != None:
            #     print('The new current node item is:'+ str(current.get_item()))
            # else:
            #     break
            #      print('Now the current value is None')

        return count

    def search(self,item):
        current =self.head #set the current variable to the first(head) node
        found =False # found is a flag

                #current is None when after last node/only one node
        while current != None and not found: #if not found is False, means that it is found so stop

            if current.get_item() == item: #if the current is same as the item
                found = True #found is true
            else:
                current = current.get_next() #move to the next linked field

        return found #returns bool

    def delete(self, item):#prerequisite: the list has the item inside

        current = self.head
        previous = None
        found = False

        while not found: #while found is false
            if current.get_item() == item:
                found = True #stop loop
            else:
                previous = current
                current = current.get_next() #go to next node

        if previous == None: #the head node is the item to delete
            self.head = current.get_next()
        else:
            #since the current.get_data() is the item, current's item is the target item
            #so set delete by setting the link data of previous node to the current's next node
            previous.set_next(current.get_next())

    def append(self, item): #add item to the end of list

        current = self.head #the start point is always the head since they are linked data
        previous = None

        count = 0

        while current != None :
            #print(previous)
            # print('this is the current:'+str(count) + str(current))
            count += 1
            previous = current #make the previous the current node
            # print('The new previous node item is', str(previous.get_item()))
            current = current.get_next() #gets next link data
            # if current != None:
                # print('The new current node item is', str(current.get_item()))

        #when the current is None, the previous is the last node
        #append the new item to the last node

        # print('end of the while loop')
        newitem = Node(item)

        # print('The final previous node item is', str(previous.get_item()))
        previous.set_next(newitem)#returns None
        #set next the Node itself!!!!!!!!
        newitem.set_next(None) #set the next to None for the new appended item
        # print('The item that is after the last item is now..'+str(previous.get_next()))


    def pop_first(self): #pops the head item
        headitem = self.head
        self.head = headitem.get_next()
        #print('Now the head is :'  + str(self.head.get_item()))

    def pop_last(self): #pops the last item

        current = self.head
        previous = None

        while current != None:
            # print('Now the current item is..:'+ str(current.item))
            previous = current
            # print('Now the previous item is..:' + str(previous.item))
            current = current.get_next()

            #check if the next item of the current is None (meaning the last variable : stop the loop
            #this makes the previous the item that is before the last item
            if current.get_next() == None:
                break

        #since the previous item is the item before the last item, we can set the next information to None to delete
        # print('The previous item is now :'+ str(previous.item))
        previous.set_next(None)


Newlist = SList()
print('New list is empty, there is no nodes inside ')
print(Newlist.is_empty())
print('Newlist has a head item now..')
Newlist.add(11)
print('The head is now..'+ str(Newlist.head.item))
print('Newlist has a new head item now..')
Newlist.add(20)

print('The head is now..'+ str(Newlist.head.item))
print('Newlist has a new head items now..')

Newlist.add(33)

print('The head is now..'+ str(Newlist.head.item))
Newlist.add(30)

print('The head is now..'+ str(Newlist.head.item))
Newlist.add(10)

print('The head is now..'+ str(Newlist.head.item))
Newlist.add(29)

print('The head is now..'+ str(Newlist.head.item))

print("The size of the list is..")
print(Newlist.size())

print('Lets search for the number 20.. Does the list have 20?')
print(Newlist.search(20))


print('Lets search for the number 90.. Does the list have 90?')
print(Newlist.search(90))

print('lets delete the number 20..')
Newlist.delete(20) #doesn't have return value

print('The size is now...')
print(Newlist.size())

print('Lets search again for the number 20.. Does the list have 20?')
print(Newlist.search(20))

print('try append item: 44 to the end point:')
#the singly linked list right now is [29,10,30,33,11] since 20 was deleted
Newlist.append(44)
print('Lets search for the number 44')
print(Newlist.search(44))
print('Lets see the size for the singly linked list:')
print(Newlist.size())

#Using pop_head()
print('Lets see the head :')
print(Newlist.head.item)
print('Lets pop the first item of the linked list')
Newlist.pop_first()
#now list is [10,30,33,11,44]
print('Lets see the head :')
print(Newlist.head.item)


#using pop_last()
print('lets pop the last item out!')
Newlist.pop_last()
print('Lets see the size for the singly linked list:')
print(Newlist.size()) #the list is now [10,30,33,11]


