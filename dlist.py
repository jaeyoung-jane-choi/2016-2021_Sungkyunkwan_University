
from dNode import Node

class Dlist:
    def __init__(self):
        self.head= None

    def is_empty(self):
        return self.head ==None #if none, empty is true

    def add(self,item): #add at the start of list
        if self.head == None: #if the list is empty
            self.head = Node(item) #set the head's inference to the new item
        else:
            temp = Node(item)
            temp.set_next(self.head) #set the next of the item to the head
            self.head.set_previous(temp) #set the head's inference previous to temp
            self.head = temp #make the temp head

    def append(self,item): #add item at last
        if self.head == None:  #when the list is empty, append at head
            self.head = Node(item)
        else: #when not empty list
            current = self.head
            while current.get_next() != None: #while loop until current's next is none: when node is the last node
                current = current.get_next()
            #outside while loop; current's next item is none
            temp = Node(item)
            current.set_next(temp)
            temp.set_previous(current)

    def insert_after(self, item, x): #insert new item after x
        if self.head == None: #when empty list there is no x
            print('List is empty!')

        else:
            current = self.head
            found = False
            while current != None and not found:
                if current.get_item() == x: #when we found the item we want to insert after
                    found = True
                else:
                    current = current.get_next()

            if found == False: #when x is not found : not in the list
                print('Item is not in the list')
            else:
                #change the inference of temp
                temp = Node(item)
                temp.set_next(current.get_next())
                temp.set_previous(current)

                if current.get_next() != None: #current is not the last item of the list
                    current.get_next().set_previous(temp)
                    #when current is the last item, no need to set the previous to temp since it is none

                current.set_next(temp) #set the current's next to temp


    def insert_before(self,item,x):
        if self.head == None:
            print('List is empty')
        else:
            current= self.head
            found = False
            while current != None and not found : #while until the current object is none(last item) or found
                if current.get_item() == x:
                    found = True
                else:
                    current = current.get_next()

            if found == False:
                print('Item is not in the list')

            else: #when the item is in the list
                temp = Node(item)
                temp.set_next(current)
                temp.set_previous(current.get_previous())
                #set the temp's previous and next according to the current item

                if current.get_previous() != None: #when the current item is not the first item in list
                    current.get_previous().set_next(temp)

                else: #when the previous item is None (current is the first item of the list)
                    self.head = temp
                current.set_previous(temp)
    def pop_first(self):
        if self.head == None:
            print('List is empty.')
        else:
            if self.head.get_next() ==None: #when there is only one item
                self.head =None  #set head to None
            else:
                self.head= self.head.get_next() #make the next item of the head to the head
                self.head.set_previous(None) #make the new head's previous to None ; making it the head


    def pop_last(self):
        if self.head == None:
            print('List is empty.')
        else:
            if self.head.get_next() == None: #when the list has only one item
                self.head = None #delete that only one item
            else:
                current = self.head
                while current.get_next() != None: #until current's next item is none(last item's get next is none)
                    current = current.get_next()

                current.get_previous().set_next(None) #set the current's previous to None

    def delete(self, item):
        if self.head.get_item() == item: #when the first item is the item we are looking for
            self.head = self.head.get_next()
            self.head.set_previous(None) #make the next item of head to the head and delete the previous inference
        else:
            current =self.head
            found = False
            while not found: #always found is the prerequisite
                if current.get_item() == item:
                    found = True
                else:
                    current = current.get_next()
            if current.get_next() != None: #when not last item
                current.get_previous().set_next(current.get_next())
                current.get_next().set_previous(current.get_previous())
            else: #when last item
                current.get_previous().set_next(None)

    def search(self,item):
        current = self.head
        found = False
        while current != None and not found:
            if current.get_item() == item:
                found = True
            else:
                current = current.get_next()
        return found

    def size(self):
        current = self.head
        count = 0
        while current != None: #while not last item
            count += 1
            current = current.get_next()
        return count




d = Dlist()
print('is the list empty..?  '+str(d.is_empty()))
d.add(30)
d.add(20)
d.add(50)
d.add(12)
d.append(22)
d.append(90)
print('the size of the list d is..'+ str(d.size()))
d.pop_last()
d.pop_first()
d.delete(20)
print('the size of the list d is..'+ str(d.size()))
print('Is the value 22 in the list..?  ' +str(d.search(22)))
d.insert_after(99,30)
print('the size of the list d is..'+ str(d.size()))
d.insert_before(100,30)
print('the size of the list d is..'+ str(d.size()))
















