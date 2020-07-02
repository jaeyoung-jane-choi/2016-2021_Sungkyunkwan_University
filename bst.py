from btnode import Node
class BST:
    def __init__(self):
        self.root = None

    def search(self,k): #look for k element
        return self._search(self.root, k) #starts at root element

    def _search(self,n,k): #protected method

        if n == None or n.get_key() == k :
            #when n is None , returns False
            #when n's key is k, returns True
            return n!= None

        elif n.get_key()> k: #when node's key is bigger than key need to move to left
            return self._search(n.get_left(), k) #recursive

        else: #when key is bigger than node's key
            return self._search(n.get_right(), k ) #recursive


    def insert(self,key):
        self.root = self._insert(self.root,key) #starts at root

    def _insert(self,n,key):

        if n== None:  #when n is none make a node
            return Node(key)  #n is now a node with element of key

        if n.get_key() > key: #if node has larger element than key look at left side
            n.set_left(self._insert(n.get_left(), key))

        elif n.get_key() <key: #when larger, look at right side
            n.set_right(self._insert(n.get_right(), key)) #n is right node of original n

        return n

    #finding the minimum value in the whole tree
    def find_min(self):
        if self.root == None:
            return None #emptry tree

        return self._find_min(self.root) #recursive

    def _find_min(self,n):
        if n.get_left() == None: #if the node's left node (which is a smaller value) none : means that node is smallest
            return n
        return self._find_min(n.get_left()) #if has left recursivly do finding min

    #delete the min value
    def delete_min(self):
        if self.root == None: #emptry tree
            print('Tree is empty')

        self.root = self._delete_min(self.root) #recursivly done

    def _delete_min(self,n):

        if n.get_left() == None: #if the left node is none, get the right node as n
            return n.get_right()
        n.set_left(self._delete_min(n.get_left())) #find minimum value until none

        return n

    def delete(self,key):
        self.root = self._delete(self.root,key)

    def _delete(self, n,key):
        if n == None:
            return None

        if n.get_key()> key:
            n.set_left(self._delete(n.get_left(), key))
        elif n.get_key() < key:
            n.set_right(self._delete(n.get_right(), key ))

        else:
            #case0 : no childs
            if n.get_left() == None and n.get_right() == None:
                return None

            #case1 : one child
            if n.get_left() == None or n.get_right() == None :
                if n.get_left() == None:
                    return n.get_right()
                else:
                    return n.get_left()
            #case2: two childs
            target = n #set n to new target value
            n= self._find_min(target.get_right()) #set n as the minimum of right child tree
            n.set_right(self._delete_min(target.get_right())) #set the right of new n to the min deleted  right tree
            n.set_left(target.get_left()) #add the left tree to the new n

        return n


    def inorder(self,n):
        if n!= None: #if the value is not none
            if n.left : #if there is a left
                self.inorder(n.left) #start recursive action on left side
            print(str(n.key),' ', end='') #print out the keys after finding last left...right
            if n.right : #if there is  a right
                self.inorder(n.right)



a = BST()
a.insert(7)
a.insert(2)
a.insert(5)
a.insert(9)
print(a.inorder(a.root))
a.delete(2)
print(a.inorder(a.root))

print('search for 2: ',a.search(2))
