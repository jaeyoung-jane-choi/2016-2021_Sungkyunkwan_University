class Node:
    def __init__(self, item):
        self.item = item #data stored
        self.next = None #link data
    def get_item(self):
        return self.item
    def get_next(self):
        return self.next
    def set_item(self, new_item):
        self.item = new_item
    def set_next(self, new_next):
        self.next = new_next

if __name__ == '__main__':
    a = Node(10)
    print("Item for a is...")
    print(a.get_item())
    print(a.item)
    print("A's linked field is...")
    print(a.get_next())
    a.set_next(0)
    print("now a's link data is set to 0") #meaning that after 10 there is 0
    print(a.get_next())