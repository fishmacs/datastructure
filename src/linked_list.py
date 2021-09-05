from dataclasses import dataclass
from copy import copy


@dataclass
class Node:
    val: any
    next: any

    def __init__(self, val, next=None):
        self.val = val
        self.next = next


def linked_list(elems):
    head = old_node = None
    for elem in elems:
        node = Node(elem)
        if not head:
            head = old_node = node
        else:
            old_node.next = node
            old_node = node
    return head


def reverse(l):
    node = l
    reversed = None
    while node:
        next1 = node.next
        node.next = reversed
        if next1:
            next2 = next1.next
            next1.next = node
            reversed = next1
            node = next2
        else:
            node = next1
    return reversed
