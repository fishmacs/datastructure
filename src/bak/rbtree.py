class Singleton(type):
    instances = {}

    def __call__(cls, *args, **kwargs):
        if cls not in cls.instances:
            instance = cls.instances[cls] = super.__call__(*args, **kwargs)
            return instance


class MyClass(metaclass=Singleton):
    pass


class Node:
    def __init__(self, parent):
        self.parent = parent


class NilNode:
    nil_node = None

    def __new__(cls, *args, **kwargs):
        if not cls.nil_node:
            cls.nil_node = super().__new__(cls, *args, **kwargs)
            return cls.nil_node

    @property
    def left(self):
        return self.nil_node

    @property
    def right(self):
        return self.nil_node

    @property
    def value(self):
        return None

    @property
    def parent(self):
        return self.nil_node


class LeafNode(Node):
    def __init__(self, parent):
        self.parent = parent

    @property
    def left(self):
        return None

    @property
    def right(self):
        return None

    @property
    def value(self):
        return None

