#lang dssl2
 
struct node:
    let key
    let left
    let right
 
def insert(t, k):
    if t is None:
        t = node(k, None, None)
    elif k < t.key:
        t.left = insert(t.left, k)
    elif k > t.key:
        t.right = insert(t.right, k)
    return t
    
insert(insert(None,5),6)
