--Evan Arends
--Cpcsc 326-01
--heap.hs
--Heap implementation

import Data.List(maximum)
import Data.List(minimum)


data Heap a = Node a (Heap a) (Heap a)
			| Nil
			deriving (Show,Eq)

--Function Insert
--takes a value and a heap and returns
-- a new heap containg the value
insert :: Ord a => a -> Heap a -> Heap a
insert v Nil  = Node v Nil Nil
insert v (Node x left right)
	| left == Nil && right == Nil = trickleUp (Node x (Node v Nil Nil) Nil) (3)
	| right == Nil = trickleUp (Node x (left) (Node v Nil Nil)) (1)
	| has_kids left && has_kids right == False = trickleUp (Node x (left) ((insert v right))) (1)
	| otherwise = trickleUp ((Node x (insert v left) (right))) (0)


--Function height
--returns the height of tree
height :: (Num a, Ord a) => Heap t -> a
height (Node x Nil Nil) = 0
height (Node x left Nil) = 1 + height left
height (Node x left right) = 1 + maxx (height left, height right)

--Function maxx
--grabs the max value between the two 
-- values in a tuple
maxx :: Ord t => (t, t) -> t
maxx (x,y)
	| x > y = x
	| otherwise = y 


--Function isFull 
-- checks to see if the tree is full 
isFull :: Heap t -> Bool
isFull (Node x Nil Nil) = True
isFull (Node x left Nil) = False
isFull (Node x Nil right) = False
isFull (Node x left right)
	| height left == height right && isFull left && isFull right = True
	| otherwise = False

--Function trickleUp
--heapifys the heap using trickleUp Method
trickleUp :: (Eq a, Ord a1, Num a) => Heap a1 -> a -> Heap a1
trickleUp (Node x (Node v left right) Nil ) _
	| x > v = Node v (Node x left right) Nil 
	| otherwise = Node x (Node v left right) Nil 
trickleUp (Node x (Node y left1 right1) (Node z left2 right2)) op
	| op == 0 && x > y  =  Node y (Node x left1 right1) (Node z left2 right2)
	| op == 0 && y >= x = Node x (Node y left1 right1) (Node z left2 right2)
	| op == 1 && x > z = Node z (Node y left1 right1) (Node x left2 right2)
	| otherwise = Node x (Node y left1 right1) (Node z left2 right2)

--Function deleteMin 
--deletes min element from list
-- then reheapifys heap
deleteMin :: Ord a => Heap a -> Heap a
deleteMin (Node x Nil Nil) = Nil
deleteMin (Node x left right) = trickleDown(deleteLast(Node (lastChild(Node x left right)) left right))


--Function lastChild
--returns value of last child in heap
lastChild :: Eq t => Heap t -> t
lastChild (Node x Nil Nil) = x
lastChild (Node x left Nil) = lastChild left
lastChild (Node x left right)
	| isFull left == False && isFull right && (height left) /= (height right) = lastChild left
	| isFull left && isFull right && (height left) /= (height right) = lastChild left
	| isFull left && isFull right == False && (height left) == (height right) = lastChild right
	| otherwise = lastChild right	

--Function deleteLast
--deletes last Node in the heap
deleteLast :: Heap a -> Heap a
deleteLast (Node x Nil Nil) = Nil
deleteLast (Node x left Nil) = Node x Nil Nil
deleteLast (Node x left right)
	| isFull left == False && isFull right && (height left) /= (height right) = Node x (deleteLast left) right
	| isFull left && isFull right && (height left) /= (height right) = Node x (deleteLast left) right
	| isFull left && isFull right == False && (height left) == (height right) = Node x left (deleteLast right)
	| otherwise = Node x left (deleteLast right)


--Function trickleDown 
--heapifys the list after deleting min value
trickleDown :: Ord a => Heap a -> Heap a
trickleDown (Node x Nil Nil) = Node x Nil Nil
trickleDown (Node x (Node y left right) Nil)
	| x > y = Node y (Node x left right) Nil
	| otherwise = Node x (Node y left right) Nil
trickleDown (Node x (Node y left1 right1) (Node z left2 right2))
	| x > y && z >= y = Node y (trickleDown(Node x left1 right1)) (Node z left2 right2)
	| x > z && z <= y = Node z (Node y left1 right1) (trickleDown (Node x left2 right2))
	| otherwise = (Node x (Node y left1 right1) (Node z left2 right2))



--Function getMin 
--returns the min value in the given heap
getMin :: Heap t -> t
getMin Nil = error "Empty heap"
getMin (Node x _ _) = x


--Function has_kids
--checks to see if the root node
-- has two kids
has_kids :: Eq t => Heap t -> Bool
has_kids (Node x left right)
	| left == Nil || right == Nil = False
	| otherwise = True


--Function buildHeap
--takes a list and a heap then adds
-- each element of the list to the heap
buildHeap :: Ord a => [a] -> Heap a -> Heap a
buildHeap l h
	| null l = h
	| otherwise = buildHeap (tail l) (insert (head l) h)

-- Function heapSort
-- sorts a given list from lowest to highest
-- uses heap implementation 
-- i.e. build the heap from the list, and then repeatedly
-- obtaining the minimum element to construct the sorted list
heapSort :: Ord a => [a] -> [a]
heapSort [] = []
heapSort x = heap_to_list(buildHeap x Nil)

--Function heap_to_list
-- takes a heap and returns list form
heap_to_list :: Ord a => Heap a -> [a]
heap_to_list (Node x Nil Nil) = [x]
heap_to_list (Node x left right) = [x] ++ heap_to_list(deleteMin(Node x left right))












