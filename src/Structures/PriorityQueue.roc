module [Queue, new, pop, peek, push, is_empty, count, reorder, from_list, to_list]

import Structures.PairingHeap as Ph

Queue a : Ph.Heap a where a implements Inspect & Eq

# Creates a new empty heap with the provided comparison function.
new : Ph.Compare a -> Queue a
new = |compare| Ph.new compare

# Removes and returns the minimum element from the priority queue,
# along with the new queue.
pop : Queue a -> Result (a, Queue a) [EmptyHeap]
pop = |queue| Ph.delete_min queue

# Returns the minimum element in the priority queue without removing it.
peek : Queue a -> Result a [EmptyHeap]
peek = |queue| Ph.find_min queue

# Inserts a new element into the priority queue.
push : Queue a, a -> Queue a
push = |queue, item| Ph.insert queue item

# Checks whether the priority queue is empty or not.
is_empty : Queue a -> Bool
is_empty = |queue| Ph.find_min queue |> Result.is_err

# Returns the number of elements in the priority queue.
count : Queue a -> U64
count = |queue| Ph.count queue

# Rebuilds the priority queue with a new comparison function.
reorder : Queue a, Ph.Compare a -> Queue a
reorder = |queue, compare|
    when Ph.delete_min queue is
        Ok (x, q) -> Ph.insert(reorder(q, compare), x)
        Err _ -> new compare

# Creates a new priority queue from a list of elements and a comparison function.
from_list : List a, Ph.Compare a -> Queue a
from_list = |xs, compare|
    List.walk xs (new compare) Ph.insert

# Converts the priority queue to a list, preserving the order of elements.
to_list : Queue a -> List a
to_list = |queue|
    when Ph.delete_min queue is
        Ok (x, q) -> List.prepend (to_list q) x
        Err _ -> []

# tests

empty_queue : Queue U64
empty_queue = new Num.compare

test_queue = from_list [3, 2, 5, 1, 4] Num.compare

expect peek test_queue == Ok 1
expect count test_queue == 5
expect is_empty empty_queue == Bool.true
expect is_empty test_queue == Bool.false
expect to_list test_queue == [1, 2, 3, 4, 5]
expect
    (x, q) = pop test_queue |> Result.with_default (0, empty_queue)
    x == 1 and to_list q == [2, 3, 4, 5]

