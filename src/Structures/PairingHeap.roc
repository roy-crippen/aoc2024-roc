module [Compare, Tree, Heap, new, insert, find_min, delete_min, merge, count]

# This module provides an implementation of the pairing heap data structure,
# a type of self-adjusting heap with efficient insert, find_min, and delete_min, and merge operations.

# Based on "Purely Functional Data Structures" by Okasaki (1998)
# A port from Gleam repository:
#     https://github.com/schurhammer/gleamy_structures/blob/main/src/gleamy/pairing_heap.gleam

Compare a : a, a -> [LT, EQ, GT] where a implements Inspect & Eq
Tree a : [Empty, Tree (a, List (Tree a))] where a implements Inspect & Eq
Heap a : { root : Tree a, compare : Compare a } where a implements Inspect & Eq

# Creates a new empty heap with the provided comparison function.
new : Compare a -> Heap a
new = |compare| { root: Empty, compare }

# Inserts a new item into the heap, preserving the heap property.
# Time complexity: O(1)
insert : Heap a, a -> Heap a
insert = |heap, key|
    root = merge_trees(Tree (key, []), heap.root, heap.compare)
    { root, compare: heap.compare }

# Returns the minimum element in the heap, if the heap is not empty.
# Time complexity: O(1)
find_min : Heap a -> Result a [EmptyHeap]
find_min = |heap|
    when heap.root is
        Tree (x, _) -> Ok x
        Empty -> Err EmptyHeap

# Removes and returns the minimum element from the heap along with the
# new heap after deletion, if the heap is not empty.
# Time complexity: O(log n) amortized
delete_min : Heap a -> Result (a, Heap a) [EmptyHeap]
delete_min = |heap|
    when heap.root is
        Tree (x, xs) -> Ok (x, { heap & root: merge_pairs(xs, heap.compare) })
        Empty -> Err EmptyHeap

# Merges two heaps into a new heap containing all elements from both heaps,
# preserving the heap property.
# The given heaps must have the same comparison function.
# Time complexity: O(1)
merge : Heap a, Heap a -> Heap a
merge = |heap1, heap2|
    compare = heap1.compare
    root = merge_trees heap1.root heap2.root compare
    { root, compare }

merge_trees : Tree a, Tree a, Compare a -> Tree a
merge_trees = |x, y, compare|
    when (x, y) is
        (x_tree, Empty) -> x_tree
        (Empty, y_tree) -> y_tree
        (Tree (xk, xs), Tree (yk, ys)) ->
            when compare xk yk is
                GT -> Tree (yk, List.prepend ys x)
                _ -> Tree (xk, List.prepend xs y)

        _ -> crash "merge_trees failed"

merge_pairs : List (Tree a), Compare a -> Tree a
merge_pairs = |l, compare|
    when l is
        [] -> Empty
        [h] -> h
        [h1, h2, .. as hs] ->
            merge_trees(
                merge_trees(h1, h2, compare),
                merge_pairs(hs, compare),
                compare,
            )

count : Heap a -> U64
count = |heap|
    go : (Tree a, U64) -> (Tree a, U64)
    go = |(tree, cnt)|
        when tree is
            Tree (_v, ts) ->
                (t0, cnt0) = List.walk
                    ts
                    (Empty, cnt)
                    (|(_, acc), t|
                        (_, cnt1) = go (t, acc)
                        (Empty, cnt1)
                    )
                (t0, cnt0 + 1)

            Empty -> (Empty, cnt)

    (_, tree_node_count) = go (heap.root, 0)
    tree_node_count

# tests

empty_heap : Heap U64
empty_heap = new Num.compare

expect empty_heap.root == Empty
expect find_min empty_heap == Err EmptyHeap
expect
    heap = empty_heap |> insert 3
    find_min heap == Ok 3
expect
    heap = empty_heap |> insert 3 |> insert 1
    find_min heap == Ok 1
expect
    heap = empty_heap |> insert 3 |> insert 1 |> insert 4
    find_min heap == Ok 1

delete_test_heap : Heap U64
delete_test_heap =
    empty_heap
    |> insert 3
    |> insert 1
    |> insert 4
    |> insert 1
    |> insert 5

d1 = delete_min delete_test_heap |> Result.with_default (0, empty_heap)
d2 = delete_min d1.1 |> Result.with_default (0, empty_heap)
d3 = delete_min d2.1 |> Result.with_default (0, empty_heap)
d4 = delete_min d3.1 |> Result.with_default (0, empty_heap)
d5 = delete_min d4.1 |> Result.with_default (0, empty_heap)

expect d1.0 == 1
expect d2.0 == 1
expect d3.0 == 3
expect d4.0 == 4
expect d5.0 == 5
expect find_min d5.1 == Err EmptyHeap

h_1 =
    new Num.compare
    |> insert 3
    |> insert 1
    |> insert 5

h_2 =
    new Num.compare
    |> insert 2
    |> insert 6
    |> insert 4

merged_heap = merge h_1 h_2

p1 = delete_min merged_heap |> Result.with_default (0, empty_heap)
p2 = delete_min p1.1 |> Result.with_default (0, empty_heap)
p3 = delete_min p2.1 |> Result.with_default (0, empty_heap)
p4 = delete_min p3.1 |> Result.with_default (0, empty_heap)
p5 = delete_min p4.1 |> Result.with_default (0, empty_heap)
p6 = delete_min p5.1 |> Result.with_default (0, empty_heap)
p7 = delete_min p6.1

expect p1.0 == 1
expect p2.0 == 2
expect p3.0 == 3
expect p4.0 == 4
expect p5.0 == 5
expect p6.0 == 6
expect Result.is_err p7

expect count h_1 == 3
expect count delete_test_heap == 5

count_test_heap : Heap U64
count_test_heap =
    empty_heap
    |> insert 3
    |> insert 1
    |> insert 4
    |> insert 1
    |> insert 5
    |> insert 3
    |> insert 10
    |> insert 4
    |> insert 11
    |> insert 5
    |> insert 6
    |> insert 8
    |> insert 3
    |> insert 5
    |> insert 66

expect count count_test_heap == 15
