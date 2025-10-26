module [
    Distances,
    Predecessors,
    AllPredecessors,
    ShortestPaths,
    AllShortestPaths,
    Successors,
    dijkstra,
    has_path_to,
    shortest_path,
    dijkstra_all,
    shortest_paths,
]

import Structures.PriorityQueue as Pq

Distances a : Dict a U64 where a implements Inspect & Eq & Hash
Predecessors a : Dict a a where a implements Inspect & Eq & Hash
AllPredecessors a : Dict a (List a) where a implements Inspect & Eq & Hash
Successors a : Dict a (Distances a) where a implements Inspect & Eq & Hash

# The return type of dijkstra. Consists of two dictionaries that contain,
# for every visited node, the shortest distance to that node and the node's immediate
# predecessor on that shortest path.
ShortestPaths a : { distances : Distances a, predecessors : Predecessors a } where a implements Inspect & Eq & Hash

# Same as ShortestPaths, except for dijkstra_all.
# The `distances` field is the same as `ShortestPaths` since there is only one shortest
# distance. But the `predecessors` field has a list of predecessors for each node instead
# of a single predecessor, to represent the possibility of there being multiple paths
# that result in the same shortest distance.
AllShortestPaths a : { distances : Distances a, predecessors : AllPredecessors a }

compare : (a, U64), (a, U64) -> [LT, EQ, GT] where a implements Inspect & Eq & Hash
compare = |p1, p2| Num.compare p1.1 p2.1

# Run Dijkstra's algorithm to determine the shortest path to every node reachable from
# start, according to successors.
dijkstra : Successors a, a -> ShortestPaths a
dijkstra = |successors, start|
    dist = Dict.from_list [(start, 0)]
    q = Pq.from_list [(start, 0)] compare
    do_dijkstra successors dist (Dict.empty {}) q

do_dijkstra : Successors a, Distances a, Predecessors a, Pq.Queue (a, U64) -> ShortestPaths a
do_dijkstra = |successors, dist, pred, q|
    if Pq.is_empty q then
        { distances: dist, predecessors: pred }
    else
        ((u, _), q0) = Pq.pop q |> msg_unwrap "Pq.pop q failed in do_dijkstra" # change function return type to to Result
        # dbg u
        edges = Dict.get(successors, u) |> msg_unwrap "Dict.get(successors, u) failed in do_dijkstra" # change function return type to to Result
        (walk_dist, walk_pred, walk_q) = Dict.walk edges (dist, pred, q0) |acc, v, uv_dist|
            (acc_dist, acc_pred, acc_q) = acc
            u_dist = Dict.get(acc_dist, u) |> msg_unwrap "Dict.get(acc_dist, u) failed in do_dijkstra" # change function return type to to Result
            alt = u_dist + uv_dist
            when Dict.get(acc_dist, v) is
                Ok v_dist if alt >= v_dist -> acc
                _ ->
                    # dbg  (v, alt)
                    (Dict.insert(acc_dist, v, alt), Dict.insert(acc_pred, v, u), Pq.push(acc_q, (v, alt)))

        do_dijkstra(successors, walk_dist, walk_pred, walk_q)

# Return true if Dijkstra's algorithm found a path to the `dest` node.
#
# Recall that in order to determine the shortest path, Dijkstra's algorithm visits all
# nodes reachable from the given start node. Thus we can exploit that to determine whether
# any particular node is reachable, without looking at the graph again.
has_path_to : ShortestPaths a, a -> Bool
has_path_to = |paths, dest| Dict.contains paths.distances dest

# When applied to the result of `dijkstra`, returns the shortest path to the
# `dest` node as a list of successive nodes, and the total length of that path.
shortest_path : ShortestPaths a, a -> (List a, U64)
shortest_path = |paths, dest|
    path = do_shortest_path(paths.predecessors, dest)
    dist = Dict.get(paths.distances, dest) |> msg_unwrap "Dict.get(paths.distances, dest) failed in shortest_path" # change function return type to to Result
    (List.reverse(path), dist)

# change function return type to to Result
do_shortest_path : Predecessors a, a -> List a where a implements Inspect & Eq & Hash
do_shortest_path = |predecessors, curr|
    when Dict.get(predecessors, curr) is
        Ok pred -> List.append(do_shortest_path(predecessors, pred), curr)
        _ -> [curr]

# Same as `dijkstra`, except each node predecessor is a `list` instead of a
# single node. If there are multiple shortest paths, junction nodes will have more than one
# predecessor.
dijkstra_all : Successors a, a -> AllShortestPaths a
dijkstra_all = |successors, start|
    dist = Dict.from_list([(start, 0)])
    q = Pq.from_list([(start, 0)], compare)
    do_dijkstra_all(successors, dist, Dict.empty {}, q)

do_dijkstra_all : Successors a, Distances a, AllPredecessors a, Pq.Queue (a, U64) -> AllShortestPaths a
do_dijkstra_all = |successors, dist, pred, q|
    if Pq.is_empty(q) then
        { distances: dist, predecessors: pred }
    else
        ((u, _), q0) = Pq.pop(q) |> msg_unwrap "Pq.pop(q) failed in do_dijkstra_all" # change function return type to to Result
        edges = Dict.get(successors, u) |> msg_unwrap "Dict.get(successors, u) failed in do_dijkstra_all" # change function return type to to Result
        (walk_dist, walk_pred, walk_q) = Dict.walk edges (dist, pred, q0) |acc, v, uv_dist|
            alter_value : Result (List _) [Missing] -> Result (List _) [Missing]
            alter_value = |possible_preds|
                when possible_preds is
                    Ok xs -> List.append xs u |> Ok
                    _ -> crash "this is a bug in alter_value"
            (acc_dist, acc_pred, acc_q) = acc
            u_dist = Dict.get(acc_dist, u) |> msg_unwrap "Dict.get(acc_dist, u) failed in do_dijkstra_all" # change function return type to to Result
            alt = u_dist + uv_dist
            when Dict.get(acc_dist, v) is
                Ok(v_dist) if alt > v_dist -> acc
                Ok(v_dist) if alt == v_dist -> (acc_dist, Dict.update(acc_pred, v, alter_value), acc_q)
                _ -> (Dict.insert(acc_dist, v, alt), Dict.insert(acc_pred, v, [u]), Pq.push(acc_q, (v, alt)))

        do_dijkstra_all(successors, walk_dist, walk_pred, walk_q)

# Same as `shortest_path`, except for `dijkstra_all`.
shortest_paths : AllShortestPaths a, a -> (List (List a), U64)
shortest_paths = |all_paths, dest|
    paths = do_shortest_paths(all_paths.predecessors, [], dest)
    dist = Dict.get(all_paths.distances, dest) |> msg_unwrap "Dict.get(all_paths.distances, dest) failed in shortest_paths"
    (paths, dist)

do_shortest_paths : AllPredecessors a, List a, a -> List (List a) where a implements Inspect & Eq & Hash
do_shortest_paths = |predecessors, path, curr|
    new_path = List.append path curr
    when Dict.get(predecessors, curr) is
        Ok(preds) -> List.map(preds, |p| do_shortest_paths(predecessors, new_path, p)) |> List.join
        _ -> [new_path]

msg_unwrap : [Err *, Ok a], Str -> a
msg_unwrap = |result, message|
    when result is
        Ok(x) -> x
        Err(_) -> crash(message)

# tests
# test graph definition from:
#   https://www.geeksforgeeks.org/dsa/dijkstras-algorithm-for-adjacency-list-representation-greedy-algo-8/

test_successors : Successors U64
test_successors =
    Dict.from_list [
        (0, Dict.from_list([(1, 4), (7, 8)])),
        (1, Dict.from_list([(0, 4), (7, 11), (2, 8)])),
        (7, Dict.from_list([(0, 8), (1, 11), (8, 8), (6, 1)])),
        (2, Dict.from_list([(1, 8), (8, 3), (3, 7), (5, 4)])),
        (8, Dict.from_list([(7, 7), (2, 3), (6, 6)])),
        (6, Dict.from_list([(7, 1), (8, 6), (5, 2)])),
        (3, Dict.from_list([(2, 7), (5, 14), (4, 9)])),
        (5, Dict.from_list([(6, 2), (2, 4), (3, 14), (4, 10)])),
        (4, Dict.from_list([(3, 9), (5, 10)])),
    ]

test_paths = dijkstra test_successors 0
expect
    ds = test_paths.distances
    ds == [(0, 0), (1, 4), (2, 12), (3, 19), (4, 21), (5, 11), (6, 9), (7, 8), (8, 15)] |> Dict.from_list
expect has_path_to test_paths 4

test_shortest_path_pair = shortest_path test_paths 4
expect test_shortest_path_pair.1 == 21
expect test_shortest_path_pair.0 == [4, 5, 6, 7, 0]

test_all_paths = dijkstra_all test_successors 0
expect
    ds = test_all_paths.distances
    ds == [(0, 0), (1, 4), (7, 8), (2, 12), (8, 15), (6, 9), (5, 11), (3, 19), (4, 21)] |> Dict.from_list
expect
    ps = test_all_paths.predecessors
    ps == [(1, [0]), (7, [0]), (2, [1]), (8, [6, 2]), (6, [7]), (5, [6]), (3, [2]), (4, [5])] |> Dict.from_list

test_shortest_path_all = shortest_paths test_all_paths 8
expect test_shortest_path_all.1 == 15
expect test_shortest_path_all.0 == [[8, 6, 7, 0], [8, 2, 1, 0]]
