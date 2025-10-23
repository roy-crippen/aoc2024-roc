module [dijkstra]

import Structures.PriorityQueue as Pq

Distances a : Dict a U64 where a implements Inspect & Eq & Hash
Predecessors a : Dict a a where a implements Inspect & Eq & Hash
AllPredecessors a : Dict a (List a) where a implements Inspect & Eq & Hash

# A function that given a node, returns successor nodes and their distances.
SuccessorsFunc a : a -> Distances a where a implements Inspect & Eq & Hash

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
# start, according to edges_from.
dijkstra : SuccessorsFunc a, a -> ShortestPaths a
dijkstra = |edges_from, start|
    dist = Dict.from_list [(start, 0)]
    q = Pq.from_list [(start, 0)] compare
    do_dijkstra edges_from dist (Dict.empty {}) q

do_dijkstra : SuccessorsFunc a, Distances a, Predecessors a, Pq.Queue (a, U64) -> ShortestPaths a
do_dijkstra = |edges_from, dist, pred, q|
    if Pq.is_empty q then
        { distances: dist, predecessors: pred }
    else
        ((u, _), q0) = Pq.pop q |> unwrap # change function return type to to Result
        (walk_dist, walk_pred, walk_q) = Dict.walk edges_from(u) (dist, pred, q0) |acc, v, uv_dist|
            (acc_dist, acc_pred, acc_q) = acc
            u_dist = Dict.get(acc_dist, u) |> unwrap # change function return type to to Result
            alt = u_dist + uv_dist
            when Dict.get(acc_dist, v) is
                Ok v_dist if alt >= v_dist -> (acc_dist, acc_pred, acc_q)
                _ -> (Dict.insert(acc_dist, v, alt), Dict.insert(acc_pred, v, u), Pq.push(acc_q, (v, alt)))

        do_dijkstra(edges_from, walk_dist, walk_pred, walk_q)

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
    dist = Dict.get(paths.distances, dest) |> unwrap # change function return type to to Result
    (List.reverse(path), dist)

# change function return type to to Result
do_shortest_path : Predecessors a, a -> List a where a implements Inspect & Eq & Hash
do_shortest_path = |predecessors, curr|
    when Dict.get(predecessors, curr) is
        Ok pred -> List.prepend(do_shortest_path(predecessors, pred), curr)
        _ -> [curr]

# /// Same as [`dijkstra`](#dijkstra), except each node predecessor is a `list` instead of a
# /// single node. If there are multiple shortest paths, junction nodes will have more than one
# /// predecessor.
# ///
# pub fn dijkstra_all(edges_from: SuccessorsFunc(node_id),
#                     start: node_id) -> AllShortestPaths(node_id)
# {
#   let dist = dict.from_list([#(start, 0)])
#   let q = pq.from_list([#(start, 0)], fn(a, b) { int.compare(a.1, b.1) })

#   do_dijkstra_all(edges_from, dist, dict.new(), q)
# }

# fn do_dijkstra_all(edges_from: SuccessorsFunc(node_id),
#                    dist: Distances(node_id), pred: AllPredecessors(node_id),
#                    q: Queue(#(node_id, Int))) -> AllShortestPaths(node_id)
# {
#   case pq.is_empty(q) {
#     True  -> AllShortestPaths(dist, pred)
#     False -> {
#       let assert Ok(#(#(u, _), q)) = pq.pop(q)
#       let #(dist, pred, q) = dict.fold(edges_from(u), #(dist, pred, q), fn(acc, v, uv_dist) {
#         let #(dist, pred, q) = acc
#         let assert Ok(u_dist) = dict.get(dist, u)
#         let alt = u_dist + uv_dist
#         case dict.get(dist, v) {
#           Ok(v_dist) if alt >  v_dist -> acc  //If already have a shorter route, then no changes.
#           Ok(v_dist) if alt == v_dist -> {    //If already have a same dist route, then
#             #(dist,                           //  leave dist alone,
#               dict.upsert(pred, v, fn(x) {
#                 case x { Some(i) -> [u, ..i]  //  prepend to pred,
#                          _ -> panic as "BUG" }}),
#               q)}                             //  and leave q alone.
#           _ -> #(dict.insert(dist, v, alt),   //Otherwise this is the shortest route, so update dist,
#                  dict.insert(pred, v, [u]),   //  pred,
#                  pq.push(q, #(v, alt)))       //  and q.
#         }
#       })
#       do_dijkstra_all(edges_from, dist, pred, q)
#     }
#   }
# }

# /// Same as [`shortest_path`](#shortest_path), except for [`dijkstra_all`](#dijkstra_all).
# ///
# pub fn shortest_paths(all_paths: AllShortestPaths(node_id), dest: node_id) -> #(List(List(node_id)), Int)
# {
#   let paths = do_shortest_paths(all_paths.predecessors, [], dest)
#   let assert Ok(dist) = dict.get(all_paths.distances, dest)
#   #(paths, dist)
# }

# fn do_shortest_paths(predecessors: AllPredecessors(node_id), path: List(node_id),
#                      curr: node_id) -> List(List(node_id))
# {
#   let new_path = [curr, ..path]
#   case dict.get(predecessors, curr) {
#     Error(_)  -> [new_path]
#     Ok(preds) -> list.flat_map(preds, do_shortest_paths(predecessors, new_path, _))
#   }
# }

unwrap : [Err *, Ok a] -> a
unwrap = |result|
    when result is
        Ok(x) -> x
        Err(_) -> crash "failed to unwrap value from Result"

# tests
# test graph definition from:
#   https://www.geeksforgeeks.org/dsa/dijkstras-algorithm-for-adjacency-list-representation-greedy-algo-8/

test_f_Succ : SuccessorsFunc U64
test_f_Succ = |node_id|
    when node_id is
        0 -> Dict.from_list([(1, 4), (7, 8)])
        1 -> Dict.from_list([(0, 4), (7, 11), (2, 8)])
        7 -> Dict.from_list([(0, 8), (1, 11), (8, 8), (6, 1)])
        2 -> Dict.from_list([(1, 8), (8, 2), (3, 7), (5, 4)])
        8 -> Dict.from_list([(7, 7), (2, 2), (6, 6)])
        6 -> Dict.from_list([(7, 1), (8, 6), (5, 2)])
        3 -> Dict.from_list([(2, 7), (5, 14), (4, 9)])
        5 -> Dict.from_list([(6, 2), (2, 4), (3, 14), (4, 10)])
        4 -> Dict.from_list([(3, 9), (5, 10)])
        _ -> crash "bug"
test_paths = dijkstra test_f_Succ 0
test_shortest_path_pair = shortest_path test_paths 4

expect test_paths.distances == [(0, 0), (1, 4), (2, 12), (3, 19), (4, 21), (5, 11), (6, 9), (7, 8), (8, 14)] |> Dict.from_list
expect has_path_to test_paths 4
expect test_shortest_path_pair.1 |> dbg == 21
expect test_shortest_path_pair.0 |> dbg == [0, 7, 6, 5, 4]
