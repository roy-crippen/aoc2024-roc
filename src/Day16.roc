module [solution_day_16]

import Util exposing [Solution]
import "../data/day_16.txt" as input_str : Str

import Structures.Dijkstra as Dk
import Structures.Grid as Gr

Node : (U64, Gr.Dir)

build_successors_dict : Gr.Grid U8 -> Dk.Successors Node
build_successors_dict = |g|
    find_edges : U64 -> List (Node, Dk.Distances Node)
    find_edges = |idx|
        ns_idx = Gr.neighbors4_unsafe g idx # [N,W,S,E] [0,1,2,3]
        when ns_idx is
            [n_idx, w_idx, s_idx, e_idx] ->
                has_n_idx = Set.contains idx_set n_idx
                has_w_idx = Set.contains idx_set w_idx
                has_s_idx = Set.contains idx_set s_idx
                has_e_idx = Set.contains idx_set e_idx

                north_k = (idx, N)
                n_edge0 = if has_n_idx then Ok ((n_idx, N), 1) else if has_s_idx then Ok ((s_idx, S), 2000) else Err Invalid
                w_edge0 = if has_w_idx then Ok ((w_idx, W), 1001) else Err Invalid
                s_edge0 = if has_s_idx then Ok ((s_idx, S), 2001) else Err Invalid
                e_edge0 = if has_e_idx then Ok ((e_idx, E), 1001) else Err Invalid
                north_dict = [n_edge0, w_edge0, s_edge0, e_edge0] |> List.keep_oks (|node| node) |> Dict.from_list

                west_k = (idx, W)
                n_edge1 = if has_n_idx then Ok ((n_idx, N), 1001) else Err Invalid
                w_edge1 = if has_w_idx then Ok ((w_idx, W), 1) else if has_e_idx then Ok ((e_idx, E), 2000) else Err Invalid
                s_edge1 = if has_s_idx then Ok ((s_idx, S), 1001) else Err Invalid
                e_edge1 = if has_e_idx then Ok ((e_idx, E), 2000) else Err Invalid
                west_dict = [n_edge1, w_edge1, s_edge1, e_edge1] |> List.keep_oks (|node| node) |> Dict.from_list

                south_k = (idx, S)
                n_edge2 = if has_n_idx then Ok ((n_idx, N), 2000) else Err Invalid
                w_edge2 = if has_w_idx then Ok ((w_idx, W), 1001) else Err Invalid
                s_edge2 = if has_s_idx then Ok ((s_idx, S), 1) else if has_n_idx then Ok ((n_idx, N), 2000) else Err Invalid
                e_edge2 = if has_e_idx then Ok ((e_idx, E), 1001) else Err Invalid
                south_dict = [n_edge2, w_edge2, s_edge2, e_edge2] |> List.keep_oks (|node| node) |> Dict.from_list

                east_k = (idx, E)
                n_edge3 = if has_n_idx then Ok ((n_idx, N), 1001) else Err Invalid
                w_edge3 = if has_w_idx then Ok ((w_idx, W), 2000) else Err Invalid
                s_edge3 = if has_s_idx then Ok ((s_idx, S), 1001) else Err Invalid
                e_edge3 = if has_e_idx then Ok ((e_idx, E), 1) else if has_w_idx then Ok ((w_idx, W), 2000) else Err Invalid
                east_dict = [n_edge3, w_edge3, s_edge3, e_edge3] |> List.keep_oks (|node| node) |> Dict.from_list

                [(north_k, north_dict), (west_k, west_dict), (south_k, south_dict), (east_k, east_dict)]

            _ -> crash "invalid number of elements from Gr.neighbors4"

    indexes = Gr.find_positions(g, |ch| ch != '#')
    idx_set = indexes |> Set.from_list
    indexes |> List.map (|index| find_edges(index)) |> List.join |> Dict.from_list

parse : Str -> (Gr.Grid U8, U64, U64)
parse = |s|
    ls = s |> Str.split_on "\n" |> List.map Str.to_utf8
    rows = List.len ls
    cols = List.first ls |> Util.unwrap |> List.len
    data = List.join ls
    g = { data, rows, cols }
    start_node = List.find_first_index(data, |ch| ch == 'S') |> Util.unwrap
    end_node = List.find_first_index(data, |ch| ch == 'E') |> Util.unwrap
    (g, start_node, end_node)

find_shortest_dist : Dk.Distances Node, U64 -> U64
find_shortest_dist = |dist, idx|
    [Dict.get dist (idx, N), Dict.get dist (idx, W), Dict.get dist (idx, S), Dict.get dist (idx, E)]
    |> List.keep_oks |v| v
    |> List.min
    |> Result.with_default Num.max_u64

find_node_count : Dk.AllShortestPaths Node, U64 -> U64
find_node_count = |all_paths, idx|
    min_dist = find_shortest_dist(all_paths.distances, idx)
    Dict.to_list all_paths.distances
    |> List.keep_if |((i, _dir), dist)| i == idx and dist == min_dist
    |> List.map
        (|(node, _dist)|
            (shortest_paths, _dist) = Dk.shortest_paths(all_paths, node)
            List.join shortest_paths |> List.map (|(index, _dir)| index))
    |> List.join
    |> Set.from_list
    |> Set.len

solution_day_16 : Solution
solution_day_16 = {
    day: 16,
    input_str,
    part1,
    part2,
    expected_part1,
    expected_part2,
}

expected_part1 : U64
expected_part1 = 106512

expected_part2 : U64
expected_part2 = 563

part1 : Str -> [Err Str, Ok U64]
part1 = |in_str|
    (g, start_idx, end_idx) = parse(in_str)
    successors = build_successors_dict(g)
    paths = Dk.dijkstra(successors, (start_idx, E))
    Ok find_shortest_dist(paths.distances, end_idx)

part2 : Str -> [Err Str, Ok U64]
part2 = |in_str|
    (g, start_idx, end_idx) = parse(in_str)
    successors = build_successors_dict(g)
    all_paths = Dk.dijkstra_all(successors, (start_idx, E))
    Ok find_node_count(all_paths, end_idx)

example_str : Str
example_str =
    """
    ###############
    #.......#....E#
    #.#.###.#.###.#
    #.....#.#...#.#
    #.###.#####.#.#
    #.#.#.......#.#
    #.#.#####.###.#
    #...........#.#
    ###.#.#####.#.#
    #...#.....#.#.#
    #.#.#.###.#.#.#
    #.....#...#.#.#
    #.###.#.#.#.#.#
    #S..#.....#...#
    ###############
    """

# tests

expect part1 example_str == Ok 7036
# expect part1 input_str == Ok expected_part1
expect part2 example_str == Ok 45

