import Data.Char
import System.IO
import Data.List
-- import Data.List.Split
import Data.Char

getFirstTuple (x,_) = x
getSecondTuple (_,x) = x


splitList (x:xs) list
  | x /= [] = splitList xs (list ++ [x])
  | x == [] = (list, xs)

-- Statement
  -- [[]]

main :: IO()
main = do
    contents <- getContents
    let
        input = map words (lines contents)
        tuple = splitList input []
        list1 = getFirstTuple tuple
        aux = getSecondTuple tuple
        aux2 = splitList aux []
        list2 = getFirstTuple aux2
        list3 = getSecondTuple aux2

        -- CORRECT PATHS GENERATING PROCESS --------------------------------------------

        main_list = list1

        destiny_node = getDestNode list3
        source_node = getSrcNode list3

        getSrcNode list = src
          where [[src, _]] = list

        getDestNode list = dest
          where [[_, dest]] = list

        getTransport [] acc = acc
        getTransport (t:ts) acc = getTransport ts (acc ++ [(name, ((convertStringToFloat time)/2))])
          where [name, time] = t

        transportList = getTransport list2 []

        createEdgesList [] edges = edges
        createEdgesList (x:xs) edges
          | edgeInEdgesList edges (src, dest) == False = createEdgesList xs (edges ++ [(src, dest, [])])
          | otherwise = createEdgesList xs edges
          where [src, dest, _, _] = x

        edgeInEdgesList [] (src, dest) = False
        edgeInEdgesList (x:xs) (src, dest)
          | x_src == src && x_dest == dest = True
          | otherwise = edgeInEdgesList xs (src, dest)
          where (x_src, x_dest, _) = x

        edge_list = createEdgesList main_list []

        groupEdges [] result = result
        groupEdges (x:xs) result = groupEdges xs (updateEdge result ((extractEdgeSrc x), (extractEdgeDest x)) (makeTuple x))

        extractEdgeSrc [src, _, _, _] = src
        extractEdgeDest [_, dest, _, _] = dest
        makeTuple [src, dest, transport, weight] = (src, dest, transport, weight)

        updateEdge [] e new = []
        updateEdge (x:xs) e new
          | e_src == src && e_dest == dest = [(src, dest, edges ++ [new])] ++ updateEdge xs e new
          | e_src /= src || e_dest /= dest = [x] ++ updateEdge xs e new
          where (src, dest, edges) = x
                (e_src, e_dest) = e

        grouped_edges = groupEdges main_list edge_list

        retrieveEdges [] result = result
        retrieveEdges (x:xs) result = retrieveEdges xs (result ++ [edges])
          where (_, _, edges) = x

        combinations :: Int -> [a] -> [[a]]
        combinations 0 _ = [[]]
        combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1]
                                          , x <- combinations (n-1) (drop (i+1) xs) ]

        edges_by_group = retrieveEdges grouped_edges []

        number_of_slots = (countVertices edges_by_group 0)

        edges_combinations = combinations number_of_slots main_list

        removeDupsInCombinations [] acc = acc
        removeDupsInCombinations (x:xs) acc = removeDupsInCombinations xs (acc ++ [(removeDups x [])])

        -- Have to remove dups in each list
        removeDups [] acc = acc
        removeDups (x:xs) acc
          | (verify x xs) == True = []
          | (verify x xs) == False =  removeDups xs (acc ++ [x])

        verify x [] = False
        verify x (y:ys)
          | x_src == y_src && x_dest == y_dest = True
          | otherwise = verify x ys
          where [x_src, x_dest, _, _] = x
                [y_src, y_dest, _, _] = y

        correct_combinations = removeDupsInCombinations edges_combinations []

        createListOfCorrectPaths [] acc = acc
        createListOfCorrectPaths (x:xs) acc
          | x == [] = createListOfCorrectPaths xs acc
          | otherwise = createListOfCorrectPaths xs (acc ++ [x])


        ROLDAWODAWFIOAWJFIOJAWODIJAWIODJAWIODAJW 12635621367278321983

        correct_paths = createListOfCorrectPaths correct_combinations []

        cretePathOfTuples [] acc = acc
        cretePathOfTuples (x:xs) acc = cretePathOfTuples xs (acc ++ [(transformToTuple x [])])

        transformToTuple [] acc = acc
        transformToTuple (x:xs) acc = transformToTuple xs (acc ++ [(a, b, c ,d)])
          where [a, b, c, d] = x

        tuple_correct_paths = cretePathOfTuples correct_paths []


        -- GRAPH GENERATION PROCESS ----------------------------------------------------

        createNodesList [] nodes = nodes
        createNodesList (x:xs) nodes
          | nodeInNodesList nodes src == False = createNodesList xs (nodes ++ [src])
          | nodeInNodesList nodes dest == False = createNodesList xs (nodes ++ [dest])
          | otherwise = createNodesList xs nodes
          where (src, dest, _, _) = x

        nodeInNodesList [] node = False
        nodeInNodesList (x:xs) node
          | x == node = True
          | otherwise = nodeInNodesList xs node

        -- For every character in main_nodes create a vertice in graph
        addVerticesToGraph [] graph = graph
        addVerticesToGraph (x:xs) graph = addVerticesToGraph xs (graph ++ [(x, [])])

        vertices = (createNodesList edges [])

        countVertices [] counter = counter
        countVertices (x:xs) counter = countVertices xs (counter + 1)

        -- Given a list of edges and vertices -> produce a graph with adj list
        -- [('a', [('b', '0.1', 'transport')])]
        -- [("ana",[()]),("a",[()]),("b",[()]),("c",[()])]

        updateArray [] element new = []
        updateArray (x:xs) element new
          | x == element = [new] ++ updateArray xs element new
          | x /= element = [x] ++ updateArray xs element new

        addEdgesToGraph [] graph = graph
        addEdgesToGraph (e:es) graph = addEdgesToGraph es (updateGraph graph (extractSrc e) (extractEdge e))

        extractSrc (src, _, _, _) = src
        extractEdge (src, dest, transport , weight) = (src, dest, transport , weight)

        -- updateGraph :: [([Char], [([Char], [Char], [Char])])] -> [Char] -> ([Char], [Char], [Char]) -> [([Char], [([Char], [Char], [Char])])]
        updateGraph [] v new = []
        updateGraph (x:xs) v new
          | v == src = [(src, adj ++ [new])] ++ updateGraph xs v new
          | v /= src = [x] ++ updateGraph xs v new
          where (src, adj) = x

        edges = tuple_correct_paths !! 0

        createGraphs [] acc = acc
        createGraphs (x:xs) acc = createGraphs xs (acc ++ [createGraph x])

        -- main_graph = addVerticesToGraph vertices []
        --
        -- final_graph = addEdgesToGraph edges main_graph

        createGraph list = (addEdgesToGraph list (addVerticesToGraph (createNodesList list []) []))

        final_graphs = (createGraphs tuple_correct_paths [])

        -- GENERATE A PRIORITY QUEUE FOR EACH GRAPH ------------------------------------

        -- MinHeap (consider upperBound because, when this upperbound for this node is the current lowest distance to travel, by either choosing it or a lower entry in the same array, you keep on running a consistent algorithm)

        createHeapNodes [] heap_nodes source = heap_nodes
        createHeapNodes (x:xs) heap_nodes source
          |source == x = createHeapNodes xs (heap_nodes ++ [(x, 0.0,[])]) source
          |otherwise = createHeapNodes xs (heap_nodes ++ [(x, 1/0, [])]) source

        -- Extract the vertex with mimimum distance value
        getMin [] minimal = minimal
        getMin (x:xs) minimal
          | min_value >= x_value = getMin xs x
          | min_value < x_value = getMin xs minimal
          where (_, x_value, _) = x
                (_, min_value, _) = minimal

        minimalist = getMin heapVertices ("", 0.0, [(0.0, "none", [("","","",0.0)])])

        removeMin [] vertex_to_remove acc = acc
        removeMin (x:xs) vertex_to_remove acc
          | x /= vertex_to_remove = removeMin xs vertex_to_remove (acc ++ [x])
          | otherwise = removeMin xs vertex_to_remove acc


        heapVertices = createHeapNodes vertices [] source_node
        -- graph = final_graphs !! 0

        listOfHeapVertices [] acc = acc
        listOfHeapVertices (g:gs) acc = listOfHeapVertices gs (acc ++ [(createHeapNodes vertices [] source_node)])

        my_list_of_heapVertices = listOfHeapVertices final_graphs []

        dijkstraList [] graphs acc = acc
        dijkstraList (h:hs) (g:gs) acc = dijkstraList hs gs (acc ++ [(dijkstra h g ("", 0.0, [(0.0, "none", [("","","",0.0)])]))])

        result_list = dijkstraList my_list_of_heapVertices final_graphs []


        getBest [] lower lowest = lowest
        getBest (r:rs) lower lowest
          | r_time < lower = getBest rs r_time (r_time, r_path)
          | otherwise = getBest rs lower lowest
          where (r_vertex, r_time, r_path) = r


        -- main = print(getBest result_list (1/0)  (0.0, [(0.0, "none", [("","","",0.0)])]))

        best = (getBest result_list (1/0)  (0.0, [(0.0, "none", [("","","",0.0)])]))

        getPath path = path
            where (time, path) = best

        bestPath2 = getPath best_time

        pullFirst [] index acc = acc
        pullFirst (p:ps) index acc
          | index == 0 = pullFirst ps (index + 1) (acc ++ [p])
          | otherwise = pullFirst ps (index + 1) acc

        pullFirstPath [] index acc = acc
        pullFirstPath (p:ps) index acc
          | index == 0 = pullFirstPath ps (index + 1) (p)
          | otherwise = pullFirstPath ps (index + 1) acc

        bestPath = pullFirst bestPath2 0 []

        best_time = getTime (pullFirstPath bestPath 0 (0.0, "", []))

        getTime best = time
          where (time, mode, path) = best

        myFloatToStr :: Float -> String
        myFloatToStr x = show x

        getMainPath tuple = main_path
          where [main_path] = tuple
        --
        main_best_path = getMainPath bestPath

        getThePath tuple = main_path
          where (_, _, main_path) = tuple

        the_path = getThePath main_best_path

        transformPathToString [] acc = acc
        transformPathToString (t:ts) acc = transformPathToString ts (acc ++ (myF src) ++ (myF mode))
          where (src, _, mode, _) = t

        myF arg = arg ++ " "

        final_path_string = transformPathToString the_path ""

        appendDestiny str dest = str ++ dest

        final_path_string_appended = appendDestiny final_path_string destiny_node

        -- Traverse through all adjacent vertices of u and update their distance values
        dijkstra [] graph lastRemoved = lastRemoved
        dijkstra heapVertices graph lastRemoved
          | vertex == destiny_node = current_min
          | otherwise = dijkstra (removeMin updated_heap_final (current_min) []) graph current_min
          where current_min = (getMin heapVertices ("", 1/0,[]))
                (vertex, elapsed_time, paths) = current_min
                adjacents = (getAdjacents graph current_min)
                adjacents_converted = (convertEdges adjacents [])
                updated_heap = (updateAdjacents adjacents_converted heapVertices current_min)
                updated_heap_final = (fixHeapTimes updated_heap [])

        -- main = print(dijkstra heapVertices ("", 0.0, [(0.0, "none", [("","","",0.0)])]))

        getAdjacents [] current_min = []
        getAdjacents (x:xs) current_min
          | vertex == x_vertex = x_edges
          | otherwise = getAdjacents xs current_min
          where (vertex, elapsed_time, paths) = current_min
                (x_vertex, x_edges) = x

        convertEdges [] acc = acc
        convertEdges (e:es) acc = convertEdges es (acc ++ [(e_src, e_dest, e_mode, (convertStringToFloat e_time))])
          where (e_src, e_dest, e_mode, e_time) = e


        filterNumberFromString :: String -> String
        filterNumberFromString s =
            let allowedString = ['0'..'9'] ++ ['.', ',']
                toPoint n
                    | n == ',' = '.'
                    | otherwise = n

                f = filter (`elem` allowedString) s
                d = map toPoint f
            in d


        convertStringToFloat :: String -> Float
        convertStringToFloat s =
            let betterString = filterNumberFromString s
                asFloat = read betterString :: Float
            in asFloat


        fixHeapTimes [] acc = acc
        fixHeapTimes (h:hs) acc = fixHeapTimes hs (acc ++ [fixHeapTime h])

        fixHeapTime vertex
          | paths /= [] = (v_src, (getLargestTime paths 0.0), paths)
          | otherwise = (v_src, v_time, paths)
          where (v_src, v_time, paths) = vertex

        getLargestTime [] large = large
        getLargestTime (x:xs) large
          | time > large = getLargestTime xs time
          | otherwise = getLargestTime xs large
          where (time, _, _) = x


        updateAdjacents [] heapVertices current_min = heapVertices
        updateAdjacents (a:as) heapVertices current_min = updateAdjacents as (updateAdjacent a heapVertices current_min []) current_min

        updateAdjacent edge [] current_min acc = acc
        updateAdjacent edge (v:vs) current_min acc
          | e_dest == v_vertex = updateAdjacent edge vs current_min (acc ++ [(updatePaths edge v current_min)])
          | otherwise = updateAdjacent edge vs current_min (acc ++ [v])
          where (e_vertex, e_dest, e_mode, e_time) = edge
                (v_vertex, v_elapsed_time, v_paths) = v

        updatePaths edge vertex current_min = newPath
          where (v_vertex, v_elapsed_time, v_paths) = vertex
                newPath = (v_vertex, v_elapsed_time, v_paths ++ [(addPath edge current_min)])

        addPath edge current_min
          | time_to_add == 1/0 = (fixing_time, e_mode, bestPath)
          | otherwise = (time_to_add, e_mode, bestPath)
          where (e_src, e_dest, e_mode, e_time) = edge
                (vertex, elapsed_time, paths) = current_min
                (time_to_add, bestPath) = (getBestPathFromCurrentMin paths edge (1/0) [edge])
                fixing_time = (calcTime transportList e_mode e_time)


        getBestPathFromCurrentMin [] edge lower lowest = (lower, lowest)
        getBestPathFromCurrentMin (p:ps) edge lower lowest
          | p_mode == e_mode && time_sum < lower = getBestPathFromCurrentMin ps edge time_sum (p_paths ++ [edge])

          | p_mode /= e_mode && time_fix_sum < lower = getBestPathFromCurrentMin ps edge time_fix_sum ((p_paths ++ [(e_src, e_dest, e_mode, (calcTime transportList e_mode e_time))]))

          | otherwise = getBestPathFromCurrentMin ps edge lower lowest

          where (p_time, p_mode, p_paths) = p
                (e_src, e_dest, e_mode, e_time) = edge
                time_sum = p_time + e_time
                time_fix_sum = p_time + (calcTime transportList e_mode e_time)

        calcTime [] e_mode e_time = e_time
        calcTime (t:ts) e_mode e_time
          | mode == e_mode = e_time + t_time
          | otherwise = calcTime ts e_mode e_time
          where (mode, t_time) = t

    -- print(list1)
    -- print(list2)
    -- print(list3)
    -- print(transportList)
    -- print(number_of_slots)
    -- print(edges_combinations)
    -- print("Rola")
    -- print(result_list)
    -- print("Rola")
    -- print(best)
    -- print("Rola")
    -- print(bestPath)
    putStrLn(final_path_string_appended)
    putStrLn(myFloatToStr best_time)
