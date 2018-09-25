-- First Lets learn about graphs in haskell

-- Our job is to make a program that takes input that represents a road system and print out what the shortest path across it is. Here's what the input would look like for this case:

  -- Road system input example∷
    -- 50
    -- 10
    -- 30
    -- 5
    -- 90
    -- 20
    -- 40
    -- 2
    -- 25
    -- 10
    -- 8
    -- 0

    -- Read each section like: Road A, Road B, crossing road.
    -- To have it neatly fit into threes we say that there's a last crossing section that takes 0 minutes to drive.

    -- we're going to solve this problem in three steps:
      -- Forget Haskell for a minute and think about how we'd solve the problem by hand
      -- Think about how we're going to represent our data in Haskell
         -- One way is to think of the starting points and crossroads as nodes of a graph that point to other crossroads
         -- If we imagine that the starting points actually point to each other with a road that has a length of one, we see that every crossroads (or node) points to the node on the other side and also to the next one on its side. Except for the last nodes, they just point to the other side.

-- data Node = Node Road Road | EndNode Road
-- data Road = Road Int Node

      -- Figure out how to operate on that data in Haskell so that we produce at a solution


  -- matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
  --
  -- getElement line colunm n matrix
  --   | (line < n) && (colunm < n) = ((matrix !! line) !! colunm)
  --   | otherwise = -1
  --
  --
  -- my_tuple = (5, True)
  -- first = fst (my_tuple)
  -- second = snd (my_tuple)
  --
  --
  -- main = print(getElement 0 1 3 matrix)

-- Create a simple weighted graph in haskell and do a djikstra

  -- Create data type for edge [dest, weight]

  -- Find a way to store those edges in a list

    -- Graph = [[(dest, weight), (dest, weight), (dest, weight)],
    --          [(dest, weight), (dest, weight), (dest, weight)],
    --          [(dest, weight), (dest, weight), (dest, weight)]
    --         ]

    -- Dont forget to count the number of nodes

-- paths = [[src, dest, weight, type]]
edges = [("ana", "b", "a-pe" , "0.1"), ("a", "c", "a-pe", "0.1"), ("b", "d", "a-pe", "0.1"), ("c", "d", "a-pe", "0.1"), ("a", "f", "a-pe", "0.1")]

src_dest_vertices = ["a", "d"]

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
extractEdge (src, dest, transport , weight) = (dest, transport , weight)

-- updateGraph :: [([Char], [([Char], [Char], [Char])])] -> [Char] -> ([Char], [Char], [Char]) -> [([Char], [([Char], [Char], [Char])])]
updateGraph [] v new = []
updateGraph (x:xs) v new
  | v == src = [(src, adj ++ [new])] ++ updateGraph xs v new
  | v /= src = [x] ++ updateGraph xs v new
  where (src, adj) = x

-- ///////////////////////////////////////////////////////////////////////////


number_of_vertices = (countVertices vertices 0)

main_graph = addVerticesToGraph vertices []

final_graph = addEdgesToGraph edges main_graph

-- main = print(final_graph)

-- ///////////////////////////////////////////////////////////////////////////
-- Given a list of [["src", "dest", "transport", "time"], ...], return a list of tuples like that
  -- [("f", "h", "linha-567", "1.2"), ("f", "h", "a-pe", "12.3")]

-- Generate all combinations

-- a -> h
--    a-pe : 0.3
--    linha-370  : 0.2
--    uber: 0.1
--
-- for every edge, if there are more than one way to traverse it, create a graph for each traverse method

  -- How can we deal with the waiting time?
    -- While generating the graph, keep track of flag variable of the satus of the choosen mehtod of parent

main_list = [["a", "b", "a-pe", "0.4"], ["b", "a", "a-pe", "0.6"], ["f", "h", "linha-567", "1.2"], ["f", "h", "a-pe", "12.3"]]

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

-- [("a","b",[]),("b","a",[]),("f","h",[])]

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

-- [[(...), (...)], [(...)]]

correct_combinations = removeDupsInCombinations edges_combinations []

createListOfCorrectPaths [] acc = acc
createListOfCorrectPaths (x:xs) acc
  | x == [] = createListOfCorrectPaths xs acc
  | otherwise = createListOfCorrectPaths xs (acc ++ [x])

correct_paths = createListOfCorrectPaths correct_combinations []

-- removeDups :: [Int] -> [Int] -> [Int]
-- removeDups [] acc = acc
-- removeDups (x:xs) acc
--   | (verify x xs) == True = removeDups xs acc
--   | (verify x xs) == False =  removeDups xs (acc ++ [x])
--
-- verify :: Int -> [Int] -> Bool
-- verify x [] = False
-- verify x (y:ys)
--   | x == y = True
--   | otherwise = verify x ys

public_transports = [["linha-567", "15.0"], ["linha-370", "12.0"]]


-- for each public_transport
  -- apply waitinTime algorithm for every path

-- [
-- [["a","b","a-pe","0.4"],["b","f","linha-567","0.6"],["f","h","linha-567","1.2"]],
-- [["a","b","a-pe","0.4"],["b","f","a-pe","0.6"],["f","h","a-pe","12.3"]]
-- ]
--
-- updatePublicTransportTimes [] paths = paths
-- updatePublicTransportTimes (x:xs) paths = updatePublicTransportTimes xs (verifyWaitingTime paths x)
--
--
-- verifyWaitingTime [] transport acc = acc
-- verifyWaitingTime (x:xs) transport acc = verifyWaitingTime xs transport (acc ++ (applyTime x transport))
--
-- applyTime (x:xs) transport last_mode =

-- Starting from the SOURCE-Vertex:
-- linear search for a src equals to destiny, until origin found.
  -- If src is equal to previous destiny
    -- If mode is equal -> Do Nothing
    -- If mode is different -> toggle mode
  -- Else
    -- keep going

simple = [['c', 'd', '3', '0.4'], ['a', 'b', '4', '0.4'], ['c', 'a', '4', '0.1']]
         -- -> [['a', 'b', '4', '0.6'], ['c', 'd', '3', '0.4'], ['b', 'c', '4', '0.1']]


-- For every, edge search for paths using the current edge as start point
  -- Apply waiting time algorithm if needed

-- updateWaitingTime [] path = path
-- updateWaitingTime (x:xs) path = updateWaitingTime xs (path ++ [(verifyTimes x xs)]
--
-- verifyTimes x [] toggle_mode = []
-- verifyTimes x (y:ys) toggle_mode
--   |
--   | otherwise
--   where [x_src, x_dest, x_transport, x_time] = x
--         [y_src, y_dest, y_transport, y_time] = y





main = print(correct_paths)





-- apply waiting time algorithm
  -- OBS: dont forget to observe changing of transport mode
-- For every list in correct_paths with waiting times for each public transport
-- transform them into simple graphs
-- Apply djikstra and store (shortestPath and min-time)
-- take the lowest time and print



-- [
--
-- [["a","b","a-pe","0.4"],["b","a","a-pe","0.6"],["f","h","linha-567","1.2"]],
-- [["a","b","a-pe","0.4"],["b","a","a-pe","0.6"],["f","h","a-pe","12.3"]],
-- [["a","b","a-pe","0.4"],["f","h","linha-567","1.2"],["f","h","a-pe","12.3"]],
-- [["b","a","a-pe","0.6"],["f","h","linha-567","1.2"],["f","h","a-pe","12.3"]]
--
-- ]

-- [("a","b",[("a","b","a-pe","0.4")]),("b","a",[("b","a","a-pe","0.6")]),("f","h",[("f","h","linha-567","1.2"),("f","h","a-pe","12.3")])]

-- [[("a","b","a-pe","0.4")],[("b","a","a-pe","0.6")],[("f","h","linha-567","1.2"),("f","h
-- ","a-pe","12.3")]]

-- Create all combinations


-- count number of generated arrays.

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1]
                                  , x <- combinations (n-1) (drop (i+1) xs) ]



-- Create a new vector for each list of edges that start and end at the same vertices


-- generate all combinations
-- apply waiting-time algorithm for each combination
-- create simple graphs for each combiantion
-- apply djikstra storing shortest-path and time
-- decide and print.



-- How do i make a combination?
  -- Generate all combinations with composite lists as an element
    -- Now, for each generated combination, generate a list



-- Generate lists for every element in an array∷


-- [
-- [("f", "h", "a-pe", "12.3"), ("a", "b", "a-pe", "0.4"), ("b", "a", "a-pe", "0.6")],
-- [("f", "h", "linha-567", "1.2"), ("a", "f", "a-pe", "0.4"), ("b", "a", "a-pe", "0.6")]
-- ]

-- How can we deal with the waiting time?
  -- Use mode flag as acc in your recursion to decide to include cost of waiting or not








  -- In order to print the path with their choices of transport we have to store the transport type at parents[] as  (parent, 'trasnport-type')


-- Second Lets define how our graph is going to be created

-- Third Define how can we adapt djikstra to our context

-- Code first attempt
