-- Mariia Semenenko
-- Student ID: 22100679

-- Define a new type Coord for a pair of two Int values, which will be used
-- to represent the row and column indices (coordinates) in the 2D list.
type Coord = (Int, Int)

-- Mark a cell at position (i, j) as visited by replacing its value with the opposite one;
-- ensure each cell is visited only once and there is no infinite loop while searching.
visited :: Int -> Int -> a -> [[a]] -> [[a]]
visited i j x bi = take i bi ++ -- create a new list that contains the first i elements of bi (binary image)
    [take j (bi !! i) ++ [x] ++ drop (j + 1) (bi !! i)] ++ -- replace the j-th element of the i-th row with x
    drop (i + 1) bi -- create a new list that contains all elements of bi after the i-th row

-- Create a list of potential neighbours (i-1,j), (i+1,j), (i,j-1) and (i,j+1) with the same value v;
-- filter out neighbours whose row or column index is out of bounds, or whose value is not equal to v.
neighbours :: [[Int]] -> Coord -> Int -> [Coord]
neighbours l (i, j) v = [(i', j') | (i', j') <-
    [(i-1,j), (i+1,j), (i,j-1), (i,j+1)], -- potential neighbours in all directions (above, below, left, right)
    i' >= 0, i' < length l, j' >= 0, -- ensure the neighbouring coordinates are within the bounds of l
    j' < length (l !! 0), l !! i' !! j' == v] -- ensure the value at the neighbouring coordinate is equal to v

-- Given a matrix of integers, a value to search and a starting coordinate, return the number of cells
-- that can be visited to reach all occurrences of the search value, starting from the given coordinate.
search :: [[Int]] -> Int -> Coord -> Int
-- The search function marks the starting coordinate as visited and calls the helper function search'
-- with a queue containing only the starting coordinate, the modified matrix and an accumulator set to 0.
search l v (i, j) = search' [(i,j)] (visited i j (1-v) l) 0
  where
    -- If the queue is empty, the search is finished and the accumulated count is returned.
    search' [] _ acc = acc + 1 -- add one in order to count the starting cell
    -- If the queue is not empty, the function dequeues a coordinate, finds its neighbours with the same value,
    -- marks them as visited, and enqueues them to the queue. This process is repeated until the queue is empty.
    -- The function accumulates the count of visited cells in the 'acc' parameter.
    search' q l acc =
      let (i', j') = head q -- get the first cell coordinates in the queue
          q' = tail q -- get the queue without the first element
          ns = neighbours l (i', j') v -- find the neighbours of the current cell
          l' = foldr (\(i'',j'') lacc -> visited i'' j'' (1-v) lacc) l ns -- mark the neighbours as visited
          acc' = acc + length ns -- increment the visited cell count by the number of neighbours found
          q'' = q' ++ ns -- add the neighbours to the queue
      in search' q'' l' acc' -- continue the search recursively with the updated queue and visited cells count

nlcc :: [[Int]] -> Int -> Int
-- Search for the maximum number of connected cells with the same value v in the list l.
nlcc l v = maximum [search l v (i,j) | -- find the maximum value
    i <- [0..length l - 1], -- cover all rows of l
    j <- [0..length (l !! 0) - 1], -- cover all columns of the first row of l 
    l !! i !! j == v] -- check the element at index (i, j) is equal to the value v