module PE2 where

-- PE2: Dungeon Crawler
-- Dungeon map is :: Tree Chamber [Encounter]
-- Each encounter is either a fight or a treasure
-- Fights deal you damage (reduce HP) but enemies drop some gold (add
-- gold)
-- Tresures just give gold, or potions (which give hp)
-- Nodes hold encounters, when you visit a node you go through all of them in order
-- You start with a certain amount of HP and 0 gold.
-- You lose HP and accumulate gold as you descend the tree and go through encounters

-- Polymorphic tree structure
data Tree a b = EmptyTree | Leaf a b | Node a b [Tree a b] deriving (Show, Eq)

-- Every location in the tree is of some Chamber type.
data Chamber = Cavern |
               NarrowPassage |
               UndergroundRiver |
               SlipperyRocks deriving (Show, Eq)

-- An enemy has a name, an amount of damage that it deals
-- and an amount of gold that it drops (in that order).
data Enemy = Enemy String Integer Integer deriving (Show, Eq)

-- Gold n gives n amount of gold
-- Potion n heals n hp
data Loot = Gold Integer | Potion Integer deriving (Show, Eq)

-- An encounter is either a Fight with an Enemy, or a treasure where
-- you find Loot
data Encounter = Fight Enemy | Treasure Loot deriving (Show, Eq)

-- This is a type synonym for how we will represents our dungeons
type Dungeon = Tree Chamber [Encounter]

-- First argument is starting HP
-- Second argument is the dungeon map
-- Third argument is the path (each integer in the list shows what child
-- you descend into)
-- Calculate how much HP you have left and how much gold you've
-- accumulated after traversing the given path
elemIndex :: Double -> [Double] -> Int -> Int
elemIndex _ [] _ = -1
elemIndex item (item1:lst) index = if item == item1 then index else elemIndex item lst (index+1)

elemIndex1 :: Integer -> [Integer] -> Int -> Int
elemIndex1 _ [] _ = -1
elemIndex1 item (item1:lst) index = if item == item1 then index else elemIndex1 item lst (index+1)

clearLeaf :: Integer -> Integer -> [Encounter] -> (Integer, Integer)
clearLeaf hp gold [] = (hp,gold)
clearLeaf hp gold ((Fight (Enemy name dmg drop)):encs) = clearLeaf (hp-dmg) (gold+drop) encs 
clearLeaf hp gold ((Treasure (Gold drop)):encs) = clearLeaf hp (gold+drop) encs
clearLeaf hp gold ((Treasure (Potion drop)):encs) = clearLeaf (hp+drop) gold encs

traversePathH :: Integer -> Integer -> Dungeon -> [Int] -> (Integer, Integer)
traversePathH hp gold EmptyTree _ = (hp,gold)
traversePathH hp gold (Leaf name encounters) _ = clearLeaf hp gold encounters
traversePathH hp gold (Node name encounters list) (int:lst) = traversePathH n_hp n_gold next lst where (n_hp,n_gold) = clearLeaf hp gold encounters; next = list !! int



traversePath :: Integer -> Dungeon -> [Int] -> (Integer, Integer)
traversePath hp = traversePathH hp 0 


-- First argument is starting HP
-- Second argument is dungeon map
-- Find which path down the tree yields the most gold for you
-- You cannot turn back, i.e. you'll find a non-branching path
-- You do not need to reach the bottom of the tree
-- Return how much gold you've accumulated
findMaximumGain :: Integer -> Dungeon -> Integer
findMaximumGain hp EmptyTree  = 0
findMaximumGain hp (Leaf name encounters) = if r_hp <= 0 then 0 else gold where (r_hp,gold) = clearLeaf hp 0 encounters
findMaximumGain hp (Node name encounters list) = if r_hp <= 0 then 0 else gold + maximum [findMaximumGain r_hp x | x <- list] where (r_hp,gold) = clearLeaf hp 0 encounters

-- First argument is starting HP
-- Second argument is the dungeon map
-- Remove paths that you cannot go thorugh with your starting HP. (By
-- removing nodes from tree).
-- Some internal nodes may become leafs during this process, make the
-- necessary changes in such a case.
findViablePaths :: Integer -> Dungeon -> Dungeon
findViablePaths _ EmptyTree = EmptyTree
findViablePaths hp (Leaf name encounters) = if r_hp >0 then Leaf name encounters else EmptyTree where r_hp = fst (clearLeaf hp 0 encounters )
findViablePaths hp (Node name encounters list) =if r_hp <=0 then EmptyTree else if r_list == []  then Leaf name encounters else Node name encounters r_list 
 where r_hp = fst (clearLeaf hp 0 encounters);
        r_list = filter (/= EmptyTree) [findViablePaths r_hp node | node <- list]

-- First argument is starting HP
-- Second Argument is dungeon map
-- Find, among the viable paths in the tree (so the nodes you cannot
-- visit is already removed) the two most distant nodes, i.e. the two
-- nodes that are furthest awat from each other.
change:: (Integer,Integer) -> Integer -> (Integer,Integer)
change (largest, secondLargest) y
          | y > largest = (y, largest)
          | y > secondLargest = (largest, y)
          | otherwise = (largest, secondLargest)

twoLargest :: [Integer] -> (Integer, Integer)
twoLargest [] = (0,0)
twoLargest (x:xs) = foldl change (x, x) xs
          
findDistance :: Dungeon -> Integer
findDistance EmptyTree = 0
findDistance (Leaf _ _)  = 0
findDistance (Node _ _ list)  = 1 + maximum [findDistance x | x <- list]
realDistance :: [Integer] -> Integer
realDistance list = if length list == 2 then 2+ sum list else if length list == 1 then 1 + head list else if list == [] then 0 else a1 + a2+2  where (a1,a2) = twoLargest list

mostDistantPairH :: Integer -> Dungeon -> (Integer, Dungeon)
mostDistantPairH _ EmptyTree = (0,EmptyTree)
mostDistantPairH hp (Leaf name encounters) = (0,Leaf name encounters)
mostDistantPairH hp (Node name encounters list) = (distance, (Node name encounters list)) where distance = realDistance [findDistance x | x <-list]
connectionPath:: Dungeon -> Bool -> Dungeon
connectionPath EmptyTree _ = EmptyTree
connectionPath (Leaf name encounters) _ = Leaf name encounters
connectionPath (Node name encounters list) True = if length list <=2 then (Node name encounters [connectionPath x False | x<-list]) else (Node name encounters [connectionPath x False | x<-list1])
    where list1 = reduce listo where listo = [(findDistance item, item) | item <- list]
connectionPath (Node name encounters list) False = if length list <=1 then (Node name encounters list) else (Node name encounters list0) where index = elemIndex1 (toInteger(maximum [toInteger(findDistance x) | x<-list ])) [toInteger(findDistance x) |x<-list ] 0; list0 = [x | x<- list, (list !! index) == x]
reduce :: [(Integer,Dungeon)] -> [Dungeon]
reduce list = [list1 !! index1, list1 !! index2] where list1 = [snd x | x <- list] ; list2 = [fst x | x<-list] ; (len1, len2) = twoLargest list2 ; index1 = elemIndex1 len1 list2 0; list3= fst (splitAt (index1) list2) ++ [toInteger (len1+1)] ++ tail (snd (splitAt (index1) list2)) ; index2 = elemIndex1 len2 list3 0
mostDistantPair :: Integer -> Dungeon -> (Integer, Dungeon)
mostDistantPair hp dungeon = mostDistantPairH hp (connectionPath (findViablePaths hp dungeon) True)
-- Find the subtree that has the highest total gold/damage ratio
-- Simply divide the total gold in the subtree by the total damage
-- in the subtree. You only take whole subtrees (i.e you can take a new
-- node as the root of your subtree, but you cannot remove nodes
-- below it). Note that the answer may be the whole tree.
divide :: Integer -> Integer -> Double
divide x y = fromIntegral x / fromIntegral y

findPair :: Integer -> Integer -> Dungeon -> (Integer,Integer)
findPair t_hp t_gold (Leaf name encounters) = ((t_hp-dmg),(t_gold +gold)) where (dmg,gold) = clearLeaf 0 0 encounters
findPair t_hp t_gold (Node name encounters list) =(hpp,goldd) where hpp = -(fst (clearLeaf 0 0 encounters)) + sum [fst (findPair 0 0 x) | x <- list] ; goldd = snd (clearLeaf 0 0 encounters) + sum [snd (findPair 0 0 x) | x <- list]
findScore :: Integer -> Integer -> Dungeon -> Double
findScore t_hp t_gold dungeon = if b <=0 then divide a 0 else divide a b where (b,a) = findPair 0 0 dungeon              --divide (t_gold +gold) (t_hp-dmg) where (dmg,gold) = clearLeaf 0 0 encounters
-- findScore t_hp t_gold (Node name encounters list) = maximum lst where (dmg,gold) = clearLeaf 0 0 encounters; lst = [findScore (-t_hp-dmg) (t_gold+gold) x | x <- list]
-- if filter (<=0) lst /= [] then head lst else
makeList :: Dungeon -> [(Double,Dungeon)]
makeList EmptyTree = []
makeList (Leaf name encounters) = [(findScore 0 0 (Leaf name encounters), Leaf name encounters)]
makeList (Node name encounters []) = []
makeList (Node name encounters list) = [(findScore 0 0 (Node name encounters list), Node name encounters list)] ++ concat [makeList item1 | item1 <- list]


mostEfficientSubtree :: Dungeon -> Dungeon
mostEfficientSubtree EmptyTree = EmptyTree
mostEfficientSubtree (Leaf name encounters) = (Leaf name encounters)
mostEfficientSubtree (Node name encounters list) = dungeon where index = elemIndex (maximum scorelist) scorelist 0 ; scorelist = [fst x | x <- makeList (Node name encounters list)] ; dungeon = dungeonlist !! index ; dungeonlist = [snd x | x <- makeList (Node name encounters list)]
