module QuickSortShuffle where
import Sorting

divide :: (Ord a) => [a] -> ([a],[a])
divide xs = (p2,p1)
               where p1 = [(xs!!i) | i <- [0,2..length xs-1]]
                     p2 = [(xs!!j) | j <- [1,3..length xs-1]]

revertePrimeiro :: (Ord a) => ([a],[a]) -> ([a],[a])
revertePrimeiro (xs,ys) = (reverse xs, ys)

juntar :: (Ord a) => ([a],[a]) -> [a]
juntar ([],ys)         = ys
juntar (xs,[])         = xs 
juntar ((x:xs),(y:ys)) = if even $ (length xs) + (length ys)
                         then [y]++[x]++(juntar (xs,ys))
                         else [x]++[y]++(juntar (ys,xs))

shuffle :: (Ord a) => [a] -> [a]
shuffle []  = []
shuffle [x] = [x]
shuffle xs  = juntar $ revertePrimeiro $ divide xs

quickSortShuffle :: (Ord a) => [a] -> [a]
quickSortShuffle (x1:x2:xs) = quickSort $ shuffle (x1:x2:xs)
quickSortShuffle xs         = xs
