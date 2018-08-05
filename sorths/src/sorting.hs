import Data.List

-- Bubble Sort
ordenado :: (Ord a) => [a] -> Bool
ordenado (x1:x2:xs) = if x1 <= x2
                      then ordenado $ x2:xs
                      else False
ordenado xs         = True

chamadaBubble :: (Ord a) => [a] -> [a]
chamadaBubble (x1:x2:xs) = if x1 <= x2
                           then x1:(bubbleSort $ x2:xs)
                           else x2:(bubbleSort $ x1:xs)
                      
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort (x1:x2:xs)  = if ordenado chamadaRec
                         then chamadaRec 
                         else bubbleSort chamadaRec
                            where chamadaRec = chamadaBubble $ x1:x2:xs
bubbleSort xs          = xs
                         
-- Insertion Sort
inserir :: (Ord a) => a -> [a] -> [a]
inserir x []     = [x]
inserir x (y:ys) = if x <= y
                   then x:y:ys
                   else y:(inserir x ys)

insertionSort :: (Ord a) => [a] -> [a]
insertionSort (x1:x2:xs) = inserir x1 $ insertionSort $ x2:xs
insertionSort xs         = xs

-- Selection Sort
remover :: (Ord a) => a -> [a] -> [a]
remover x []     = []
remover x (y:ys) = if x == y
                   then ys
                   else y:(remover x ys)

selectionSort :: (Ord a) => [a] -> [a]
selectionSort (x1:x2:xs) = minimo:(selectionSort $ remover minimo (x1:x2:xs)) 
                              where minimo = minimum $ x1:x2:xs
selectionSort xs         = xs
                              
-- Merge Sort
intercalar :: (Ord a) => [a] -> [a] -> [a]
intercalar [] []         = []
intercalar [] ys         = ys
intercalar xs []         = xs
intercalar (x:xs) (y:ys) = if x <= y
                           then x:(intercalar xs (y:ys))
                           else y:(intercalar (x:xs) ys)

dividir :: (Ord a) => [a] -> ([a],[a])
dividir []  = ([], [])
dividir [x] = ([x], [])
dividir xs  = (take metade xs, drop metade xs)
                 where metade = div (length xs) 2
                 
mergeSort :: Ord a => [a] -> [a]
mergeSort (x1:x2:xs) = intercalar (mergeSort esq) (mergeSort dir)
                          where (esq, dir) = dividir $ x1:x2:xs
mergeSort xs         = xs

-- Quick Sort
dividirPivot :: (Ord a) => [a] -> ([a],[a])
dividirPivot (x:xs)  = (filter (<= x) xs, filter (> x) xs)

quickSort :: (Ord a) => [a] -> [a]
quickSort (x1:x2:xs) = quickSort pivotEsq ++ [x1] ++ quickSort pivotDir
                          where (pivotEsq, pivotDir) = dividirPivot $ x1:x2:xs
quickSort xs         = xs

-- Counting Sort
tamanhoAuxiliar :: [Int] -> Int
tamanhoAuxiliar xs = (maximum xs)-(minimum xs)

contarOcorrencias :: Int -> [Int] -> Int
contarOcorrencias x xs = length $ filter (== x) xs

construirAuxiliar :: [Int] -> [Int]
construirAuxiliar xs = [contarOcorrencias (x+min) xs | x <- [0..tAux]] 
                          where min = minimum xs
                                tAux = tamanhoAuxiliar xs
                                
add :: Int -> Int -> [Int] -> [Int]
add _ 0 xs = xs
add x i xs = add x (i-1) (xs ++ [x])  

countingSort :: [Int] -> [Int]
countingSort (x1:x2:xs) = concat [add (y+min) (auxiliar!!y) [] | y <- [0..tAux]]
                             where auxiliar = construirAuxiliar $ x1:x2:xs
                                   min      = minimum $ x1:x2:xs
                                   tAux     = tamanhoAuxiliar $ x1:x2:xs
countingSort xs         = xs

-- Heap Sort
-- Bucket Sort (Utilizando 10 buckets)
divisor :: [Int] -> Int
divisor xs = (div ((maximum xs) + 1) 10) + 1

encontrarBucket :: Int -> [Int] -> Int
encontrarBucket x xs = div x (divisor xs)

criarBucket :: [Int] -> Int -> [Int]
criarBucket xs i = [x | x <- xs, (encontrarBucket x xs) == i]

criarBuckets :: [Int] -> [[Int]]
criarBuckets xs = [criarBucket xs i | i <- [0..9]]

ordenarBuckets :: [[Int]] -> [[Int]]
ordenarBuckets []       = []
ordenarBuckets [xs]     = [xs]
ordenarBuckets (xs:xss) = (insertionSort xs):(ordenarBuckets xss)

bucketSort :: [Int] -> [Int]
bucketSort (x1:x2:xs) = concat $ ordenarBuckets $ criarBuckets (x1:x2:xs)
bucketSort xs         = xs

-- Radix Sort
