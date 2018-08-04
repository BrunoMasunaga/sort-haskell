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
                            where chamadaRec = chamadaBubble (x1:x2:xs)
bubbleSort xs          = xs
                         
-- Insertion Sort
inserir :: (Ord a) => a -> [a] -> [a]
inserir x []     = [x]
inserir x (y:ys) = if x <= y
                   then x:y:ys
                   else y:(inserir x ys)

insertionSort :: (Ord a) => [a] -> [a]
insertionSort (x1:x2:xs) = inserir x1 $ insertionSort (x2:xs)
insertionSort xs         = xs

-- Selection Sort
remover :: (Ord a) => a -> [a] -> [a]
remover x []     = []
remover x (y:ys) = if x == y
                   then ys
                   else y:(remover x ys)

selectionSort :: (Ord a) => [a] -> [a]
selectionSort (x1:x2:xs) = minimo:(selectionSort $ remover minimo (x1:x2:xs)) 
                              where minimo = minimum (x1:x2:xs)
selectionSort xs         = xs
                              
-- Counting Sort
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
                          where (esq, dir) = dividir (x1:x2:xs)
mergeSort xs         = xs

-- Quick Sort
dividirPivot :: (Ord a) => [a] -> ([a],[a])
dividirPivot (x:xs)  = (filter (<= x) xs, filter (> x) xs)

quickSort :: (Ord a) => [a] -> [a]
quickSort (x1:x2:xs) = quickSort pivotEsq ++ [x1] ++ quickSort pivotDir
                          where (pivotEsq, pivotDir) = dividirPivot (x1:x2:xs)
quickSort xs         = xs

-- Heap Sort
-- Bucket Sort
-- Radix Sort
