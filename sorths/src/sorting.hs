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
chamadaBubble xs         = xs
                      
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort xs  = if ordenado chamadaRec
                 then chamadaRec 
                 else bubbleSort chamadaRec
                    where chamadaRec = chamadaBubble xs
                         
-- Insertion Sort
inserir :: (Ord a) => a -> [a] -> [a]
inserir x []     = [x]
inserir x (y:ys) = if x <= y
                   then x:y:ys
                   else y:(inserir x ys)

insertionSort :: (Ord a) => [a] -> [a]
insertionSort []     = []
insertionSort [x]    = [x]
insertionSort (x:xs) = inserir x $ insertionSort xs

-- Selection Sort
remover :: (Ord a) => a -> [a] -> [a]
remover x []     = []
remover x (y:ys) = if x == y
                   then ys
                   else y:(remover x ys)

selectionSort :: (Ord a) => [a] -> [a]
selectionSort []  = []
selectionSort [x] = [x]
selectionSort xs  = minimo:(selectionSort $ remover minimo xs) 
                       where minimo = minimum xs

-- Counting Sort
-- Merge Sort
-- Quick Sort
-- Heap Sort
-- Bucket Sort
-- Radix Sort
