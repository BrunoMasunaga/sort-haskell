module Sorting where

---------------------------------- Bubble Sort ---------------------------------
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
-------------------------------- Insertion Sort --------------------------------
inserir :: (Ord a) => a -> [a] -> [a]
inserir x []     = [x]
inserir x (y:ys) = if x <= y
                   then x:y:ys
                   else y:(inserir x ys)

insertionSort :: (Ord a) => [a] -> [a]
insertionSort (x1:x2:xs) = inserir x1 $ insertionSort $ x2:xs
insertionSort xs         = xs
-------------------------------- Selection Sort --------------------------------
remover :: (Ord a) => a -> [a] -> [a]
remover x []     = []
remover x (y:ys) = if x == y
                   then ys
                   else y:(remover x ys)

selectionSort :: (Ord a) => [a] -> [a]
selectionSort (x1:x2:xs) = minimo:(selectionSort $ remover minimo (x1:x2:xs)) 
                              where minimo = minimum $ x1:x2:xs
selectionSort xs         = xs
---------------------------------- Merge Sort ----------------------------------
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
---------------------------------- Quick Sort ----------------------------------
dividirPivot :: (Ord a) => [a] -> ([a],[a])
dividirPivot (x:xs)  = (filter (< x) xs, filter (>= x) xs)

quickSort :: (Ord a) => [a] -> [a]
quickSort (x1:x2:xs) = quickSort pivotEsq ++ [x1] ++ quickSort pivotDir
                          where (pivotEsq, pivotDir) = dividirPivot $ x1:x2:xs
quickSort xs         = xs
--------------------------------- Counting Sort --------------------------------
tamanhoAuxiliar :: [Int] -> Int
tamanhoAuxiliar xs = (maximum xs)-(minimum xs)

contarOcorrencias :: Int -> [Int] -> Int
contarOcorrencias x xs = length $ filter (== x) xs

construirAuxiliar :: [Int] -> [Int]
construirAuxiliar xs = [contarOcorrencias (x+min) xs | x <- [0..tAux]] 
                          where min = minimum xs
                                tAux = tamanhoAuxiliar xs

adicionar :: Int -> Int -> [Int] -> [Int]
adicionar _ 0 xs = xs
adicionar x i xs = adicionar x (i-1) (xs ++ [x])  

countingSort :: [Int] -> [Int]
countingSort []  = []
countingSort [x] = [x]
countingSort xs  = concat [adicionar (y+min) (auxiliar!!y) [] | y <- [0..tAux]]
                      where auxiliar = construirAuxiliar xs
                            min      = minimum xs
                            tAux     = tamanhoAuxiliar xs
--------------------------------- Max-Heap Sort --------------------------------
trocar :: (Ord a) => Int -> Int -> [a] -> [a]
trocar i j xs = if i == j
                then xs
                else esquerda ++ [xs!!y] ++ meio ++ [xs!!x] ++ direita
                   where esquerda = take x xs
                         meio     = take (y - x - 1) $ drop (x + 1) xs
                         direita  = drop (y + 1) xs
                         (x,y)    = if i <= j
                                    then (i,j)
                                    else (j,i)

encontrarMaximo :: (Ord a) => Int -> Int -> [a] -> Int
encontrarMaximo ipai tam xs = 
        let imaior = if (iesq < tam) && ((xs!!iesq) > (xs!!ipai)) 
                     then iesq 
                     else ipai
        in if (idir < tam) && ((xs!!idir) > (xs!!imaior)) 
           then idir 
           else imaior
              where iesq = 2 * ipai + 1
                    idir = 2 * ipai + 2

maxHeapify :: (Ord a) => Int -> Int -> [a] -> [a]
maxHeapify ipai tam xs = if maior /= ipai 
                         then maxHeapify maior tam $ trocar maior ipai xs
                         else xs
                            where maior = encontrarMaximo ipai tam xs

construirHeap :: (Ord a) => Int -> [a] -> [a]
construirHeap 0 xs = maxHeapify 0 (length xs) xs
construirHeap tam xs = construirHeap (tam - 1) $ maxHeapify tam (length xs) xs

chamadaHeap :: (Ord a) => Int -> [a] -> [a]
chamadaHeap tam xs = if tam /= 1 
                     then chamadaHeap (tam - 1) $ maxHeapify 0 tam trocado
                     else maxHeapify 0 tam trocado
                         where trocado = trocar 0 tam xs
                         
heapSort :: (Ord a) => [a] -> [a]
heapSort []  = []
heapSort [x] = [x]
heapSort xs  = chamadaHeap (length xs - 1) heap
                  where heap = construirHeap (length xs `div` 2) xs
---------------------- Bucket Sort (Utilizando 10 buckets) ---------------------
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
ordenarBuckets (xs:xss) = (insertionSort xs):(ordenarBuckets xss)

bucketSort :: [Int] -> [Int]
bucketSort (x1:x2:xs) = concat $ ordenarBuckets $ criarBuckets (x1:x2:xs)
bucketSort xs         = xs
------------------------------- Radix Sort (LSB) -------------------------------
maiorElemento :: [Int] -> Int
maiorElemento xs = maximum xs

numDigitos :: Int -> Int -> Int
numDigitos 0 contador = contador
numDigitos x contador = numDigitos (x `div` 10) (contador + 1)

obterDigito :: Int -> Int -> Int
obterDigito 0 _      = 0
obterDigito x 1      = x `mod` 10
obterDigito x digito = obterDigito (x `div` 10) (digito - 1)

obterDigitos :: [Int] -> Int -> Int -> [(Int,Int)]
obterDigitos [] _ _               = []
obterDigitos (x:xs) digito i = (dig, i):(obterDigitos xs digito (i+1)) 
                                   where dig = obterDigito x digito
                                   
ordenarDigitos :: [(Int,Int)] -> [Int]
ordenarDigitos ts = [snd t | t <- (selectionSort ts)]

recuperarNumeros :: [Int] -> [Int] -> [Int] 
recuperarNumeros xs is = [xs!!i | i <- is]

chamadaRadix :: [Int] -> Int -> [Int]
chamadaRadix xs digito = recuperarNumeros xs is 
                            where is = ordenarDigitos $ obterDigitos xs digito 0

radix :: [Int] -> Int -> Int -> [Int]
radix xs digito num = if digito > num
                      then xs
                      else radix (chamadaRadix xs digito) (digito + 1) num

radixSort :: [Int] -> [Int]
radixSort []  = []
radixSort [x] = [x]
radixSort xs  = radix xs 1 $ numDigitos (maiorElemento xs) 0
