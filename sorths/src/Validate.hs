module Validate where
import Sorting
import Test.QuickCheck

p1b :: (Ord a) => [a] -> Bool
p1b xs = (bubbleSort $ bubbleSort xs) == bubbleSort xs

p2b :: (Ord a) => [a] -> Bool
p2b xs = (length $ bubbleSort xs) == length xs

p3b :: (Ord a) => [a] -> Property
p3b xs = not (null xs)
        ==> (head $ bubbleSort xs) == minimum xs

p1i :: (Ord a) => [a] -> Bool
p1i xs = (insertionSort $ insertionSort xs) == insertionSort xs

p2i :: (Ord a) => [a] -> Bool
p2i xs = (length $ insertionSort xs) == length xs

p3i :: (Ord a) => [a] -> Property
p3i xs = not (null xs)
        ==> (head $ insertionSort xs) == minimum xs

p1s :: (Ord a) => [a] -> Bool
p1s xs = (selectionSort $ selectionSort xs) == selectionSort xs

p2s :: (Ord a) => [a] -> Bool
p2s xs = (length $ selectionSort xs) == length xs

p3s :: (Ord a) => [a] -> Property
p3s xs = not (null xs)
        ==> (head $ selectionSort xs) == minimum xs

p1m :: (Ord a) => [a] -> Bool
p1m xs = (mergeSort $ mergeSort xs) == mergeSort xs

p2m :: (Ord a) => [a] -> Bool
p2m xs = (length $ mergeSort xs) == length xs

p3m :: (Ord a) => [a] -> Property
p3m xs = not (null xs)
        ==> (head $ mergeSort xs) == minimum xs

p1q :: (Ord a) => [a] -> Bool
p1q xs = (quickSort $ quickSort xs) == quickSort xs

p2q :: (Ord a) => [a] -> Bool
p2q xs = (length $ quickSort xs) == length xs

p3q :: (Ord a) => [a] -> Property
p3q xs = not (null xs)
        ==> (head $ quickSort xs) == minimum xs

p1c :: [Int] -> Bool
p1c xs = (countingSort $ countingSort xs) == countingSort xs

p2c :: [Int] -> Bool
p2c xs = (length $ countingSort xs) == length xs

p3c :: [Int] -> Property
p3c xs = not (null xs)
        ==> (head $ countingSort xs) == minimum xs

p1h :: (Ord a) => [a] -> Bool
p1h xs = (heapSort $ heapSort xs) == heapSort xs

p2h :: (Ord a) => [a] -> Bool
p2h xs = (length $ heapSort xs) == length xs

p3h :: (Ord a) => [a] -> Property
p3h xs = not (null xs)
        ==> (head $ heapSort xs) == minimum xs

p1bk :: [Int] -> Bool
p1bk xs = (bucketSort $ bucketSort xs) == bucketSort xs

p2bk :: [Int] -> Bool
p2bk xs = (length $ bucketSort xs) == length xs

p3bk :: [Int] -> Property
p3bk xs = not (null xs)
        ==> (head $ bucketSort xs) == minimum xs

p1r :: [Int] -> Bool
p1r xs = (radixSort $ radixSort xs) == radixSort xs

p2r :: [Int] -> Bool
p2r xs = (length $ radixSort xs) == length xs

p3r :: [Int] -> Property
p3r xs = not (null xs)
        ==> (head $ radixSort xs) == minimum xs
