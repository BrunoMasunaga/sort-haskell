module Main where
import Sorting

import Data.List
import Control.Exception
import Data.Time (getCurrentTime)
import Data.Time (diffUTCTime)

array = [2,4,13,35,6,54,213,16,7,8,0,0,1]

main = do
    -- Bubble Sort --
    start1 <- getCurrentTime
    evaluate $ bubbleSort array
    end1 <- getCurrentTime
    let dif1 = diffUTCTime end1 start1
    print ("Bubble Sort:     " ++ show dif1)

    -- Insertion Sort --
    start2 <- getCurrentTime
    evaluate $ insertionSort array
    end2 <- getCurrentTime
    let dif2 = diffUTCTime end2 start2
    print ("Insertion Sort:  " ++ show dif2)

    -- Selection Sort --
    start3 <- getCurrentTime
    evaluate $ selectionSort array
    end3 <- getCurrentTime
    let dif3 = diffUTCTime end3 start3
    print ("Selection Sort:  " ++ show dif3)

    -- Merge Sort --
    start4 <- getCurrentTime
    evaluate $ mergeSort array
    end4 <- getCurrentTime
    let dif4 = diffUTCTime end4 start4
    print ("Merge Sort:      " ++ show dif4)

    -- Quick Sort --
    start5 <- getCurrentTime
    evaluate $ quickSort array
    end5 <- getCurrentTime
    let dif5 = diffUTCTime end5 start5
    print ("Quick Sort:      " ++ show dif5)

    -- Counting Sort --
    start6 <- getCurrentTime
    evaluate $ countingSort array
    end6 <- getCurrentTime
    let dif6 = diffUTCTime end6 start6
    print ("Counting Sort:   " ++ show dif6)

    -- Max-Heap Sort --
    start7 <- getCurrentTime
    evaluate $ heapSort array
    end7 <- getCurrentTime
    let dif7 = diffUTCTime end7 start7
    print ("Heap Sort:       " ++ show dif7)

    -- Bucket Sort (10 buckets) --
    start8 <- getCurrentTime
    evaluate $ bucketSort array
    end8 <- getCurrentTime
    let dif8 = diffUTCTime end8 start8
    print ("Bucket Sort:     " ++ show dif8)

    -- Radix Sort (LSB) --
    start9 <- getCurrentTime
    evaluate $ radixSort array
    end9 <- getCurrentTime
    let dif9 = diffUTCTime end9 start9
    print ("Radix Sort:      " ++ show dif9)

    -- Default Haskell Sorting --
    start0 <- getCurrentTime
    evaluate $ sort array
    end0 <- getCurrentTime
    let dif0 = diffUTCTime end0 start0
    print ("Default Haskell: " ++ show dif0)
