    eager:
minimum [2, 7, 1, 9, 6, 5]
head(insertionSort [2,7,1,9,6,5])
head(insert 2 (insertionsort[7,1,9,6,5]))
head(insert 2 (insert 7 (insertionsort[1,9,6,5])))
head(insert 2 (insert 7 (insert 1 (insertionsort[9,6,5]))))
head(insert 2 (insert 7 (insert 1 (insert 9 (insertionsort[6,5])))))
head(insert 2 (insert 7 (insert 1 (insert 9 (insert 5 (insertionsort[]))))))
head(insert 2 (insert 7 (insert 1 (insert 9 (insert 5 [])))))
head(insert 2 (insert 7 (insert 1 (insert 9 [5]))))
head(insert 2 (insert 7 (insert 1 ([5]: insert 9: []))))
head(insert 2 (insert 7 (insert 1 [5:9])))
head(insert 2 (insert 7 [1:5:9]))
head(insert 2 ([1:insert 7 :5:9]))
head(insert 2 ([1:5:insert 7:9]))
head(insert 2 [1:5:7:9])
head([1:insert 2:5:7:9])
head([1:2:5:7:9])
1

    lazy:
minimum [2, 7, 1, 9, 6, 5]
head(insertionSort [2,7,1,9,6,5])
head(insert 2 (insertionsort[7,1,9,6,5]))
head(insert 2 (insert 7 (insertionsort[1,9,6,5])))
head(insert 2 (insert 7 (insert 1 (insertionsort[9,6,5]))))
head(insert 2 (insert 7 (insert 1 (insert 9 (insertionsort[6,5])))))
head(insert 2 (insert 7 (insert 1 (insert 9 (insert 5 (insertionsort[]))))))
head(insert 2 (insert 7 (insert 1 (insert 9 (insert 5 [])))))
head(insert 2 (insert 7 (insert 1 (insert 9 [5]))))
head(insert 2 (insert 7 (insert 1 [5]:insert 9 [])))
head(insert 2 (insert 7 [1:5]:insert 9 []))
head(insert 2 [1:5]:insert 7: insert 9 [])
head([1:insert 2:5]:insert 7: insert 9 [])
1
