{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
-- Лабораторна робота №3
-- студентки групи КН-32
-- Козелько Софії
-- Варіант №14
-- Функцiї вищого порядку

-- Мета роботи
-- Набути досвiду визначення та використання функцiй вищого порядку.

-- Завдання
-- Визначте вказанi функцiї в кожному з завдань: а) без застосування, б) з
-- застосуванням вбудованих функцiй вищого порядку.

-- 1.14 Знайти передостаннiй елемент списку.

myButLast [x, _] = x  -- base case
myButLast (_ : xs) = myButLast xs
myButLast [x] = error "List is a singleton"
myButLast [] = error "oho"

--Тести
-- ghci> myButLast "avav"
-- 'a'
-- ghci> myButLast "123456"
-- '5'
-- ghci> myButLast ""      
-- *** Exception: oho
-- CallStack (from HasCallStack):
--   error, called at lr3/lr3.hs:18:20 in main:Main
-- ghci>


myButLast2 [] = error "List is empty"
myButLast2 [x] = error "List is a singleton"
myButLast2 xs = last $ init xs


--Тести
-- ghci> myButLast2 "aaaaabbb"
-- 'b'
-- ghci> myButLast2 "1234567" 
-- '6'
-- ghci> myButLast2 "adadadada"
-- 'd'
-- ghci>


-- 2.14 Циклiчний лiвий зсув списку на n позицiй.

-- а)
shiftList n [] = []
shiftList 0 x  = x
shiftList n (x:xs) = shiftList (n - 1) xs++[x]

--Тести
-- ghci> shiftList 2 [1..10]
-- [3,4,5,6,7,8,9,10,2,1]
-- ghci> shiftList 2 [1..25]
-- [3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,2,1]
-- ghci>

-- б)
cyclicShiftR :: Int -> [a] -> [a]
cyclicShiftR n xs = let (a, b) = splitAt (length xs - n) xs
                    in b ++ a

--Тести
-- ghci> cyclicShiftR 3 "12345678910"
-- "91012345678"
-- ghci> cyclicShiftR 3 [1,2,3,5,6,7]
-- [5,6,7,1,2,3]
-- ghci> cyclicShiftR 3 [1..10]      
-- [8,9,10,1,2,3,4,5,6,7]
-- ghci> 

-- Висновок
-- В результаті виконання лабораторної роботи, ми навчилися використовувати функції вищих порядків в мови Haskell.
-- В своїх завданнях ми застосували функції, що вбудовані в мову та викликали інші, для реалізації
-- поставлених завдань.

