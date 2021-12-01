-- Лабораторна робота №1
-- студентки групи КН-32
-- Козелько Софії
-- Варіант №14
-- Мова Haskell. Робота з iнтерпретатором ghci

-- Мета роботи
-- Ознайомитись з основними типами мови. Ознайомитись зi структурою та
-- функцiями Glasgow Haskell Compiller. Набути навичок роботи з iнтерпретатором
-- ghci та визначення найпростiших функцiй.

-- Завдання
-- 1. Наведiть приклади виразiв вказаного типу. Кожен список має мiстити
-- кiлька елементiв. Перегляньте тип прикладiв, як їх визначає ghci. Про-
-- коментуйте.
-- 1.14 [([Double],(Bool,Char),Integer)]

a = [([1.5], (True, 'b'), 5)]

-- 2.14 Функцiя за довжиною чотирьох вiдрiзкiв визначає, чи можна на них
-- побудувати прямокутник.
-- а) як один кортеж,
-- else if (a1=a2) and (a3=a4) or (a1=a3) or (a2=a4) then write('2')

checkIfRectangle :: (Float, Float, Float, Float) -> Bool 

checkIfRectangle (a1,a2,a3,a4) | (a1==a2) && (a3==a4) = True 
                               | (a1==a3) && (a2==a4) = True 
                               | otherwise = False

--Тести
-- ghci> checkIfRectangle (2,3,4,5)
-- False 
-- ghci> checkIfRectangle (2,2,4,4)
-- True  
-- ghci> checkIfRectangle (2,2,3,4)
-- False
-- ghci> checkIfRectangle (3,3,3,3)
-- True
-- ghci>

-- б) без використання кортежiв чи спискiв

checkIfRectangle2 :: Float -> Float -> Float -> Float -> Bool 

checkIfRectangle2 a1 a2 a3 a4 | (a1==a2) && (a3==a4) = True 
                               | (a1==a3) && (a2==a4) = True 
                               | otherwise = False


--Тести
-- ghci> checkIfRectangle2 3 3 3 3
-- True
-- ghci> checkIfRectangle2 3 3 3 4
-- False
-- ghci> checkIfRectangle2 3 5 3 4
-- False
-- ghci> checkIfRectangle2 5 5 4 4
-- True
-- ghci>

-- Висновок
-- В результаті виконання лабораторної роботи, ми ознайомитись з основними типами даних мови Haskell.
-- Ознайомилися зi структурою та функцiями Glasgow Haskell Compiller. Набули навичок роботи з iнтерпретатором
-- ghci та визначили найпростiші функцiї.