-- Лабораторна робота №4
-- студентки групи КН-32
-- Козелько Софії
-- Варіант №14
-- Типи i класи типiв

-- Мета роботи
-- Ознайомитись з системою типiв та класiв типiв. Набути досвiду визначення
-- нових типiв та класiв типiв i їх використання.

-- Записник. У записнику зберiгається iнформацiя про знайомих: телефон
-- (iм’я, телефон — один, або кiлька), нагадування про день народження (iм’я, дата
-- — день та мiсяць), зустрiчi (дата та час, мiсце, опис — тема зустрiчi, зауваження
-- про зустрiч — чи вiдбулась та iнше). Визначне функцiї для :

-- 14. зустрiчi на певний день або перiод;

type Name = String
type Phone  = String
type Notes  = String
type DayMonthYear = (Int,Int,Int) 
data Notebook = BirthdayCalendar Name DayMonthYear |  
               Phones  Name Phone |
               Meeting   Notes  DayMonthYear
               deriving Show


findMeetings [] _ _ = []
findMeetings ( Meeting [] _ :xs) c k = findMeetings xs c k
findMeetings ( Meeting a all@(n,_,_) :xs) c k
            | n>c && n<k = a:findMeetings xs c k
            | otherwise = findMeetings xs c k
findMeetings (_:xs) c k = findMeetings xs c k




notebook = [
    Phones "Hleb" "066 623 42 34",
    Meeting "Discussion" (28, 1, 1991),
    BirthdayCalendar "Arseniy" (20, 2, 1998),
    Phones "Maxim" "097 311 34 34",
    BirthdayCalendar "Inga" (11, 8, 1996), 
    BirthdayCalendar "Aleksandra" (14, 2, 2018),
    Meeting "StandUp" (12, 9, 2005),
    BirthdayCalendar "Nataliya" (23, 1, 1981),
    Phones "Olga" "050 000 15 31",
    Phones "Zinayida" "066 322 22 81",
    BirthdayCalendar "Vyacheslav" (8, 3, 1946),
    Phones "Ignat" "093 426 64 46",
    Phones "Ihor" "067 523 41 80",
    BirthdayCalendar "Volodymyr" (7, 7, 1977),
    Meeting "Serious talk" (21, 8, 2009)
           ]

--Тести
-- ghci> searchNameOn notebook 12 28
-- ["Serious talk"]
-- ghci> searchNameOn notebook 12 30
-- ["Discussion","Serious talk"]
-- ghci> searchNameOn notebook 2 15 
-- ["StandUp"]
-- ghci> searchNameOn notebook 2 30
-- ["Discussion","StandUp","Serious talk"]
-- ghci>

-- Висновок
-- В результаті виконання лабораторної роботи, я навчилась створювати свої типи даних, 
-- працювати з ними та аналізувати їх у функції.
