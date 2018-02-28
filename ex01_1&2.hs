{-
Задача 1. Да се напише функция mymin, която приема два аргумента и връща по-малкият от тях.
-}
mymin :: Int -> Int -> Int
mymin a b = if a < b then a else b


{-
Задача 2. Да се напише функция mymax, която приема два аргумента и връща по-големият от тях.
-}
mymax :: Int -> Int -> Int
mymax a b = if a > b then a else b


{-
Задача 3. Да се напише функция myfunc, която пресмята на средно аритметичното от квадратите на 2 числа.
-}
myfunc :: Double -> Double -> Double
myfunc a b = (a^2 + b^2) / 2


{-
Задача 4. Да се напише myfib, която получава един аргумент n и връща n-тото число на Фибоначи.
(Заб.: редицата е 1, 1, 2, 3, 5, ... и е индексирана от 0.)

Да се напише и итеративно решение.
-}
myfib :: Integer -> Integer
myfib n =
    if n == 0 then 1
    else if n == 1 then 1
    else myfib (n-1) + myfib (n-2)


myfibIterative :: Integer -> Integer
myfibIterative n = undefined


{-
Задача 5. Да се напише функция mygcd a b, която връща НОД(a, b).
-}
mygcd :: Int -> Int -> Int
mygcd a b =
    if b == 0 then a
    else mygcd b (a `mod` b)


{-
Задача 6. Да се напише функция mymaxdivisor x, която намира най-големия делител d на цялото число x > 1,
за който d < x.
-}
mymaxdivisor :: Int -> Int
mymaxdivisor x = go (x-1) x
    where
      go n x
       | x `mod` n == 0 = n
       | otherwise = go (n-1) (x)
{-
Задача 7. Да се дефинира функцията isInside x a b, която проверява дали числото x се намира
в затворения интервал [a .. b].
-}
isInside :: Integer -> Integer -> Integer -> Bool
isInside x a b = (x >= a && x <= b)


{-
Задача 8. Да се дефинира функцията isLeapYear year, която проверява дали годината year
е високосна.
-}
isLeapYear :: Integer -> Bool
isLeapYear year
 | isDivBy 400 = True
 | isDivBy 100 = False
 | isDivBy 4   = True
 | otherwise   = False
 where isDivBy a = mod year a == 0


{-
Задача 9. Да се дефинира функцията isValidDate day month year, която връща дали датата
(day, month, year) e валидна.
-}
isValidDate :: Integer -> Integer -> Integer -> Bool
isValidDate day month year =
    if month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12 then
        if day >= 1 && day <= 31 then True
        else False
    else if month == 2 then
        if day >= 1 && day <= 29 && isLeapYear year then True
        else if day >= 1 && day <= 28 &&  isLeapYear year == False then True
        else False
    else
        if day >= 1 && day <= 30 then True
        else False


main :: IO()
main = do
    print (mymin 1 2)
    print (mymax 2 1)
    print (myfunc 2 4)
    print (mygcd 12 18)
    print (myfib 4)
    print (mymaxdivisor 64)
    print (isInside 6 1 5)
    print (isInside 2 1 5)
    print (isLeapYear 2008)
    print (isValidDate 29 2 2004)
