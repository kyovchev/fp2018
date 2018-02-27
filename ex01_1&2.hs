{-
Задача 1. Да се напише функция mymin, която приема два аргумента и връща по-малкият от тях.
-}
mymin :: Int -> Int -> Int
mymin a b = if a > b then b else a

{-
Задача 2. Да се напише функция mymax, която приема два аргумента и връща по-големият от тях.
-}

mymax :: Int -> Int -> Int
mymax a b 
  | a > b     = a
  | otherwise = b

{-
Задача 3. Да се напише функция myfunc, която пресмята на средно аритметичното от квадратите на 2 числа.
-}

myfunc :: Double -> Double -> Double
myfunc a b = getAvg (a^2) (b^2)

getAvg :: Double -> Double -> Double
getAvg a b = (a+b)/2

{-
Задача 4. Да се напише myfib, която получава един аргумент n и връща n-тото число на Фибоначи.
(Заб.: редицата е 1, 1, 2, 3, 5, ... и е индексирана от 0.)

Да се напише и итеративно решение.
-}

myfib :: Integer -> Integer
myfib 0 = 0
myfib 1 = 1
mybib n = myfib (n-1) + myfib (n-2)

myfibIterative :: Integer -> Integer
myfibIterative n = myfibIterativeHelper n 1 1

myfibIterativeHelper :: Integer -> Integer -> Integer -> Integer
myfibIterativeHelper n count product
    | count > n = product
    | otherwise = myfibIterativeHelper n (count+1) (count-1)


{-
Задача 5. Да се напише функция mygcd a b, която връща НОД(a, b).
-}

mygcd :: Int -> Int -> Int
mygcd a b = if b == 0 then a else mygcd b (a `mod` b)


{-
Задача 6. Да се напише функция mymaxdivisor x, която намира най-големия делител d на цялото число x > 1,
за който d < x.
-}
mymaxdivisor :: Int -> Int
mymaxdivisor x = undefined


{-
Задача 7. Да се дефинира функцията isInside x a b, която проверява дали числото x се намира
в затворения интервал [a .. b].
-}
isInside :: Integer -> Integer -> Integer -> Bool
isInside x a b = undefined


{-
Задача 8. Да се дефинира функцията isLeapYear year, която проверява дали годината year
е високосна.
-}
isLeapYear :: Integer -> Bool
isLeapYear year = undefined


{-
Задача 9. Да се дефинира функцията isValidDate day month year, която връща дали датата
(day, month, year) e валидна.
-}
isValidDate :: Integer -> Integer -> Integer -> Bool
isValidDate day month year = undefined


main :: IO()
main = do
    print (fib 0)
    print (fib 1)
