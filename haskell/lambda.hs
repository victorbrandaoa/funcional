pow = \x y -> if y == 0 then 1
              else if y < 0 then x / (pow x ((abs y)+1))
              else x * (pow x (y-1))

fatorial = \x -> if x <= 1 then 1 else x * (fatorial (x - 1))

getDividers = \num divider -> if num == divider then [num]
                              else if mod num divider == 0 then [divider] ++ getDividers num (divider + 1)
                              else getDividers num (divider + 1)

isPrime = \x -> length (getDividers x 1) == 2

fib = \n -> if n == 0 || n == 1 then n else (fib (n - 1)) + (fib (n - 2))

mdc = \x y -> if y == 0 then x else mdc y (mod x y)

mmc = \x y -> div (x * y) (mdc x y)

coprimo = \x y -> (mdc x y) == 1
