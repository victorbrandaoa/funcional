pow = \x y -> if y == 0 then 1
              else if y < 0 then x / (pow x ((abs y)+1))
              else x * (pow x (y-1))
