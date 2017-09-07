double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

doubleSmallNumber x = if x > 100
                       then x
                       else x * 2
