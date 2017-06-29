mySqr = [x^2 | x <- [1..5]]
evenSqr = [x | x <- mySqr, rem x 2 == 0]
nothing = [(x,y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
notNothing = [(x,y) | x <- mySqr, y <- mySqr]


