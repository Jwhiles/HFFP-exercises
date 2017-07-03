mySqr = [x^2 | x <- [1..5]]
myCub = [x^3 | x <- [1..5]]

myTuples = [(x,y) | x <- mySqr, y <- myCub]
myLittleTuples = [(x,y) | x <- mySqr, y <- myCub, x < 50, y < 50]
myLittleTuplesLength = length myLittleTuples

