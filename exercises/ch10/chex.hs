stops = "pbtdkg"
vowels = "aeiou"

f xs ys = [(x,y,z) | x <- xs, y <- ys, z <- xs]
f2 xs ys = [(x,y,z) | x <- xs, y <- ys, z <- xs, x == 'p']

-- seekritFunc returns the average length of a list of words
