a = fmap (+1) $ read "[1]" :: [Int]
b = (fmap . fmap) (++ "lol") (Just ["hi", "hello"])
c = fmap (*2) (\x -> x - 2)
-- d = ((return "1" ++) . show) (\x -> [x, 1..3])
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read (("123"++) fmap show ioi)
    in (*3) changed
