import Control.Applicative

-- 1
one = const <$> Just "Hello" <*> Just "world"
two = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1,2,3]

