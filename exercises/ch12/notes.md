Maybe a = Just a | Nothing

helps us in situations where we aren't sure if a function will return a useful
value

Either a b = Left a | Right b

Either lets us have more control over our error handling than maybe. It appears
that anything maybe can do - either can do as well

we can pattern match on Left and Right 
```
myFunc :: Either a b -> Bool
myFunc (Left _) = False
myFunc (Right _) = True
```
