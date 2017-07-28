import Data.List (sort)
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

myTree :: BinaryTree String
myTree = Node Leaf "hey" (Node Leaf "woah" Leaf) 

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf 
insert' b (Node left a right) -- pattern matches on our existing tree so we can move left and right around
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left x right) =
  f x ((foldTree f (foldTree f acc right) left))

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) =
  [a] ++ (preorder left) ++ (preorder right)

inorder :: Ord a => BinaryTree a -> [a]
inorder = sort . preorder

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) =
  (postorder left) ++ (postorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2,1,3]
  then putStrLn "Preorder fine!"
  else putStrLn "bad news bears!"

testInorder :: IO ()
testInorder =
  if inorder testTree == [1,2,3]
  then putStrLn "inorder fine!"
  else putStrLn "bad news bears!"

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1,3,2]
  then putStrLn "postorder fine!"
  else putStrLn "bad news bears!"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

