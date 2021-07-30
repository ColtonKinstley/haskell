import           Lib

main :: IO ()
main = putStrLn "Test suite not yet implemented"

l = [1, 2, 3, 4]

s :: IO ()
s = putStrLn "Testing is_elem..."

test_is_elem :: IO ()
test_is_elem = if is_elem 1 l then putStrLn pass else putStrLn fail
 where
  pass = "Passed test"
  fail = "Failed test"

t = test_is_elem
