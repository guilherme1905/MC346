import Data.Char
import System.IO
import Data.List
import Data.List.Split
import Data.Char

getFirstTuple (x,_) = x
getSecondTuple (_,x) = x


splitList (x:xs) list
  | x /= [] = splitList xs (list ++ [x])
  | x == [] = (list, xs)


main :: IO()
main = do
    contents <- getContents
    let
        input = map words (lines contents)
        tuple = splitList input []
        list1 = getFirstTuple tuple
        aux = getSecondTuple tuple
        aux2 = splitList aux []
        list2 = getFirstTuple aux2
        list3 = getSecondTuple aux2
    print (list1)
    print (list2)
    print (list3)
