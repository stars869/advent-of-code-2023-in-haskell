import System.IO
import Data.Char (isDigit, digitToInt)


solveCase :: String -> Int
solveCase str = head nums * 10 + last nums
    where nums = map digitToInt $ filter isDigit str

main :: IO ()
main = do
    input <-  readFile "./input"
    let result = sum $ map solveCase $ lines input
    print result
    