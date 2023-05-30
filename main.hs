import System.IO

-- Generates the next row of Pascal's triangle given the previous
newRow :: [Int] -> [Int]
newRow [] = []
newRow (x:[]) = [x]
newRow (x:xs) = (head xs + x):newRow xs

-- Pascal's triangle from row 1 to row n
triangle :: Int -> [[Int]]
triangle n 
    | n == 0    = []
    | n == 1    = [[1]]
    | otherwise = prev ++ [1:newRow (last prev)] -- a triangle of height n is a triangle of height n-1 + the next row
        where prev = triangle (n - 1) 

-- The number of digits in integer n
numDigits :: Int -> Int
numDigits n = (length . show) n -- equivalent to length(show n)

-- The number of characters required to display all numbers in a particular row of a triangle when each number takes up width spaces
rowLength :: [Int] -> Int -> Int
rowLength row width = (length row - 1) * (width + 1) + numDigits (last row)

{- 
  Prints a given row of Pascal's triangle left-justified
  - row is the row to be printed
  - width is the number of total spaces that should be used in printing each number
  - evenColor is the ANSI escape sequecne corresponding to the backgorund color of even numbers
  - oddColor is the ANSI escape sequecne corresponding to the backgorund color of even numbers
-}
printRowLJ :: [Int] -> Int -> String -> String -> IO ()
printRowLJ row width evenColor oddColor = putStr str
    where spaces x = replicate (width - numDigits x + 1) ' '
          tint x = if even x then evenColor else oddColor 
          str = concat [tint x ++ show x ++ spaces x | x <- row]

-- Prints a given row of Pascal's triangle normally
-- maxRowLen is the length of the longest row in the triangle
printRowNormal :: [Int] -> Int -> Int -> [Char] -> [Char] -> IO ()
printRowNormal row width maxRowLen evenColor oddColor = do putStr frontspaces
                                                           printRowLJ row width evenColor oddColor 
                                                        where numSpaces = maxRowLen - rowLength row width
                                                              numFrontspaces = numSpaces `div` 2 
                                                              frontspaces = replicate numFrontspaces ' '

-- Displays Pascal's triangle
printTriangle :: [[Int]] -> Int -> Int -> Bool -> [Char] -> [Char] -> IO ()                       
printTriangle []     _     _         _  _         _        = putStrLn ""
printTriangle (r:rs) width maxRowLen lj evenColor oddColor = do if lj 
                                                                  then printRowLJ r width evenColor oddColor 
                                                                  else printRowNormal r width maxRowLen evenColor oddColor
                                                                putStrLn default_
                                                                printTriangle rs width maxRowLen lj evenColor oddColor                           

default_ = "\ESC[0m"

-- Maps numbers to their corresponding ANSI escape sequence
escSec :: Char -> String
escSec c
    | c == '1' = "\ESC[104m" -- blue
    | c == '2' = "\ESC[101m" -- red
    | c == '3' = "\ESC[102m" -- green
    | c == '4' = "\ESC[103m" -- yellow 
    | c == '5' = "\ESC[105m" -- magenta
    | c == '6' = "\ESC[45m" -- purple
    | c == '7' = "\ESC[47m" -- white
    | c == '8' = "\ESC[106m" -- cyan
    | c == '9' = "\ESC[40m" -- black
    | otherwise = "\ESC[0m" 

-- Asks the user for the colors they would like to use 
printSierpinski :: [[Int]] -> Int -> Int -> Bool -> IO ()
printSierpinski t width maxRowLen lj = do putStr "\nBlue - 1, Red - 2, Green - 3, Yellow - 4, Light Purple - 5, Dark Purple - 6, White - 7, Cyan - 8, Black - 9\n"
                                          putStr "Choose an odd color: "
                                          odd <- getChar
                                          _ <- getChar
                                          putStr "Choose an even color: "
                                          even <- getChar
                                          _ <- getChar
                                          putStrLn ""
                                          printTriangle t width maxRowLen lj (escSec even) (escSec odd)

main = do 
  hSetBuffering stdout NoBuffering
  putStr "Show Pascal's triangle up to row _ (enter a natural number) : "
  -- generate triangle 
  h <- getLine 
  let height    = read h :: Int
  let triang    = triangle height
  let width     = numDigits (maximum (last triang)) -- the number of digits of the largest number in the triangle
  let maxRowLen = rowLength (last triang) width

  putStrLn "\n----------------------------------------- Configure Settings -----------------------------------------"
  putStr "Inverted (0/1) | Left-Justified (0/1) | Show Sierpinski's Triangle (0/1) (Enter a bit string): "
  settings <- getLine
  let inverted    = if settings !! 0 == '1' then True else False
  let lj          = if settings !! 1 == '1' then True else False
  let sierpinskis = if settings !! 2 == '1' then True else False
  putStrLn ""

  -- invert tringle if applicable
  let t = if inverted then reverse triang else triang

  -- get colors for sierpinski's triangle if applicable
  if sierpinskis 
    then printSierpinski t width maxRowLen lj 
    else printTriangle t width maxRowLen lj default_ default_

  -- ask user if they want to quit 
  putStr "\nQuit program? (y/n): "
  quit <- getChar
  _ <- getChar    

  if quit == 'y' then putStr "" else main 
                
          
          