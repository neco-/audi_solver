import Char
import List

type MagicNumber = Int
type Position = (Int, Int)
type Cube = (Int, Int, Int)
type Square = [MagicNumber]
type SplitSquare = [[MagicNumber]]

squareSize :: Int
squareSize = 3

squareNum :: Int
squareNum = squareSize ^ 2

vacant :: Int
vacant = 0

constantSum :: Int
constantSum = squareSize * (squareSize ^ 2 + 1) `div` 2

center :: Int
center = squareSize `div` 2

magicNumbers :: [MagicNumber]
magicNumbers = [1..squareNum]

cubeTable :: [Cube]
cubeTable= ["enter your answer"]

magicSquare :: Square
magicSquare = [2, 0, 4,
               0, 0, 0,
               6, 0, 8]

candidateMagicNumbers :: [MagicNumber]
candidateMagicNumbers = magicNumbers \\ magicSquare

main = putStrLn $ show $ solveCube result
         where
           result = concat $ solveMagicSquare $ splitSquare 3 $ magicSquare

cubeToChar :: Cube -> Char
cubeToChar (x, y, z) = chr $ ord 'a' + x*3*3 + y*3 + z - 1

splitSquare :: Int -> Square -> SplitSquare
splitSquare = unfoldr . f
                where
                  f n xs = if null xs
                             then Nothing
                             else Just $ splitAt n xs

solveCube result = map (cubeToChar . snd) $ sortBy cmp $ zip result cubeTable
       where
         cmp (i, _) (j, _) = i `compare` j

solveMagicSquare s
  = case vacantPositions s of
      []  -> s
      ps -> case nextVacant s ps of
               (p, xs) -> concatMap solveMagicSquare
                            $ map (putCell s)
                            $ [(p, x) | x <- xs]

putCell s ((i, j), x) = ls0 ++ (xs0 ++ x:xs1):ls1
                        where
                          (ls0, l:ls1) = splitAt i s
                          (xs0, _:xs1) = splitAt j l

nextVacant s ps = minimumBy cmp [(p, candidates p s) | p <- ps]
                  where
                    (_,xs) `cmp` (_,ys) = length xs `compare` length ys

vacantPositions :: SplitSquare -> [Position]
vacantPositions s = map fst
                    $ filter (\(pos, x) -> x == vacant)
                    $ concatMap (\(i, xs) -> zipWith (\j x -> ((i, j), x)) [0..] xs)
                    $ zip [0..] s

candidates :: Position -> SplitSquare -> [MagicNumber]
candidates p s = minimumBy cmp $ map (\ c -> c p s) [col,row,dia]
                 where
                   cmp xs ys = length xs `compare` length ys

candidate :: Int -> SplitSquare -> [MagicNumber] -> [MagicNumber]
candidate n s c = if elem vacant $ c \\ [(c !! n)]
                 then
                   [x | x <- candidateMagicNumbers, x + sum c <= constantSum]
                 else
                   [constantSum - sum c] \\ magicSquare

row, col, dia :: Position  -> SplitSquare -> [MagicNumber]
row (i, j) s = candidate j s (s !! i)
col (i, j) s = candidate i s (transpose s !! j)
dia (i, j) s = if (i, j) == (center, center)
                   then
                     nub (ca ++ cb)
                   else
                     candidateMagicNumbers
                 where
                   ca = candidate center s [a, c, e]
                   cb = candidate center s [b, c, d]
                   a = (s !! (center-1)) !! (center-1)
                   b = (s !! (center+1)) !! (center-1)
                   c = (s !! (center+0)) !! (center+0)
                   d = (s !! (center-1)) !! (center+1)
                   e = (s !! (center+1)) !! (center+1)

