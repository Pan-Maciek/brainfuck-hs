import Data.List
import System.IO

data Instruction =
  Add Int | Move Int
  | Print | Read
  | Loop [Instruction] 
  | Program [Instruction]
  deriving (Show)

parse :: [Char] -> Instruction
parse code = program where 
  parser (acc, ('+':xs)) = parser (Add 1 : acc, xs)
  parser (acc, ('-':xs)) = parser (Add (-1) : acc, xs)
  parser (acc, ('>':xs)) = parser (Move 1 : acc, xs)
  parser (acc, ('<':xs)) = parser (Move (-1) : acc, xs)
  parser (acc, ('.':xs)) = parser (Print : acc, xs)
  parser (acc, (',':xs)) = parser (Read : acc, xs)
  parser (acc, ('[':xs)) = parser (Loop instructions : acc, tail)
    where (instructions, tail) = parser ([], xs)
  parser (acc, (']':xs)) = (reverse acc, xs)
  parser (acc, _:xs) = parser (acc, xs) -- skip comments
  parser (acc, []) = ([Program (reverse acc)], []) 

  (program: _, _) = parser ([], code)

optimize :: Instruction -> Instruction
optimize program = optimized where
  optimize (Add n: Add m: xs) = optimize (Add (m + n) : xs)
  optimize (Move n: Move m: xs) = optimize (Move (m + n) : xs)
  optimize ((Loop instructions): xs) = (Loop (optimize instructions)) : (optimize xs)
  optimize ((Program instructions): xs) = [Program (optimize instructions)]
  optimize (instr: xs) = instr : (optimize xs)
  optimize [] = []
  optimized:_ = optimize [program]

compile :: Instruction -> [Char]
compile (Add val) = "data[ptr] = (data[ptr] + " ++ show (val `mod` 256) ++ ") % 256;"
compile (Move val) = "ptr +=" ++ show val ++ ";"
compile Print = "print (data[ptr]);"
compile Read = "data[ptr] = await read ();"
compile (Loop instructions) = "while (data[ptr]) { " ++ intercalate "\n" (map compile instructions) ++ " }"
compile (Program instructions) = "async function program(print, read) { " ++ runtime ++ intercalate "\n" (map compile instructions) ++ " }" where
  runtime = "const data = Array.from({ length: 30000 }, () => 0); let ptr = 0;"