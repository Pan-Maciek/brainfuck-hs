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
  optimize (Add n: Add m: xs) =  optimize (if m + n /= 0 then (Add (m + n) : xs) else xs)
  optimize (Move n: Move m: xs) = optimize (Move (m + n) : xs)
  optimize ((Loop instructions): xs) = (Loop (optimize instructions)) : (optimize xs)
  optimize ((Program instructions): xs) = [Program (optimize instructions)]
  optimize (instr: xs) = instr : (optimize xs)
  optimize [] = []
  optimized:_ = optimize [program]

compile :: Instruction -> [Char]
compile (Add val) | val > 0 = "data[ptr] += " ++ show val ++ ";"
compile (Add val) | val < 0 = "data[ptr] -= " ++ show (-val) ++ ";"
compile (Add 0) = ""
compile (Move val) = "ptr += " ++ show val ++ ";"
compile Print = "printf(\"%c\", data[ptr]);"
compile Read = "scanf(\"%c\", data + ptr);"
compile (Loop instructions) = "while (data[ptr]) { " ++ intercalate "\n" (map compile instructions) ++ " }"
compile (Program instructions) = runtime ++ "int main() { " ++ intercalate "\n" (map compile instructions) ++ " }" where
  memSize = 30000
  runtime = "#include <stdio.h>\n#define MEM_SIZE = " ++ show memSize ++ "\nchar data[MEM_SIZE];"