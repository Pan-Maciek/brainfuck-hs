import Data.List

data Instruction =
  Add Int | Move Int
  | Print | Read
  | Loop [Instruction] 
  | Program [Instruction]
  deriving (Show)

parser :: ([Instruction], [Char]) -> ([Instruction], [Char])
parser (acc, ('+':xs)) = parser (Add 1 : acc, xs)
parser (acc, ('-':xs)) = parser (Add (-1) : acc, xs)
parser (acc, ('>':xs)) = parser (Move 1 : acc, xs)
parser (acc, ('<':xs)) = parser (Move (-1) : acc, xs)
parser (acc, ('.':xs)) = parser (Print : acc, xs)
parser (acc, (',':xs)) = parser (Read : acc, xs)
parser (acc, ('[':xs)) = parser (Loop instructions : acc, tail)
  where (instructions, tail) = parser ([], xs)
parser (acc, (']':xs)) = (acc, xs)
parser (acc, _:xs) = parser (acc, xs) -- skip comments
parser (acc, []) = (acc, []) 

parse :: [Char] -> Instruction
parse input = Program tokens
  where (tokens,_) = parser ([], input)

compile :: Instruction -> String
compile (Add val) = "data[ptr] = (data[ptr] + " ++ show (val + 256) ++ ") % 256;"
compile (Move val) = "ptr +=" ++ show val ++ ";"
compile Print = "print (data[ptr])"
compile Read = "data[ptr] = await read ()"
compile (Loop instructions) = "while (data[ptr]) { " ++ intercalate "\n" (map compile instructions) ++ " }"
compile (Program instructions) = "async function program(print, read) { const data = Array.from({length:30000},()=>0);let ptr=0; " ++ intercalate "\n" (map compile instructions) ++ " }"
