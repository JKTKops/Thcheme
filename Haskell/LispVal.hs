module LispVal (
  LispVal (..)
) where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where
  show (Atom s) = s
  show (Number n) = show n
  show (String s) = "\"" ++ s ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List ls) = "(" ++ unwordsList ls ++ ")"
  show (DottedList ls l) = "(" ++ unwordsList ls ++ " . " ++ show l ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show
