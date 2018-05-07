module Lexer (Token(..), tokenize, removespace) where 

data Token = Identifier String | KeyWord String | Number Int | StringConstant String | 
             PlusOperator | MinusOperator | DivideOperator | MultiplyOperator | OpenP | 
	     ClosedP | Comma | Space | AssignmentOperator |
	     OpenScope | ClosedScope | ListBegin | ListEnd deriving (Show, Eq)

takestring :: String -> (Token, String)
takestring ('"':s) = (StringConstant "", s)
takestring (x:s) = let (StringConstant rest, l) = takestring s in (StringConstant (x:rest), l)


tonum :: Char -> Int 
tonum '0' = 0 
tonum '1' = 1
tonum '2' = 2
tonum '3' = 3
tonum '4' = 4
tonum '5' = 5
tonum '6' = 6
tonum '7' = 7
tonum '8' = 8
tonum '9' = 9

strtonum :: String -> Int 
strtonum "" = 0
strtonum s = (tonum x) + 10 * (strtonum l) where (x,l) = (head (reverse s), reverse (tail (reverse s)))

takenumber :: String -> (String, String) 
takenumber [] = ("", [])
takenumber (x:s) = if '0' <= x && x <= '9' then let (n, l) = (takenumber s) in (x:n, l) else ("", x:s)

takeidentifier :: String -> (Token, String) 
takeidentifier [] = (Identifier "", [])
takeidentifier (x:s) = if ('a' <= x && x <= 'z') || ('A' <= x && x <= 'Z') then let (Identifier rest, l) = takeidentifier s  in (Identifier (x:rest), l) else (Identifier "", x:s)

tokenize :: String -> [Token] 
tokenize "" = []
tokenize ('"':s) = ss : (tokenize l) where (ss, l) = takestring s
tokenize (' ':s) = Space : (tokenize s )
tokenize ('\t' : s) = Space : (tokenize s )
tokenize ('\n' : s) = Space : (tokenize s )
tokenize (')':s) = ClosedP : ( tokenize s )
tokenize (',':s) = Comma : (tokenize s)
tokenize ('(':s) = OpenP : ( tokenize s )
tokenize ('*':s) = MultiplyOperator : ( tokenize s )
tokenize ('/':s) = DivideOperator : (tokenize s) 
tokenize ('-':s) = MinusOperator : (tokenize s) 
tokenize ('+':s) = PlusOperator : (tokenize s)
tokenize ('=':s) = AssignmentOperator : (tokenize s)
tokenize ('l':'a':'m':'b':'d':'a':s) = (KeyWord "lambda") : (tokenize s)
tokenize ('{':s) = OpenScope : (tokenize s)
tokenize ('}':s) = ClosedScope : (tokenize s)
tokenize ('[':s) = ListBegin : (tokenize s)
tokenize (']':s) = ListEnd : (tokenize s)
tokenize (x:s) 
              | '0' <= x && x <= '9' = let (t,l) = takenumber (x:s)  in (Number (strtonum t)):(tokenize l) 
              | ('a' <= x && x <= 'z') || ('A' <= x && x <= 'Z')  = let (t,l) = takeidentifier (x:s) in t:(tokenize l) 

removespace :: [Token] -> [Token]
removespace [] = []
removespace (Space:l) = removespace l 
removespace (x:xs) = x : (removespace xs)
