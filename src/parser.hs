module Parser (compile) where 
import Lexer 


data Factor = JustNumber Int | FactorExpression Expression deriving (Show)
data Term = MultiplicationTerm Factor Term | DivisionTerm Factor Term | JustFactor Factor deriving (Show)
data Expression = SumExpression Term Expression | DifferenceExpression Term Expression | JustTerm Term | NegativeTerm Term  deriving (Show)

parsefactor :: [Token] -> (Factor, [Token])
parsefactor ((Number n):l) = (JustNumber n, l)
parsefactor ((OpenP) : l) = let (e, x:rest) = parseexpression l in if x /= ClosedP then (JustNumber 0, []) else (FactorExpression e, rest)

parseterm :: [Token] -> (Term, [Token])
parseterm l = let (f, rest) = parsefactor l in case rest of 
        [] -> (JustFactor f, [])
        MultiplyOperator:h -> let (t, other) = parseterm h in (MultiplicationTerm f t, other)
        DivideOperator:h -> let (t, other) = parseterm h in (DivisionTerm f t, other)
        x:h   -> (JustFactor f, x:h)

parseexpression :: [Token] -> (Expression, [Token])
parseexpression (MinusOperator:x) = let (t, rest) = parseterm x in (NegativeTerm t, rest)
parseexpression l = let (t, rest) = parseterm l in case rest of 
      [] -> (JustTerm t, [])
      PlusOperator:h -> let (e, haha) = parseexpression h in (SumExpression t e, haha)
      MinusOperator:h -> let (e, haha) = parseexpression h in (DifferenceExpression t e, haha)
      x:h   -> (JustTerm t, x:h)

compile :: String -> Expression 
compile s = let (e, rest) =( parseexpression . removespace . tokenize) s in e 
