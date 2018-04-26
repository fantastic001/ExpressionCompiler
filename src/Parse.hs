module Parse where 

import Lexer
import Control.Applicative 

type LineNumber = Int 
data ParseResult a = ParseResult [Token] a | ParseError LineNumber String
data Parse a = Parse ([Token] -> ParseResult a)

instance Functor Parse where 
	fmap f action = do 
		x <- action 
		return $ f x 

instance Applicative Parse where 
	(<*>) af action = do 
		f <- af 
		x <- action 
		return $ f x 
	pure = return 

instance Monad Parse where 
	return x = Parse $ \tokens -> ParseResult tokens x
	(Parse run) >>= f = Parse $ \tokens -> case run tokens of 
		(ParseResult tokkens' x) -> let (Parse run') = f x in case run' tokens'
		(ParseError l msg) -> ParseError l msg


data Factor = JustNumber Int | FactorExpression Expression deriving (Show)
data Term = MultiplicationTerm Factor Term | DivisionTerm Factor Term | JustFactor Factor deriving (Show)
data Expression = SumExpression Term Expression | DifferenceExpression Term Expression | JustTerm Term | NegativeTerm Term  deriving (Show)


accept :: Parse Token 
accept = Parse $ \tokens -> case tokens of 
	[] -> ParseError 0 "Cannot take token"
	t:ts -> ParseResult ts t 

error :: String -> Parse ()
error msg = Parse $ \token -> ParseError 0 msg

expect :: Token -> Parse Token
expect token = Parse $ \tokens -> case tokens of 
	[] -> ParseError 0 "Expected token but no such token found"
	t:ts -> if t == token then ParseResult ts t else ParseError 0 $ "Expected token " ++ (show token) ++ " but " ++ (show t) ++ "found"

oneOf :: Int -> String -> [Parse a] -> Parse a
oneOf l msg actions = Parse $ \tokens -> case actions of 
	[] -> ParseError l msg
	(Parse run):as -> case run tokens of 
		(ParseError l m) -> let (Parse run') = oneOf as in run' tokens 
		(ParseResult ts x) -> ParseResult ts x

factor :: Parse Factor 
factor = do 
	t <- accept 
	case t of 
		Number n -> JustNumber n
		OpenP -> do 
			expr <- expression 
			expect ClosedP
			return FactorExpression expr
		_ -> error "Expected ( or number here"


division = do 
	f <- factor 
	expect DivideOperator 
	t <- term 
	return DivisionTerm f t 

multiplication = do 
	f <- factor 
	expect MultiplyOperator 
	t <- term 
	return MultiplicationTerm f t 

factorterm = do 
	f <- factor
	return JustFactor f

term :: Parse Term 
term = oneOf 0 "Cannot pparse term" [division, multiplication, factorterm]

expression = oneOf 0 "Cannot parse expression" [sumexpression, termexpression, negativetermexpression]

sumexpression = do 
	t <- term 
	op <- accept
	e <- expression
	case op of 
		PlusOperator -> return SumExpression t e 
		MinusOperator -> return DifferenceExpression t e 
		_ -> error "Cannot parse expression as sum or difference"


termexpression = do 
	t <- term 
	return JustTerm t 


negativetermexpression = do 
	expect (MinusOperator) 
	t <- term 
	return NegativeTerm t 


data Statement = Assignment String Expression 

assignment = do 
	t <- accept 
	case t of 
		(Identifier identifier) -> do 
			expect AssignmentOperator 
			expr <- expression 
			return Assignment identifier expr

compile :: String -> Parse Statement -> ParseResult
compile s (Parse run) = run ((removespace . tokenize) s)
