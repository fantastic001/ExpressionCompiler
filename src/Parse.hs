module Parse where 

import Lexer
import Control.Applicative 

type LineNumber = Int 
data ParseResult a = ParseResult [Token] a | ParseError LineNumber String deriving (Show)
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
		(ParseResult tokens' x) -> let (Parse run') = f x in case run' tokens' of 
			(ParseError l m) -> ParseError l m 
			res -> res 
		(ParseError l m) -> ParseError l m 


data Factor = JustNumber Int | FactorExpression Expression deriving (Show)
data Term = MultiplicationTerm Factor Term | DivisionTerm Factor Term | JustFactor Factor deriving (Show)
data Expression = SumExpression Term Expression | DifferenceExpression Term Expression | JustTerm Term | NegativeTerm Term  deriving (Show)


accept :: Parse Token 
accept = Parse $ \tokens -> case tokens of 
	[] -> ParseError 0 "Cannot take token"
	t:ts -> ParseResult ts t 

cerror :: String -> Parse a
cerror msg = Parse $ \token -> ParseError 0 msg

expect :: Token -> Parse Token
expect token = Parse $ \tokens -> case tokens of 
	[] -> ParseError 0 "Expected token but no such token found"
	t:ts -> if t == token then ParseResult ts t else ParseError 0 $ "Expected token " ++ (show token) ++ " but " ++ (show t) ++ "found"

oneOf :: Int -> String -> [Parse a] -> Parse a
oneOf l msg actions = Parse $ \tokens -> case actions of 
	[] -> ParseError l msg
	(Parse run):as -> case run tokens of 
		(ParseError l m) -> let (Parse run') = oneOf l msg as in run' tokens 
		(ParseResult ts x) -> ParseResult ts x

factor :: Parse Factor 
factor = do 
	t <- accept 
	case t of 
		Number n -> return $ JustNumber n
		OpenP -> do 
			expr <- expression 
			expect ClosedP
			return $ FactorExpression expr
		_ -> cerror "Expected ( or number here"


division = do 
	f <- factor 
	expect DivideOperator 
	t <- term 
	return $ DivisionTerm f t 

multiplication = do 
	f <- factor 
	expect MultiplyOperator 
	t <- term 
	return $ MultiplicationTerm f t 

factorterm = do 
	f <- factor
	return $ JustFactor f

term :: Parse Term 
term = oneOf 0 "Cannot pparse term" [division, multiplication, factorterm]

expression :: Parse Expression 
expression = oneOf 0 "Cannot parse expression" [sumexpression, termexpression, negativetermexpression]

sumexpression :: Parse Expression 
sumexpression = do 
	t <- term 
	op <- accept
	e <- expression
	case op of 
		PlusOperator -> return $ SumExpression t e 
		MinusOperator -> return $ DifferenceExpression t e 
		_ -> cerror "Cannot parse expression as sum or difference"

termexpression :: Parse Expression 
termexpression = do 
	t <- term 
	return $ JustTerm t 


negativetermexpression :: Parse Expression 
negativetermexpression = do 
	expect (MinusOperator) 
	t <- term 
	return $ NegativeTerm t 


data Statement = Assignment String Expression deriving (Show)

assignment :: Parse Statement
assignment = do 
	t <- accept 
	case t of 
		(Identifier identifier) -> do 
			expect AssignmentOperator 
			expr <- expression 
			return $ Assignment identifier expr
		_ -> cerror "Identifier expected"

parse :: String -> Parse Statement -> ParseResult Statement
parse s (Parse run) = run ((removespace . tokenize) s)

compile :: String -> ParseResult Statement 
compile s = parse s assignment 
