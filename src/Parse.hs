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


data Factor = JustNumber Int | JustIdentifier String | 
	FunctionCall Factor [Expression] | FactorExpression Expression |
	JustList ListExpression deriving (Show)
data Term = MultiplicationTerm Factor Term | DivisionTerm Factor Term | JustFactor Factor deriving (Show)
data LambdaArgument = LambdaArgument String deriving (Show)
data Expression = SumExpression Term Expression | DifferenceExpression Term Expression | JustTerm Term | NegativeTerm Term  |
	Lambda [LambdaArgument] Scope deriving (Show)


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

factorexpression :: Parse Factor 
factorexpression = do 
	t <- accept 
	case t of 
		Number n -> return $ JustNumber n
		Identifier i -> return $ JustIdentifier i 
		OpenP -> do 
			expr <- expression 
			expect ClosedP
			return $ FactorExpression expr
		_ -> cerror "Expected ( or number here"

functionnoarguments :: Parse [Expression]
functionnoarguments = do 
	expect ClosedP
	return [] 

functionarguments :: Parse [Expression]
functionarguments = do 
	arg <- expression 
	next <- accept 
	case next of 
		ClosedP -> return [arg]
		Comma -> do 
			rest <- functionarguments 
			return $ arg : rest
		_ -> cerror "Unexpected token (expecting , or ))"

functioncall :: Parse Factor 
functioncall = do 
	func <- accept 
	case func of 
		OpenP -> do 
			fexpr <- expression 
			expect ClosedP
			expect OpenP 
			args <- oneOf 0 "Cannot parse function arguments" [functionnoarguments, functionarguments]
			return $ FunctionCall (FactorExpression fexpr) args 
		(Identifier i) -> do 
			expect OpenP 
			args <- oneOf 0 "Cannot parse function arguments" [functionnoarguments, functionarguments]
			return $ FunctionCall (JustIdentifier i) args
		_ -> cerror "Identifier or expression expected"


data ListExpression = ElementList Expression ListExpression | EmptyList deriving (Show)
expressionlist :: Parse ListExpression 
expressionlist = do 
	oneOf 0 "Cannot parse list of expressions" [
		(do 
			expect ListEnd 
			return EmptyList
			),
		(do 
			e <- expression 
			op <- accept 
			case op of 
				Comma -> do 
					rest <- expressionlist
					return $ ElementList e rest
				ListEnd -> return $ ElementList e EmptyList
				_ -> cerror "Expected comma or closed list bracket"
			)
		]
listfactor :: Parse Factor 
listfactor = do 
	expect ListBegin 
	l <- expressionlist 
	return $ JustList l

factor :: Parse Factor 
factor = oneOf 0 "Cannot parse factor" [functioncall, factorexpression, listfactor]

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

lambdaarguments :: Parse [LambdaArgument]
lambdaarguments = do 
	i <- accept 
	case i of 
		(Identifier identifier) -> do 
			rest <- lambdaarguments 
			return $ (LambdaArgument identifier) : rest
		(ClosedP) -> return []

lambda :: Parse Expression 
lambda = do 
	expect (KeyWord "lambda")
	expect (OpenP)
	args <- lambdaarguments
	expect (AssignmentOperator)
	s <- scope
	return $ Lambda args s

expression :: Parse Expression 
expression = oneOf 0 "Cannot parse expression" [
	sumexpression
	, termexpression
	, negativetermexpression
	, lambda
	]

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


data Statement = Assignment String Expression | JustExpression Expression deriving (Show)

assignment :: Parse Statement
assignment = do 
	t <- accept 
	case t of 
		(Identifier identifier) -> do 
			expect AssignmentOperator 
			expr <- expression 
			return $ Assignment identifier expr
		_ -> cerror "Identifier expected"

statement :: Parse Statement
statement = oneOf 0 "Cannot parse statement" [
	assignment, 
	( do  
		expr <- expression 
		return $ JustExpression expr
		)
	]

data Scope = StatementList [Statement] | JustStatement Statement deriving (Show)

statementlist = do 
	sl <- oneOf 0 "Cannotparse statement list" [
		(do 
			expect ClosedScope 
			return []
			),
		(do
			stmt <- statement 
			rest <- statementlist
			return $ stmt : rest)
		]
	return sl 
scope :: Parse Scope 
scope = do 
	sc <- oneOf 0 "Cannot parse scope" [
		( do 
			expect OpenScope 
			stmts <- statementlist
			return $ StatementList $ stmts
			),
		(do 
			s <- statement
			return $ JustStatement s
			)
		]
	return sc

data Program = Program [Statement] deriving (Show)
program :: Parse Program 
program = do 
	p <- oneOf 0 "Cannot parse program" [
		(do 
			stmt <- statement 
			(Program rest) <- program
			return $ Program $ stmt : rest
			),
		(return $ Program [])
		]
	return $ p

parse :: String -> Parse Program -> ParseResult Program
parse s (Parse run) = run ((removespace . tokenize) s)

compile :: String -> ParseResult Program
compile s = parse s program 
