import Control.Applicative ((<*>),(<$>))

class CompState s where
    ret     :: a -> s a
    (=:)    :: s a -> (a -> s b) -> s b
    (!)     :: s a -> s b -> s b

    x ! y = x =: (\_ -> y)

newtype State s a = State { runState :: s -> (a,s) }

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

instance CompState (State s) where
    ret x = State $ \s -> (x,s)

    h =: f = State $ \s ->
        let (a, newState) = runState h s 
        in runState (f a) newState
            

type Stack a b = State [a] b
type IntStack a = Stack Int a

push :: a -> Stack a ()
push x = modify (x:)

unsafePop :: Stack a a
unsafePop = 
    get =: \(x:xs) -> 
    put xs ! 
    ret x

pop :: Stack a (Maybe a)
pop = 
    get =: \ls -> 
    case ls of
        [] -> ret Nothing
        x:xs -> put xs ! ret (Just x)
    
binOpN :: Int -> (a -> a -> a) -> Stack a (Maybe a)
binOpN 1 op = binOp op
binOpN n op = 
    binOp op ! 
    binOpN (n - 1) op

-- Stack a (Maybe a) is a new monad?
stackMaybe :: Maybe a -> Stack a (Maybe a)
stackMaybe r =
    case r of 
        Just x -> push x ! ret r
        Nothing -> ret r

binOp :: (a -> a -> a) -> Stack a (Maybe a)
binOp op =
    pop =: \x ->
    pop =: \y ->
    stackMaybe (op <$> x <*> y)

type StackLoc = Int 
type InstrLoc = Int 
type ClassName = String
type MethodName = String

data Instruction =
     MOV StackLoc StackLoc
   | LOADIMM StackLoc Int 
   | ADD StackLoc StackLoc StackLoc
   | SUB StackLoc StackLoc StackLoc
   | MULT StackLoc StackLoc StackLoc
   | DIV StackLoc StackLoc StackLoc
   | LESS StackLoc StackLoc StackLoc
   | AND StackLoc StackLoc StackLoc
   | OR StackLoc StackLoc StackLoc
   | EQUAL StackLoc StackLoc StackLoc
   | JUMP InstrLoc
   | CJUMP StackLoc  InstrLoc  InstrLoc
   | INT2STRING StackLoc StackLoc
   | BOOL2STRING StackLoc StackLoc
   | GETFLD StackLoc Int 
   | PUTFLD Int  StackLoc
   | NEWSTRING StackLoc String
   | CATSTRINGS StackLoc  StackLoc  StackLoc
   | NEWOBJECT StackLoc ClassName Int 
   | NEWARRAY StackLoc StackLoc
   | RETURN StackLoc
   | LOADRESULT StackLoc
   | INVOKE StackLoc MethodName [StackLoc]
   | JUMPIND StackLoc
   | ARRAYREF StackLoc StackLoc StackLoc

stackTest :: IntStack Int
stackTest = 
    push 5 !
    push 2 !
    pop !
    push 23 !
    pop =: \(Just x) ->
    push (x + 4) !
    unsafePop

