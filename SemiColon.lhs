\documentclass[11pt]{article}

\usepackage[margin=2.5cm]{geometry}

\title{\bf Monads as Programmable Semicolons}
\author{Matthew Wraith}
\date{\today}

\begin{document}
\maketitle

\begin{abstract}\noindent
Monads are often described as programmable semi-colons. This explains what that means.
\end{abstract}

\section{Introduction}

\section{What Semi-colons Actually Mean}

\section{An Abstraction for Semi-colons}

\section{In Practice}

\section{Conclusion}


\begin{code}
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
unsafePop = State $ \(x:xs) -> (x,xs)

pop :: Stack a (Maybe a)
pop = 
    get =: \ls -> 
    case ls of
        [] -> ret Nothing
        x:xs -> put xs ! ret (Just x)
    
stackTest :: IntStack Int
stackTest = 
    push 5 !
    push 2 !
    pop !
    push 23 !
    pop =: \(Just x) ->
    push (x + 4) !
    unsafePop

main = print $ runState stackTest []
\end{code}

\end{document}
