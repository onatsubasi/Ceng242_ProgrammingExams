{-# LANGUAGE FlexibleInstances #-}

module PE3 where

import Data.List (sort, sortBy)
import Text.Printf (printf)

data Term = Const Integer | Pw Integer Power | Trig Integer Power Trigonometric | Exp Integer Power Exponential

data Power = Power Integer
data Polynomial = Polynomial [(Integer, Power)]
data Exponential = Exponential Polynomial
data Trigonometric = Sin Polynomial | Cos Polynomial

class Evaluable a where
    function :: a -> (Integer -> Double)

class Differentiable a where
    derivative :: a -> [Term]

-- You can use this as is
getRounded :: Double -> Double 
getRounded x = read s :: Double
               where s = printf "%.2f" x

-- You don't have to follow the order the functions appear in the file
-- For example, you could first define all Show instances, then all Eq instances etc.
-- if that implementation order is more convenient for you.

checkplus:: [Char] -> Bool
checkplus [] = False
checkplus (char:lst) = if char == '+' then True else checkplus lst

checkexp [] = False
checkexp (char:lst) = if char == '^' then True else checkexp lst

checkx [] = False
checkx (char:lst) = if char == 'x' then True else checkx lst

checkneg [] = False
checkneg (char:lst) = if char == '-' then True else checkneg lst
-- INSTANCES FOR POWER

instance Show Power where
    show (Power 0) = "1"
    show (Power 1) = "x"
    show (Power n) = "x^"++ show n

instance Eq Power where
    (Power i) == (Power j) = if i == j then True else False


instance Ord Power where
    (Power i) <= (Power j) = if i <= j then True else False
    (Power i) < (Power j) = if i < j then True else False
    (Power i) >= (Power j) = if i >= j then True else False
    (Power i) > (Power j) = if i > j then True else False

instance Evaluable Power where
    function (Power i) x = getRounded ((fromInteger x)**(fromInteger i))

instance Differentiable Power where
    derivative (Power 0) = [Const 0]
    derivative (Power 1) = [Const 1]
    derivative (Power j) = [(Pw j (Power (j-1)))]



-- INSTANCES FOR POLYNOMIAL

instance Show Polynomial where

    show (Polynomial []) = ""
    show (Polynomial ((1, Power 0):lst)) = if lst == [] then show 1 else show 1 ++ " + " ++ show (Polynomial lst)
    show (Polynomial ((1, Power j):lst)) = if lst == [] then show (Power j) else show (Power j) ++ " + " ++ show (Polynomial lst)
    show (Polynomial ((-1, Power 0):lst)) = if lst == [] then show (-1) else show (-1) ++ " + " ++ show (Polynomial lst)
    show (Polynomial ((-1, Power j):lst)) = if lst == [] then "-" ++ show (Power j) else "-" ++ show (Power j) ++ " + " ++ show (Polynomial lst)
    show (Polynomial ((i, Power 0):lst)) = if lst == [] then show i else show i ++ " + " ++ show (Polynomial lst)
    show (Polynomial ((i, Power j):lst)) = if lst == [] then show i ++ show (Power j) else show i ++ show (Power j) ++ " + " ++ show (Polynomial lst)

instance Evaluable Polynomial where
    function (Polynomial []) x = 0.0
    function (Polynomial ((i,Power j):lst)) x = getRounded (fromInteger i* function (Power j) x + function (Polynomial lst) x)

instance Differentiable Polynomial where
    derivative (Polynomial []) = []
    derivative (Polynomial ((i, Power j):lst)) = if show (Pw (i*j) (Power (j-1))) == "0" then [] ++ derivative (Polynomial lst) else [Pw (i*j) (Power (j-1))] ++ derivative (Polynomial lst)




-- INSTANCES FOR TRIGONOMETRIC

instance Show Trigonometric where
    show (Cos (Polynomial lst))  =  if checkplus polynomial || checkexp polynomial || checkneg polynomial
                                        then "cos(" ++ polynomial ++ ")"
                                        else "cos" ++ polynomial
                                    where polynomial = show (Polynomial lst)

    show (Sin (Polynomial lst))  =  if checkplus polynomial || checkexp polynomial || checkneg polynomial
                                        then "sin(" ++ polynomial ++ ")"
                                        else "sin" ++ polynomial
                                    where polynomial = show (Polynomial lst)

instance Evaluable Trigonometric where
    function (Sin (Polynomial lst)) x = getRounded (sin (function (Polynomial lst) x))
    function (Cos (Polynomial lst)) x = getRounded (cos (function (Polynomial lst) x))


instance Differentiable Trigonometric where
    derivative (Sin (Polynomial lst)) = [Trig (i*j) (Power (j-1)) (Cos (Polynomial lst)) | (i, (Power j)) <- lst ]
    derivative (Cos (Polynomial lst)) = [Trig (-i*j) (Power (j-1)) (Sin (Polynomial lst)) | (i, (Power j)) <- lst ]




-- INSTANCES FOR EXPONENTIAL

instance Show Exponential where
    show (Exponential (Polynomial lst)) = 
        if checkplus polynomial || checkexp polynomial || checkneg polynomial
            then "e^(" ++ polynomial ++ ")"
            else "e^" ++ polynomial
        where polynomial = show (Polynomial lst)



instance Evaluable Exponential where
    function (Exponential (Polynomial lst)) x = getRounded (exp (function (Polynomial lst) x))

instance Differentiable Exponential where
    derivative (Exponential (Polynomial lst)) = [Exp (i*j) (Power (j-1)) (Exponential (Polynomial lst)) | (i, (Power j)) <- lst]



-- INSTANCES FOR TERM

instance Show Term where
    show (Const i) = show i
    show (Pw 0 (Power j)) = "0"
    show (Pw i (Power 0)) = show i
    show (Pw 1 (Power j)) = show (Power j)
    show (Pw (-1) (Power j)) = "-" ++show (Power j)
    show (Pw i (Power j)) = show i ++ show (Power j)
    show (Exp 0 (Power j) (Exponential (Polynomial lst))) = "0"
    show (Exp 1 (Power 0) (Exponential (Polynomial lst))) = show (Exponential (Polynomial lst))
    show (Exp 1 (Power j) (Exponential (Polynomial lst))) = show (Power j) ++ show (Exponential (Polynomial lst))
    show (Exp (-1) (Power 0) (Exponential (Polynomial lst))) = "-" ++ show (Exponential (Polynomial lst))
    show (Exp (-1) (Power j) (Exponential (Polynomial lst))) = "-" ++ show (Power j) ++ show (Exponential (Polynomial lst))
    show (Exp i (Power 0) (Exponential (Polynomial lst))) = show i ++ show (Exponential (Polynomial lst))
    show (Exp i (Power j) (Exponential (Polynomial lst))) = show i ++ show (Power j) ++ show (Exponential (Polynomial lst))
    
    show (Trig 0 (Power j) (Cos (Polynomial list))) = "0"
    show (Trig 1 (Power 0) (Cos (Polynomial list))) = show (Cos (Polynomial list))
    show (Trig 1 (Power j) (Cos (Polynomial list))) = show (Power j) ++ show (Cos (Polynomial list))
    show (Trig (-1) (Power 0) (Cos (Polynomial list))) = show (Cos (Polynomial list))
    show (Trig (-1) (Power j) (Cos (Polynomial list))) = "-" ++ show (Power j) ++ show (Cos (Polynomial list))
    show (Trig i (Power 0) (Cos (Polynomial list))) = show i ++ show (Cos (Polynomial list))
    show (Trig i (Power j) (Cos (Polynomial list))) = show i ++ show (Power j) ++ show (Cos (Polynomial list))
    
    
    show (Trig 0 (Power j) (Sin (Polynomial list))) = "0"
    show (Trig 1 (Power 0) (Sin (Polynomial list))) = show (Sin (Polynomial list))
    show (Trig 1 (Power j) (Sin (Polynomial list))) = show (Power j) ++ show (Sin (Polynomial list))
    show (Trig (-1) (Power 0) (Sin (Polynomial list))) = "-" ++ show (Sin (Polynomial list))
    show (Trig (-1) (Power j) (Sin (Polynomial list))) = "-" ++ show (Power j) ++ show (Sin (Polynomial list))
    show (Trig i (Power 0) (Sin (Polynomial list))) = show i ++ show (Sin (Polynomial list))
    show (Trig i (Power j) (Sin (Polynomial list))) = show i ++ show (Power j) ++ show (Sin (Polynomial list))


instance Evaluable Term where
    function (Const i) x = fromInteger i
    function (Pw i (Power j)) x = getRounded (fromInteger(i) * (function (Power j) x))
    function (Exp i (Power j) (Exponential (Polynomial lst))) x = getRounded (fromInteger i * function (Power j) x * function (Exponential (Polynomial lst)) x)
    function (Trig i (Power j) (Cos (Polynomial lst))) x = getRounded (fromInteger i * function (Power j) x * function (Cos (Polynomial lst)) x)
    function (Trig i (Power j) (Sin (Polynomial lst))) x = getRounded (fromInteger i * function (Power j) x * function (Sin (Polynomial lst)) x)
instance Differentiable Term where
    derivative (Const i) = []
    derivative (Pw i (Power j)) = [(Pw (i*j) (Power (j-1)))]
    derivative (Exp i (Power j) (Exponential (Polynomial lst))) = [Exp (i*j) (Power (j-1)) (Exponential (Polynomial lst))] ++ [Exp (i*k) (Power (j+l)) (Exponential (Polynomial lst)) | (Pw k (Power l)) <- derivative (Polynomial lst)]
    derivative (Trig i (Power j) (Sin (Polynomial lst))) = [Trig (i*j) (Power (j-1)) (Sin (Polynomial lst))] ++ [Trig (i*k) (Power (j+l)) (Cos (Polynomial lst)) | (Pw k (Power l)) <- derivative (Polynomial lst)]
    derivative (Trig i (Power j) (Cos (Polynomial lst))) = [Trig (i*j) (Power (j-1)) (Cos (Polynomial lst))] ++ [Trig (-i*k) (Power (j+l)) (Sin (Polynomial lst)) | (Pw k (Power l)) <- derivative (Polynomial lst)]



-- INSTANCES FOR [TERM]
instance Evaluable [Term] where
    function [] _ = 0.0
    function ((Const term):lst) x = getRounded (function (Const term) x + function lst x)
    function ((Pw i (Power j)):lst) x = getRounded (function (Pw i (Power j)) x + function lst x)
    function ((Exp i (Power j) (Exponential (Polynomial list))):lst) x = getRounded (function ((Exp i (Power j) (Exponential (Polynomial list))):lst) x + function lst x)
    function ((Trig i (Power j) (Sin sinn)):lst) x = getRounded (function ((Trig i (Power j) (Sin sinn))) x + function lst x)
    function ((Trig i (Power j) (Cos coss)):lst) x = getRounded (function ((Trig i (Power j) (Cos coss))) x + function lst x)

instance Differentiable [Term] where
    derivative [] = []
    derivative ((Const term):lst) = derivative lst
    derivative ((Pw i (Power j)):lst) = if not (checkx (show (Pw i (Power j)))) then derivative lst else derivative (Pw i (Power j)) ++ derivative lst
    derivative ((Exp i (Power j) (Exponential (Polynomial list))):lst) = if not (checkx (show (Exp i (Power j) (Exponential (Polynomial list))))) then derivative lst else derivative (Exp i (Power j) (Exponential (Polynomial list))) ++ derivative lst
    derivative ((Trig i (Power j) (Sin sinn)):lst) = if not (checkx (show (Trig i (Power j) (Sin sinn)))) then derivative lst else derivative (Trig i (Power j) (Sin sinn)) ++ derivative lst
    derivative ((Trig i (Power j) (Cos sinn)):lst) = if not (checkx (show (Trig i (Power j) (Cos sinn)))) then derivative lst else derivative (Trig i (Power j) (Cos sinn)) ++ derivative lst
    
