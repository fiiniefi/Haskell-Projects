{-# LANGUAGE Safe #-}
module MarcinDrewniak (typecheck, eval) where
import AST
import DataTypes

data Types p = Boolean p | Itg p | Err p String deriving(Eq, Show)

data ResTypes = Bln Bool | Itgr Integer | DivZero deriving(Eq, Show)

{-*********EVAL********-}

eval :: [(Var,Integer)] -> Expr p -> EvalResult  --z glownego evala wywolam rekurencyjny recEval, ktory pozwala mi na zwrocenie wartosci typu ResTypes
eval pairs program =
     let pairsrt = makeResType pairs
     in let res = recEval pairsrt program
     in if res == DivZero then RuntimeError
        else 
        let Itgr result = res
        in Value result

--przepisanie AST
recEval :: [(Var, ResTypes)] -> Expr p -> ResTypes
recEval pairs (EVar _ var) = valueOf var pairs
recEval _ (ENum _ itg) = Itgr itg
recEval _ (EBool _ booln) = Bln booln
recEval pairs (EUnary _ op expr) = 
        let x = recEval pairs expr
        in case x of
            Itgr itg -> Itgr (-itg)
            Bln booln -> Bln (not booln)
            DivZero -> DivZero
recEval pairs (EBinary _ op expr1 expr2) = 
    case recEval pairs expr1 of
         DivZero -> DivZero
         Itgr i1 -> case recEval pairs expr2 of
                DivZero -> DivZero
                Itgr i2 -> case op of
                     BEq -> Bln (i1 == i2)
                     BNeq -> Bln (i1 /= i2)
                     BLt -> Bln (i1 < i2)
                     BGt -> Bln (i1 > i2)
                     BLe -> Bln (i1 <= i2)
                     BGe -> Bln (i1 >= i2)
                     BAdd -> Itgr (i1 + i2)
                     BSub -> Itgr (i1 - i2)
                     BMul -> Itgr (i1 * i2)
                     BDiv -> case i2 of
                              0 -> DivZero
                              _ -> Itgr (i1 `div` i2)
                     BMod -> case i2 of
                              0 -> DivZero
                              _ -> Itgr (i1 `mod` i2)
         Bln i1 -> case recEval pairs expr2 of
                DivZero -> DivZero
                Bln i2 -> case op of
                    BAnd -> Bln (i1 && i2)
                    BOr -> Bln (i1 || i2)
recEval pairs (ELet _ var expr1 expr2) = 
        let x = recEval pairs expr1
        in if x == DivZero then DivZero
           else 
           let pairsx = (var, x):pairs
           in recEval pairsx expr2
recEval pairs (EIf _ expr1 expr2 expr3) = 
        let condition = recEval pairs expr1
        in let x = recEval pairs expr2
        in let y = recEval pairs expr3
        in case condition of
            DivZero -> DivZero
            Bln True -> x
            Bln False -> y


{-********TYPECHECK*********-}

typecheck :: [Var] -> Expr p -> TypeCheckResult p   --z glownego typechecka wywolam recTypecheck ktory pozwoli mi na zwrocenie wartosci typu Types
typecheck variables program = 
    let pdat = getData program
    in let pairs = makePairs pdat variables
    in let programt = recTypecheck pairs program 
    in if equalTypes programt (Itg pdat) then Ok
       else 
       if equalTypes programt (Boolean pdat) then Error pdat ("Bool cannot be value of the program")
       else 
       let Err pt message = programt 
       in Error pt message

--przepisanie AST
recTypecheck :: [(Var, Types p)] -> Expr p -> Types p
recTypecheck variables (EVar p var) = isMember p var variables
recTypecheck _ (ENum p _) = Itg p
recTypecheck _ (EBool p _) = Boolean p
recTypecheck variables (EUnary p op expr) = 
         let x = recTypecheck variables expr
         in case x of
             Err p1 message -> Err p1 message
             Itg _ -> case op of
                       UNeg -> x
                       _ -> Err p ("Wrong unary operator applied to int expression")
             Boolean _ -> case op of
                           UNot -> x
                           _ -> Err p ("Wrong unary operator applied to bool expression")
recTypecheck variables (EBinary p op expr1 expr2) =
         let x = recTypecheck variables expr1
         in let y = recTypecheck variables expr2
         in if equalTypes x (Err p "") then x
            else 
            if equalTypes y (Err p "") then y
            else 
            if equalTypes x y == False then Err p ("Different types on both sides of binary operator")
            else 
            case x of
            Boolean p -> case op of
                          BOr -> x
                          BAnd -> x
                          _ -> Err p ("Wrong binary operator applied to bool expressions")
            Itg p -> case op of
                     BEq -> Boolean p
                     BNeq -> Boolean p
                     BLt -> Boolean p
                     BGt -> Boolean p
                     BLe -> Boolean p
                     BGe -> Boolean p
                     BAdd -> x
                     BSub -> x
                     BMul -> x
                     BDiv -> x
                     BMod -> x
                     _ -> Err p ("Wrong binary operator applied to int expressions")
recTypecheck variables (ELet p var expr1 expr2) = 
         let x = recTypecheck variables expr1
         in if equalTypes x (Err p "") then x 
            else
            recTypecheck ((var, x):variables) expr2
recTypecheck variables (EIf p expr1 expr2 expr3) = 
         let x = recTypecheck variables expr1
         in if equalTypes x (Boolean p) == False then Err p ("Condition of \"if\" statement is not a bool") 
            else
            let y = recTypecheck variables expr2
            in let z = recTypecheck variables expr3
            in if equalTypes y z == False then Err p ("Branches of \"if\" statement have different types")
               else y


{-*********FUNKCJE POMOCNICZE********-}

makeResType :: [(Var, Integer)] -> [(Var, ResTypes)]   --zamienia w parach typ Integer na ResTypes
makeResType [] = []
makeResType ((var, value):pairs) = (var, Itgr value):makeResType pairs    

valueOf :: Var -> [(Var, ResTypes)] -> ResTypes   --wyznacza wartosc zmiennej
valueOf var (pair:pairs) = if var == (fst pair) then (snd pair)
                           else 
                           valueOf var pairs

makePairs :: p -> [Var] -> [(Var, Types p)]   --z listy zmiennych tworzy liste par (zmienna, typ). robi same integery, bo to lista wejsciowa
makePairs p [] = []
makePairs p (var:variables) = (var, Itg p):makePairs p variables

equalTypes :: Types p -> Types p -> Bool   --sprawdza, czy typy wyrazen sa jednakowe
equalTypes a b = case (a,b) of
                 (Boolean _, Boolean _) -> True
                 (Itg _, Itg _) -> True
                 (Err _ _, Err _ _) -> True
                 _ -> False

isMember :: p -> Var -> [(Var, Types p)] -> Types p   --sprawdza, czy zmienna do ktorej probujemy sie odwolac zostala zadeklarowana
isMember p var [] = Err p ("Variable \"" ++ var ++ "\" has not been declared")
isMember p var (x:variables) = if var == fst x then snd x 
                               else 
                               isMember p var variables