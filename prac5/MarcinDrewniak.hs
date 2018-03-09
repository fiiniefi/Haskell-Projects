{-# LANGUAGE Safe #-}
module MarcinDrewniak (typecheck, eval) where
import AST
import DataTypes

data Types p = Err p String | T Type deriving(Eq, Show)

data ResTypes = Bln Bool | Itgr Integer | DivZero | Unit | Pr (ResTypes, ResTypes) | Lt [ResTypes] deriving(Eq, Show)



{-********TYPECHECK*********-}

typecheck :: [FunctionDef p] -> [Var] -> Expr p -> TypeCheckResult p   --z glownego typechecka wywolam recTypecheck ktory pozwoli mi na zwrocenie wartosci typu Types
typecheck functions variables program = 
    let pdat = getData program
    in let pairs = makePairs variables
    in case functionCheck functions functions of
            Nothing -> case recTypecheck functions pairs program of
                            Err pos mess -> Error pos mess
                            T TInt -> Ok
                            _ -> Error pdat ("Wrong value of the program")
            Just (Err pos mess) -> Error pos mess

--przepisanie AST
recTypecheck :: [FunctionDef p] -> [(Var, Types p)] -> Expr p -> Types p
recTypecheck _ variables (EVar p var) = isMember p var variables
recTypecheck _ _ (ENum p _) = T TInt
recTypecheck _ _ (EBool p _) = T TBool
recTypecheck functions variables (EUnary p op expr) = 
        case recTypecheck functions variables expr of
             Err p1 message -> Err p1 message
             T TInt -> case op of
                       UNeg -> T TInt
                       _ -> Err p ("Wrong unary operator applied to int expression")
             T TBool -> case op of
                           UNot -> T TBool
                           _ -> Err p ("Wrong unary operator applied to bool expression")
             _ -> Err p ("Wrong unary operator applied to expression")
recTypecheck functions variables (EBinary p op expr1 expr2) =
         case recTypecheck functions variables expr1 of
              Err pos mess -> Err pos mess
              T x -> case recTypecheck functions variables expr2 of
                          Err pos mess -> Err pos mess
                          T y -> case x == y of
                                      False -> Err p ("Different types on both sides of binary operator")
                                      True -> case T x of
                                                   T TBool -> case op of
                                                                   BOr -> T x
                                                                   BAnd -> T x
                                                                   _ -> Err p ("Wrong binary operator applied to bool expressions")
                                                   T TInt -> case op of
                                                                  BEq -> T TBool
                                                                  BNeq -> T TBool
                                                                  BLt -> T TBool
                                                                  BGt -> T TBool
                                                                  BLe -> T TBool
                                                                  BGe -> T TBool
                                                                  BAdd -> T x
                                                                  BSub -> T x
                                                                  BMul -> T x
                                                                  BDiv -> T x
                                                                  BMod -> T x
                                                                  _ -> Err p ("Wrong binary operator applied to int expressions")
                                                   _ -> Err p ("Wrong binary operator applied to expression")
recTypecheck functions variables (ELet p var expr1 expr2) = 
         case recTypecheck functions variables expr1 of
              Err pos mess -> Err pos mess
              y -> recTypecheck functions ((var, y):variables) expr2
recTypecheck functions variables (EIf p expr1 expr2 expr3) = 
         case recTypecheck functions variables expr1 of
              T TBool -> case recTypecheck functions variables expr2 of
                              Err pos mess -> Err pos mess
                              T x -> case recTypecheck functions variables expr3 of
                                          Err pos mess -> Err pos mess
                                          T y -> case x == y of
                                                      False -> Err p ("Branches of \"if\" statement have different types")
                                                      True -> T y
              _ -> Err p ("Condition of \"if\" statement is not a bool")
recTypecheck functions variables (EApp p id expr) = 
             case giveFunction id functions of
                  Nothing -> Err p ("Function "++id++" hasn't been declared")
                  Just fun -> case ( ( T (funcArgType fun) ), (recTypecheck functions variables expr) ) of
                                   (_, (Err pos mess)) -> Err pos mess 
                                   (T a, T b) | a == b -> T (funcResType fun)
                                   _ -> Err (funcPos fun) ("In function "++id++" invalid type of argument")
recTypecheck _ _ (EUnit _) = T TUnit
recTypecheck functions variables (EPair _ expr1 expr2) = 
         case recTypecheck functions variables expr1 of
              Err pos mess -> Err pos mess
              T x -> case recTypecheck functions variables expr2 of
                        Err pos mess -> Err pos mess
                        T y -> T (TPair x y)
                       -- _ -> Err p ("EPair: Unknown error")
             -- _ -> Err p ("EPair: Unknown error")
recTypecheck functions variables (EFst p expr) = 
         case recTypecheck functions variables expr of
              Err pos mess -> Err pos mess
              T (TPair a _) -> T a
              _ -> Err p ("\"fst\" operator applied to something that's not a pair")
recTypecheck functions variables (ESnd p expr) = 
         case recTypecheck functions variables expr of
              Err pos mess -> Err pos mess
              T (TPair _ b) -> T b
              _ -> Err p ("\"snd\" operator applied to something that's not a pair")
recTypecheck functions variables (ENil p t) = 
         case t of 
              TList x -> T (TList x)
              _ -> Err p ("Wrong list type")
recTypecheck functions variables (ECons p expr1 expr2) = 
         case recTypecheck functions variables expr1 of
              Err pos mess -> Err pos mess
              T a -> case recTypecheck functions variables expr2 of
                          Err pos mess -> Err pos mess
                          T (TList b) -> case a == b of
                                            True -> T (TList b)
                                            False -> Err p ("Head and tail have different types")
                          _ -> Err p ("In list constructor: Tail is not a list")
recTypecheck functions variables (EMatchL p expr empty (var1, var2, consexpr)) = 
         case recTypecheck functions variables expr of
              Err pos mess -> Err pos mess
              T (TList a) -> case recTypecheck functions variables empty of
                                Err pos mess -> Err pos mess
                                T x -> case recTypecheck functions ((var1, T a) : (var2, T (TList a)) : variables) consexpr of
                                            Err pos mess -> Err pos mess
                                            T y -> case x == y of
                                                        True -> T x
                                                        False -> Err p ("Branches of \"match\" statement have different types")
              _ -> Err p ("Match condition is not a list") 



{-*********EVAL********-}

eval :: [FunctionDef p] -> [(Var,Integer)] -> Expr p -> EvalResult  --z glownego evala wywolam rekurencyjny recEval, ktory pozwala mi na zwrocenie wartosci typu ResTypes
eval functions pairs program =
     let pairsrt = makeResType pairs
     in let res = recEval functions pairsrt program
     in if res == DivZero then RuntimeError
        else 
        let Itgr result = res
        in Value result

--przepisanie AST
recEval :: [FunctionDef p] -> [(Var, ResTypes)] -> Expr p -> ResTypes
recEval _ pairs (EVar _ var) = valueOf var pairs
recEval _ _ (ENum _ itg) = Itgr itg
recEval _ _ (EBool _ booln) = Bln booln
recEval functions pairs (EUnary _ op expr) = 
        let x = recEval functions pairs expr
        in case x of
            Itgr itg -> Itgr (-itg)
            Bln booln -> Bln (not booln)
            DivZero -> DivZero
recEval functions pairs (EBinary _ op expr1 expr2) = 
    case recEval functions pairs expr1 of
         DivZero -> DivZero
         Itgr i1 -> case recEval functions pairs expr2 of
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
         Bln i1 -> case recEval functions pairs expr2 of
                DivZero -> DivZero
                Bln i2 -> case op of
                    BAnd -> Bln (i1 && i2)
                    BOr -> Bln (i1 || i2)
recEval functions pairs (ELet _ var expr1 expr2) = 
        let x = recEval functions pairs expr1
        in if x == DivZero then DivZero
           else 
           let pairsx = (var, x):pairs
           in recEval functions pairsx expr2
recEval functions pairs (EIf _ expr1 expr2 expr3) = 
        let condition = recEval functions pairs expr1
        in case condition of
            DivZero -> DivZero
            Bln True -> recEval functions pairs expr2
            Bln False -> recEval functions pairs expr3
recEval functions pairs (EApp _ id expr) = 
        case recEval functions pairs expr of
             DivZero -> DivZero
             x -> let Just fun = giveFunction id functions
                  in recEval functions [((funcArg fun), x)] (funcBody fun)
recEval _ _ (EUnit _) = Unit
recEval functions pairs (EPair _ expr1 expr2) = 
        case recEval functions pairs expr1 of
             DivZero -> DivZero
             x -> case recEval functions pairs expr2 of
                       DivZero -> DivZero
                       y -> Pr (x, y)
recEval functions pairs (EFst _ expr) = 
        case recEval functions pairs expr of
             DivZero -> DivZero
             Pr (a, b) -> a
recEval functions pairs (ESnd _ expr) = 
        case recEval functions pairs expr of
             DivZero -> DivZero
             Pr (a, b) -> b
recEval _ _ (ENil _ _) = Lt []
recEval functions pairs (ECons _ expr1 expr2) = 
        case recEval functions pairs expr1 of
             DivZero -> DivZero
             x -> case recEval functions pairs expr2 of
                       DivZero -> DivZero
                       Lt y -> Lt (x:y)
recEval functions pairs (EMatchL _ expr empty (var1, var2, consexpr)) = 
        case recEval functions pairs expr of
             DivZero -> DivZero
             Lt [] -> recEval functions pairs empty
             Lt (x:xs) -> recEval functions ((var1, x) : (var2, Lt xs) : pairs) consexpr




{-*********FUNKCJE POMOCNICZE********-}

makePairs :: [Var] -> [(Var, Types p)]   --z listy zmiennych tworzy liste par (zmienna, typ). robi same integery, bo to lista wejsciowa
makePairs [] = []
makePairs (var:variables) = (var, T TInt):makePairs variables

isMember :: p -> Var -> [(Var, Types p)] -> Types p   --sprawdza, czy zmienna do ktorej probujemy sie odwolac zostala zadeklarowana
isMember p var [] = Err p ("Variable \"" ++ var ++ "\" has not been declared")
isMember p var (x:variables) = if var == fst x then snd x 
                               else 
                               isMember p var variables

giveFunction :: String -> [FunctionDef p] -> Maybe (FunctionDef p)  --zwraca poszukiwana funkcje, ktora bedziemy "wywolywac"
giveFunction name [] = Nothing
giveFunction name (x:xs) = if (funcName x) == name
                           then Just x
                           else giveFunction name xs

functionCheck :: [FunctionDef p] -> [FunctionDef p] -> Maybe (Types p)   --sprawdza poprawnosc wewnatrz funkcji
functionCheck all [] = Nothing
functionCheck all (x:xs) = case recTypecheck all [( (funcArg x), ( T (funcArgType x) ) )] (funcBody x) of
                                Err pos mess -> Just (Err pos mess)
                                T a -> case (funcResType x) of
                                       b | a == b -> functionCheck all xs
                                       _ -> Just (Err (funcPos x) ("In function "++(funcName x)++" result type is invalid"))

makeResType :: [(Var, Integer)] -> [(Var, ResTypes)]   --zamienia w parach typ Integer na ResTypes
makeResType [] = []
makeResType ((var, value):pairs) = (var, Itgr value):makeResType pairs    

valueOf :: Var -> [(Var, ResTypes)] -> ResTypes   --wyznacza wartosc zmiennej
valueOf var (pair:pairs) = if var == (fst pair) then (snd pair)
                           else 
                           valueOf var pairs