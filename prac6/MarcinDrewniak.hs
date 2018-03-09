{-# LANGUAGE Safe #-}
module MarcinDrewniak (typecheck, eval) where
import AST
import DataTypes

data Types p = Err p String | T Type deriving(Eq, Show)

data ResTypes p = Bln Bool | Itgr Integer | DivZero | Unit | Pr (ResTypes p, ResTypes p) | Lt [ResTypes p] | FClose [FunctionDef p] (FunctionDef p) | LClose [(Var, ResTypes p)] Var (Expr p) deriving(Eq, Show)



{-********TYPECHECK*********-}

typecheck :: [FunctionDef p] -> [Var] -> Expr p -> TypeCheckResult p   --z glownego typechecka wywolam recTypecheck ktory pozwoli mi na zwrocenie wartosci typu Types
typecheck functions variables program = 
    let pdat = getData program
    in let environment = makeEnvironment functions variables
    in case functionCheck (makeEnvironment functions []) functions of
            Nothing -> case recTypecheck environment program of
                            Err pos mess -> Error pos mess
                            T TInt -> Ok
                            _ -> Error pdat ("Wrong value of the program")
            Just (Err pos mess) -> Error pos mess

--przepisanie AST
recTypecheck :: [(Var, Types p)] -> Expr p -> Types p
recTypecheck environment (EVar p var) = isMember p var environment
recTypecheck _ (ENum p _) = T TInt
recTypecheck _ (EBool p _) = T TBool
recTypecheck environment (EUnary p op expr) = 
        case recTypecheck environment expr of
             Err p1 message -> Err p1 message
             T TInt -> case op of
                       UNeg -> T TInt
                       _ -> Err p ("Wrong unary operator applied to int expression")
             T TBool -> case op of
                           UNot -> T TBool
                           _ -> Err p ("Wrong unary operator applied to bool expression")
             _ -> Err p ("Wrong unary operator applied to expression")
recTypecheck environment (EBinary p op expr1 expr2) =
         case recTypecheck environment expr1 of
              Err pos mess -> Err pos mess
              T x -> case recTypecheck environment expr2 of
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
recTypecheck environment (ELet p var expr1 expr2) = 
         case recTypecheck environment expr1 of
              Err pos mess -> Err pos mess
              y -> recTypecheck ((var, y):environment) expr2
recTypecheck environment (EIf p expr1 expr2 expr3) = 
         case recTypecheck environment expr1 of
              T TBool -> case recTypecheck environment expr2 of
                              Err pos mess -> Err pos mess
                              T x -> case recTypecheck environment expr3 of
                                          Err pos mess -> Err pos mess
                                          T y -> case x == y of
                                                      False -> Err p ("Branches of \"if\" statement have different types")
                                                      True -> T y
              _ -> Err p ("Condition of \"if\" statement is not a bool")
recTypecheck environment (EApp p expr1 expr2) = 
             case recTypecheck environment expr1 of
                  Err pos mess -> Err pos mess
                  T (TArrow ft1 ft2) -> case recTypecheck environment expr2 of
                                           Err pos mess -> Err pos mess
                                           T st -> case ft1 == st of
                                                   True -> T ft2
                                                   False -> Err p ("Invalid type of argument in function expression")
                  _ -> Err p ("In Application: expected function expression")
recTypecheck _ (EUnit _) = T TUnit
recTypecheck environment (EPair _ expr1 expr2) = 
         case recTypecheck environment expr1 of
              Err pos mess -> Err pos mess
              T x -> case recTypecheck environment expr2 of
                        Err pos mess -> Err pos mess
                        T y -> T (TPair x y)
recTypecheck environment (EFst p expr) = 
         case recTypecheck environment expr of
              Err pos mess -> Err pos mess
              T (TPair a _) -> T a
              _ -> Err p ("\"fst\" operator applied to something that's not a pair")
recTypecheck environment (ESnd p expr) = 
         case recTypecheck environment expr of
              Err pos mess -> Err pos mess
              T (TPair _ b) -> T b
              _ -> Err p ("\"snd\" operator applied to something that's not a pair")
recTypecheck environment (ENil p t) = 
         case t of 
              TList x -> T (TList x)
              _ -> Err p ("Wrong list type")
recTypecheck environment (ECons p expr1 expr2) = 
         case recTypecheck environment expr1 of
              Err pos mess -> Err pos mess
              T a -> case recTypecheck environment expr2 of
                          Err pos mess -> Err pos mess
                          T (TList b) -> case a == b of
                                            True -> T (TList b)
                                            False -> Err p ("Head and tail have different types")
                          _ -> Err p ("In list constructor: Tail is not a list")
recTypecheck environment (EMatchL p expr empty (var1, var2, consexpr)) = 
         case recTypecheck environment expr of
              Err pos mess -> Err pos mess
              T (TList a) -> case recTypecheck environment empty of
                                Err pos mess -> Err pos mess
                                T x -> case recTypecheck ((var1, T a) : (var2, T (TList a)) : environment) consexpr of
                                            Err pos mess -> Err pos mess
                                            T y -> case x == y of
                                                        True -> T x
                                                        False -> Err p ("Branches of \"match\" statement have different types")
              _ -> Err p ("Match condition is not a list")
recTypecheck environment (EFn p var t expr) = 
         case recTypecheck ((var, (T t)):environment) expr of
              Err pos mess -> Err pos mess
              T rt -> T (TArrow t rt)



{-*********EVAL********-}

eval :: [FunctionDef p] -> [(Var,Integer)] -> Expr p -> EvalResult  --z glownego evala wywolam rekurencyjny recEval, ktory pozwala mi na zwrocenie wartosci typu ResTypes
eval functions pairs program =
     let pairsrt = makeResType functions functions pairs
     in case recEval pairsrt program of
             DivZero -> RuntimeError
             Itgr result -> Value result

--przepisanie AST
recEval :: [(Var, ResTypes p)] -> Expr p -> ResTypes p
recEval environment (EVar _ var) = valueOf var environment
recEval _ (ENum _ itg) = Itgr itg
recEval _ (EBool _ booln) = Bln booln
recEval environment (EUnary _ op expr) = 
        let x = recEval environment expr
        in case x of
            Itgr itg -> Itgr (-itg)
            Bln booln -> Bln (not booln)
            DivZero -> DivZero
recEval environment (EBinary _ op expr1 expr2) = 
    case recEval environment expr1 of
         DivZero -> DivZero
         Itgr i1 -> case recEval environment expr2 of
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
         Bln i1 -> case recEval environment expr2 of
                DivZero -> DivZero
                Bln i2 -> case op of
                    BAnd -> Bln (i1 && i2)
                    BOr -> Bln (i1 || i2)
recEval environment (ELet _ var expr1 expr2) = 
       case recEval environment expr1 of
            DivZero -> DivZero
            x -> recEval ((var, x):environment) expr2
recEval environment (EIf _ expr1 expr2 expr3) = 
        let condition = recEval environment expr1
        in case condition of
            DivZero -> DivZero
            Bln True -> recEval environment expr2
            Bln False -> recEval environment expr3
recEval environment (EApp _ expr1 expr2) = 
        case recEval environment expr2 of 
             DivZero -> DivZero
             t -> case recEval environment expr1 of
                       DivZero -> DivZero
                       FClose funs f -> recEval ( ( (funcArg f), t):(makeResType funs funs [] ) ) (funcBody f)
                       LClose env var body -> recEval ((var, t):env) body
recEval _ (EUnit _) = Unit
recEval environment (EPair _ expr1 expr2) = 
        case recEval environment expr1 of
             DivZero -> DivZero
             x -> case recEval environment expr2 of
                       DivZero -> DivZero
                       y -> Pr (x, y)
recEval environment (EFst _ expr) = 
        case recEval environment expr of
             DivZero -> DivZero
             Pr (a, b) -> a
recEval environment (ESnd _ expr) = 
        case recEval environment expr of
             DivZero -> DivZero
             Pr (a, b) -> b
recEval _ (ENil _ _) = Lt []
recEval environment (ECons _ expr1 expr2) = 
        case recEval environment expr1 of
             DivZero -> DivZero
             x -> case recEval environment expr2 of
                       DivZero -> DivZero
                       Lt y -> Lt (x:y)
recEval environment (EMatchL _ expr empty (var1, var2, consexpr)) = 
        case recEval environment expr of
             DivZero -> DivZero
             Lt [] -> recEval environment empty
             Lt (x:xs) -> recEval ((var1, x) : (var2, Lt xs) : environment) consexpr
recEval environment (EFn _ var _ expr) = LClose environment var expr

 


{-*********FUNKCJE POMOCNICZE********-}

makeEnvironment :: [FunctionDef p] -> [Var] -> [(Var, Types p)]   --z listy zmiennych tworzy liste par (zmienna, typ). robi same integery, bo to lista wejsciowa
makeEnvironment [] [] = []
makeEnvironment (f:functions) [] = ( (funcName f), (T (TArrow (funcArgType f) (funcResType f)) ) ):makeEnvironment functions []
makeEnvironment functions (var:variables) = (var, T TInt):makeEnvironment functions variables

isMember :: p -> Var -> [(Var, Types p)] -> Types p   --sprawdza, czy zmienna do ktorej probujemy sie odwolac zostala zadeklarowana
isMember p var [] = Err p ("Variable \"" ++ var ++ "\" has not been declared")
isMember p var (x:variables) = if var == fst x then snd x 
                               else 
                               isMember p var variables

giveFunction :: String -> [(Var, Types p)] -> Maybe ((Var, Types p))  --zwraca poszukiwana funkcje, ktora bedziemy "wywolywac"
giveFunction name [] = Nothing
giveFunction name (x:xs) = if (fst x) == name
                           then Just x
                           else giveFunction name xs

functionCheck :: [(Var, Types p)] -> [FunctionDef p] -> Maybe (Types p)   --sprawdza poprawnosc wewnatrz funkcji
functionCheck all [] = Nothing
functionCheck all (x:xs) = case recTypecheck ( ( (funcArg x), ( T (funcArgType x) ) ):all ) (funcBody x) of
                                Err pos mess -> Just (Err pos mess)
                                T a -> case (funcResType x) of
                                       b | a == b -> functionCheck all xs
                                       _ -> Just (Err (funcPos x) ("In function "++(funcName x)++" result type is invalid"))

makeResType :: [FunctionDef p] -> [FunctionDef p] -> [(Var, Integer)] -> [(Var, ResTypes p)]   --zamienia w parach typ Integer na ResTypes
makeResType _ [] [] = []
makeResType funs (f:functions) [] = ((funcName f), (FClose funs f)):makeResType funs functions []
makeResType funs functions ((var, value):pairs) = (var, Itgr value):makeResType funs functions pairs

valueOf :: Var -> [(Var, ResTypes p)] -> ResTypes p   --wyznacza wartosc zmiennej
valueOf var (pair:pairs) = if var == (fst pair) then (snd pair)
                           else 
                           valueOf var pairs