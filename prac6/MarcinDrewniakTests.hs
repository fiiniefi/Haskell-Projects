{-# LANGUAGE Safe #-}

module MarcinDrewniakTests(tests) where
import DataTypes
tests :: [Test]


tests =
    [ 
      Test "constValue" (SrcString "2") (Eval [] (Value 2)),
      Test "unary" (SrcString "input x y in if not x < y then -x else -y") (Eval [120, 1434] (Value (-1434))),
      Test "decrement" (SrcString "input x in x - 1") (Eval [42] (Value 41)),
      Test "doesntMakeSense" (SrcString "input x in if if x>0 then true else false then x + x else 0") (Eval [10] (Value 20)),
      Test "notAllUsed" (SrcString "input x y z in if x >= y then x else y") (Eval [10, 11, 12] (Value 11)),
      Test "constant" (SrcString "let x = 10 in let y = 100 in x + y") (Eval [] (Value 110)),
      Test "lksBadButItsNot" (SrcString "input x in if x<10 then x + 1 else x div 0") (Eval [9] (Value 10)),
      Test "lksBadAndItsBad" (SrcString "input x in if x<10 then x + 1 else x div 0") (Eval [11] RuntimeError),
      Test "runtimeAnyway" (SrcString "10 div 0") (Eval [] RuntimeError),
      Test "medium" (SrcFile "test1.pp6") (Eval [32, 11] (Value 43)),
      Test "big" (SrcFile "test2.pp6") (Eval [5, 2, 3, 4] (Value (-11))),
      Test "huge" (SrcFile "test3.pp6") (Eval [1, 1, 1, 2, 3, 4] (Value 9)),
      Test "undefVar" (SrcString "x + 4") TypeError,
      Test "intIf" (SrcString "if 4 then true else false") TypeError,
      Test "wrongUBool" (SrcString "not 4") TypeError,
      Test "wrongUInt" (SrcString "-true") TypeError,
      Test "wrongBInt" (SrcString "4 + true") TypeError,
      Test "wrongBBool" (SrcString "let x = 4 in true and x") TypeError,
      Test "wrongBRel" (SrcString "let x = 21 in let y = true in x < y") TypeError,
      Test "differentInIf" (SrcString "input x y in if x >= y then true else 10") TypeError, 
      Test "inputAlwaysInt" (SrcString "input x y z in if x then if y then y+z else z+y else if z then z+x else x+z") TypeError,
      Test "wrongResult" (SrcString "input x y in if x <> y then true else false") TypeError,
      Test "opticalIllusion" (SrcString "input q w e r t y u i o p a s d f g h j k l z x c b n m in q + w + e - r - t + y + u + i + o + p + a + s + d + f + g + h + j + k + l + z + x + c + v + b + n + m") TypeError,
      Test "wrongFirst" (SrcString "input a b in fst a + b") TypeError,
      Test "wrongSecond" (SrcString "snd 1") TypeError,
      Test "wrongCons" (SrcFile "test4.pp6") TypeError,
      Test "wrongTApp" (SrcFile "test7.pp6") TypeError,
      Test "diffOnMatch" (SrcFile "test13.pp6") TypeError,
      Test "wrongArgument" (SrcString "fun abc(x : bool) : int = 10 in abc (2000)") TypeError,
      Test "wrongRetValue" (SrcString "fun deg(x : unit) : int = true in deg ()") TypeError,
      Test "wrongProgValue" (SrcString "fun hjk (x : int) : bool = x < 0 in hjk(10)") TypeError,
      Test "intOnMatch" (SrcString "match 1 with [] -> 1 | x :: xs -> 21") TypeError,
      Test "insertWrongValue" (SrcString "let x = [] : int in true :: x") TypeError,
      Test "wrongFunAtTheEnd" (SrcFile "test17.pp6") TypeError,
      Test "addXToHead" (SrcFile "test5.pp6") (Eval [3] (Value 3)),
      Test "voidArguments" (SrcString "fun void(x : unit) : int = 2 + 2 in void () ") (Eval [] (Value 4)),
      Test "dystopia" (SrcFile "test6.pp6") (Eval [5] (Value 1984)),
      Test "animals" (SrcFile "test12.pp6") (Eval [10000] (Value 1917)),
      Test "pairs" (SrcFile "test8.pp6") (Eval [11] (Value 111)),
      Test "recursion" (SrcFile "test9.pp6") (Eval [10] (Value 55)),
      Test "pairsFib" (SrcFile "test10.pp6") (Eval [3] (Value 2)),
      Test "almostInfinity" (SrcFile "test11.pp6") (Eval [0] (Value 0)),
      Test "unitListLength" (SrcFile "test14.pp6") (Eval [10] (Value 4)),   --ten test i ponizszy byly znacznie ladniejsze, ale ten jezyk jest jakis dziwny i wyrzucal bledy tam, gdzie nie powinien
      Test "trueValues" (SrcFile "test15.pp6") (Eval [7] (Value 1)),
      Test "allGoodFun" (SrcFile "test16.pp6") (Eval [4, 8, 15, 16, 23, 42] (Value 2559)),
      Test "funcToFunc" (SrcFile "test18.pp6") (Eval [] (Value 0)),
      Test "multiNesting" (SrcFile "test19.pp6") (Eval [] (Value 21)),
      Test "reversedNesting" (SrcFile "test20.pp6") (Eval [4, 8, 15, 16, 23, 42] (Value 1)),
      Test "letLambdaInt" (SrcFile "test21.pp6") (Eval [8,4,2] (Value 20)),
      Test "letLambdaMixed" (SrcFile "test22.pp6") (Eval [9,7] (Value 15)),
      Test "letLambdaAdvncd" (SrcFile "test23.pp6") (Eval [11, 12] (Value 11)),
      Test "unusedLambda" (SrcFile "test24.pp6") (Eval [13] (Value 125)),
      Test "lambdaInFuncts" (SrcFile "test25.pp6") (Eval [2] (Value 0)),
      Test "returnLambda" (SrcFile "test28.pp6") (Eval [(-1)] (Value 1)),
      Test "nestedLambda" (SrcString "let fun1 = fn(x : bool) -> fn (y: int) -> fn (z: int) -> if x then y * z else y - z in fun1(true)(3)(4)") (Eval [] (Value 12)),
      Test "exprApplication" (SrcFile "test27.pp6") (Eval [11] (Value 121)),
      Test "idRepetition" (SrcString "fun f(x : int) : (int * int) = (x, x) input x in let f = x in fst f(f)") TypeError,
      Test "lbdCallOutOfRg" (SrcFile "test26.pp6") TypeError,
      Test "lbdWrongArgVal" (SrcString "let fun1 = fn(x : bool) -> if x then 3 else 5 in fun1(5)") TypeError,
      Test "lbdWrongRetVal" (SrcString "let fun1 = fn(x : int) -> if x > 0 then true else false in fun1 10") TypeError,
      Test "wrongUsedLocal" (SrcString "let fun1 = fn(x : int) -> x in x") TypeError,
      Test "lambdaValOfPr" (SrcString "fn(x: int) -> x + 2 + 3 - 4 - 15") TypeError,
      Test "pairValOfPr" (SrcString "(12, true)") TypeError,
      Test "listValOfPr" (SrcString "[1,2,3]:int list") TypeError,
      Test "unitValOfPr" (SrcString "()") TypeError
    ]


-- mam tu troche testow z prac4 (w tym poprawione), bo po prostu tu pasuja
-- idea jest taka, ze TypeErrory maja byc krotkie, ale konkretne (reagowac na kazdy blad wyprowadzenia z punktu widzenia interpretera). mam nadzieje, ze to dobra droga