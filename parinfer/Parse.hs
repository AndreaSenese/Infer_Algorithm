{-# OPTIONS_GHC -w #-}
module Parse where
import Lex
import Term
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t8 t9 t10 t11
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn5 ([(VarId,Term)])
	| HappyAbsSyn6 ((VarId,Term))
	| HappyAbsSyn7 (Term)
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,79) ([26624,160,256,0,0,0,0,64,0,4,0,0,384,0,24,33152,0,0,4096,0,0,0,0,1664,10,16,32768,0,0,1,16,0,0,8288,32768,129,1536,2,2072,26624,160,0,0,4096,0,6,6144,32768,129,1536,2,10266,0,0,33184,2,0,1024,0,0,40960,641,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseExp","%start_parseBinds","binds","bind","Exp","Exp1","Term","App","Atom","let","in","int","var","'->'","'='","'+'","'-'","'*'","'/'","'('","')'","'\\\\'","';'","%eof"]
        bit_start = st * 26
        bit_end = (st + 1) * 26
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..25]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (12) = happyShift action_11
action_0 (14) = happyShift action_12
action_0 (15) = happyShift action_13
action_0 (22) = happyShift action_14
action_0 (24) = happyShift action_15
action_0 (7) = happyGoto action_6
action_0 (8) = happyGoto action_7
action_0 (9) = happyGoto action_8
action_0 (10) = happyGoto action_9
action_0 (11) = happyGoto action_10
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (15) = happyShift action_5
action_1 (5) = happyGoto action_3
action_1 (6) = happyGoto action_4
action_1 _ = happyReduce_2

action_2 _ = happyFail (happyExpListPerState 2)

action_3 (26) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (15) = happyShift action_5
action_4 (5) = happyGoto action_25
action_4 (6) = happyGoto action_4
action_4 _ = happyReduce_2

action_5 (17) = happyShift action_24
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (26) = happyAccept
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (18) = happyShift action_22
action_7 (19) = happyShift action_23
action_7 _ = happyReduce_7

action_8 (20) = happyShift action_20
action_8 (21) = happyShift action_21
action_8 _ = happyReduce_10

action_9 (14) = happyShift action_12
action_9 (15) = happyShift action_13
action_9 (22) = happyShift action_14
action_9 (11) = happyGoto action_19
action_9 _ = happyReduce_13

action_10 _ = happyReduce_15

action_11 (15) = happyShift action_18
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_16

action_13 _ = happyReduce_17

action_14 (12) = happyShift action_11
action_14 (14) = happyShift action_12
action_14 (15) = happyShift action_13
action_14 (22) = happyShift action_14
action_14 (24) = happyShift action_15
action_14 (7) = happyGoto action_17
action_14 (8) = happyGoto action_7
action_14 (9) = happyGoto action_8
action_14 (10) = happyGoto action_9
action_14 (11) = happyGoto action_10
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (15) = happyShift action_16
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (16) = happyShift action_33
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (23) = happyShift action_32
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (17) = happyShift action_31
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_14

action_20 (14) = happyShift action_12
action_20 (15) = happyShift action_13
action_20 (22) = happyShift action_14
action_20 (10) = happyGoto action_30
action_20 (11) = happyGoto action_10
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (14) = happyShift action_12
action_21 (15) = happyShift action_13
action_21 (22) = happyShift action_14
action_21 (10) = happyGoto action_29
action_21 (11) = happyGoto action_10
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (14) = happyShift action_12
action_22 (15) = happyShift action_13
action_22 (22) = happyShift action_14
action_22 (9) = happyGoto action_28
action_22 (10) = happyGoto action_9
action_22 (11) = happyGoto action_10
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (14) = happyShift action_12
action_23 (15) = happyShift action_13
action_23 (22) = happyShift action_14
action_23 (9) = happyGoto action_27
action_23 (10) = happyGoto action_9
action_23 (11) = happyGoto action_10
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (12) = happyShift action_11
action_24 (14) = happyShift action_12
action_24 (15) = happyShift action_13
action_24 (22) = happyShift action_14
action_24 (24) = happyShift action_15
action_24 (7) = happyGoto action_26
action_24 (8) = happyGoto action_7
action_24 (9) = happyGoto action_8
action_24 (10) = happyGoto action_9
action_24 (11) = happyGoto action_10
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_3

action_26 (25) = happyShift action_36
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (20) = happyShift action_20
action_27 (21) = happyShift action_21
action_27 _ = happyReduce_9

action_28 (20) = happyShift action_20
action_28 (21) = happyShift action_21
action_28 _ = happyReduce_8

action_29 (14) = happyShift action_12
action_29 (15) = happyShift action_13
action_29 (22) = happyShift action_14
action_29 (11) = happyGoto action_19
action_29 _ = happyReduce_12

action_30 (14) = happyShift action_12
action_30 (15) = happyShift action_13
action_30 (22) = happyShift action_14
action_30 (11) = happyGoto action_19
action_30 _ = happyReduce_11

action_31 (12) = happyShift action_11
action_31 (14) = happyShift action_12
action_31 (15) = happyShift action_13
action_31 (22) = happyShift action_14
action_31 (24) = happyShift action_15
action_31 (7) = happyGoto action_35
action_31 (8) = happyGoto action_7
action_31 (9) = happyGoto action_8
action_31 (10) = happyGoto action_9
action_31 (11) = happyGoto action_10
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_18

action_33 (12) = happyShift action_11
action_33 (14) = happyShift action_12
action_33 (15) = happyShift action_13
action_33 (22) = happyShift action_14
action_33 (24) = happyShift action_15
action_33 (7) = happyGoto action_34
action_33 (8) = happyGoto action_7
action_33 (9) = happyGoto action_8
action_33 (10) = happyGoto action_9
action_33 (11) = happyGoto action_10
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_6

action_35 (13) = happyShift action_37
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_4

action_37 (12) = happyShift action_11
action_37 (14) = happyShift action_12
action_37 (15) = happyShift action_13
action_37 (22) = happyShift action_14
action_37 (24) = happyShift action_15
action_37 (7) = happyGoto action_38
action_37 (8) = happyGoto action_7
action_37 (9) = happyGoto action_8
action_37 (10) = happyGoto action_9
action_37 (11) = happyGoto action_10
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_5

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 4 6 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ((happy_var_1,happy_var_3)
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 6 7 happyReduction_5
happyReduction_5 ((HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 4 7 happyReduction_6
happyReduction_6 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Abs happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (App (App (Var "+") happy_var_1) happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (App (App (Var "-") happy_var_1) happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  9 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (App (App (Var "*") happy_var_1) happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  9 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (App (App (Var "/") happy_var_1) happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  9 happyReduction_13
happyReduction_13 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  10 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (App happy_var_1 happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  10 happyReduction_15
happyReduction_15 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  11 happyReduction_16
happyReduction_16 (HappyTerminal (TInt happy_var_1))
	 =  HappyAbsSyn11
		 (Int happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 (HappyTerminal (TVar happy_var_1))
	 =  HappyAbsSyn11
		 (Var happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  11 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 26 26 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TLet -> cont 12;
	TIn -> cont 13;
	TInt happy_dollar_dollar -> cont 14;
	TVar happy_dollar_dollar -> cont 15;
	TArrow -> cont 16;
	TSym '=' -> cont 17;
	TSym '+' -> cont 18;
	TSym '-' -> cont 19;
	TSym '*' -> cont 20;
	TSym '/' -> cont 21;
	TSym '(' -> cont 22;
	TSym ')' -> cont 23;
	TSym '\\' -> cont 24;
	TSym ';' -> cont 25;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 26 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Either String a -> (a -> Either String b) -> Either String b
happyThen = (>>=)
happyReturn :: () => a -> Either String a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either String a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> Either String a
happyError' = (\(tokens, _) -> happyError tokens)
parseExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

parseBinds tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [Token] -> Either String a
happyError [] = Left "Parse error at end of input"
happyError (tk:tks) = Left ("Parse error before " ++ show tk)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}







# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}















{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc7894_0/ghc_2.h" #-}




















































































































































































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
