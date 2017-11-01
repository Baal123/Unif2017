{-# OPTIONS_GHC -w #-}
module Parser.NlasPreParser where
import Data.Char
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11

action_0 (12) = happyShift action_7
action_0 (16) = happyShift action_8
action_0 (18) = happyShift action_9
action_0 (20) = happyShift action_10
action_0 (21) = happyShift action_11
action_0 (4) = happyGoto action_12
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (9) = happyGoto action_4
action_0 (10) = happyGoto action_5
action_0 (11) = happyGoto action_6
action_0 _ = happyReduce_4

action_1 (12) = happyShift action_7
action_1 (16) = happyShift action_8
action_1 (18) = happyShift action_9
action_1 (20) = happyShift action_10
action_1 (21) = happyShift action_11
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (9) = happyGoto action_4
action_1 (10) = happyGoto action_5
action_1 (11) = happyGoto action_6
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (13) = happyShift action_25
action_3 _ = happyFail

action_4 (12) = happyShift action_7
action_4 (16) = happyShift action_8
action_4 (18) = happyShift action_9
action_4 (20) = happyShift action_10
action_4 (21) = happyShift action_11
action_4 (6) = happyGoto action_24
action_4 (9) = happyGoto action_4
action_4 (10) = happyGoto action_16
action_4 (11) = happyGoto action_6
action_4 _ = happyFail

action_5 (14) = happyShift action_23
action_5 _ = happyReduce_9

action_6 _ = happyReduce_10

action_7 _ = happyReduce_18

action_8 (12) = happyShift action_7
action_8 (16) = happyShift action_8
action_8 (18) = happyShift action_9
action_8 (20) = happyShift action_10
action_8 (21) = happyShift action_11
action_8 (6) = happyGoto action_21
action_8 (9) = happyGoto action_4
action_8 (10) = happyGoto action_22
action_8 (11) = happyGoto action_6
action_8 _ = happyFail

action_9 (12) = happyShift action_7
action_9 (16) = happyShift action_20
action_9 (8) = happyGoto action_17
action_9 (9) = happyGoto action_18
action_9 (10) = happyGoto action_19
action_9 _ = happyFail

action_10 (12) = happyShift action_7
action_10 (16) = happyShift action_8
action_10 (18) = happyShift action_9
action_10 (20) = happyShift action_10
action_10 (21) = happyShift action_11
action_10 (6) = happyGoto action_14
action_10 (7) = happyGoto action_15
action_10 (9) = happyGoto action_4
action_10 (10) = happyGoto action_16
action_10 (11) = happyGoto action_6
action_10 _ = happyReduce_13

action_11 _ = happyReduce_19

action_12 (22) = happyShift action_13
action_12 (23) = happyAccept
action_12 _ = happyFail

action_13 (12) = happyShift action_7
action_13 (16) = happyShift action_8
action_13 (18) = happyShift action_9
action_13 (20) = happyShift action_10
action_13 (21) = happyShift action_11
action_13 (5) = happyGoto action_34
action_13 (6) = happyGoto action_3
action_13 (9) = happyGoto action_4
action_13 (10) = happyGoto action_5
action_13 (11) = happyGoto action_6
action_13 _ = happyReduce_3

action_14 (12) = happyShift action_7
action_14 (16) = happyShift action_8
action_14 (18) = happyShift action_9
action_14 (20) = happyShift action_10
action_14 (21) = happyShift action_11
action_14 (6) = happyGoto action_14
action_14 (7) = happyGoto action_33
action_14 (9) = happyGoto action_4
action_14 (10) = happyGoto action_16
action_14 (11) = happyGoto action_6
action_14 _ = happyReduce_13

action_15 _ = happyReduce_12

action_16 _ = happyReduce_9

action_17 (19) = happyShift action_32
action_17 _ = happyFail

action_18 (12) = happyShift action_7
action_18 (16) = happyShift action_20
action_18 (8) = happyGoto action_31
action_18 (9) = happyGoto action_18
action_18 (10) = happyGoto action_19
action_18 _ = happyFail

action_19 _ = happyReduce_16

action_20 (12) = happyShift action_7
action_20 (10) = happyGoto action_30
action_20 _ = happyFail

action_21 (17) = happyShift action_29
action_21 _ = happyFail

action_22 (15) = happyShift action_28
action_22 _ = happyReduce_9

action_23 (12) = happyShift action_7
action_23 (16) = happyShift action_8
action_23 (18) = happyShift action_9
action_23 (20) = happyShift action_10
action_23 (21) = happyShift action_11
action_23 (6) = happyGoto action_27
action_23 (9) = happyGoto action_4
action_23 (10) = happyGoto action_16
action_23 (11) = happyGoto action_6
action_23 _ = happyFail

action_24 _ = happyReduce_7

action_25 (12) = happyShift action_7
action_25 (16) = happyShift action_8
action_25 (18) = happyShift action_9
action_25 (20) = happyShift action_10
action_25 (21) = happyShift action_11
action_25 (6) = happyGoto action_26
action_25 (9) = happyGoto action_4
action_25 (10) = happyGoto action_16
action_25 (11) = happyGoto action_6
action_25 _ = happyFail

action_26 _ = happyReduce_5

action_27 _ = happyReduce_6

action_28 (12) = happyShift action_7
action_28 (10) = happyGoto action_36
action_28 _ = happyFail

action_29 _ = happyReduce_11

action_30 (15) = happyShift action_28
action_30 _ = happyFail

action_31 _ = happyReduce_15

action_32 (12) = happyShift action_7
action_32 (16) = happyShift action_8
action_32 (18) = happyShift action_9
action_32 (20) = happyShift action_10
action_32 (21) = happyShift action_11
action_32 (6) = happyGoto action_35
action_32 (9) = happyGoto action_4
action_32 (10) = happyGoto action_16
action_32 (11) = happyGoto action_6
action_32 _ = happyFail

action_33 _ = happyReduce_14

action_34 _ = happyReduce_2

action_35 _ = happyReduce_8

action_36 (17) = happyShift action_37
action_36 _ = happyFail

action_37 _ = happyReduce_17

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_1 _  = notHappyAtAll

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_3:happy_var_1
	)
happyReduction_2 _ _ _  = notHappyAtAll

happyReduce_3 = happySpecReduce_2  4 happyReduction_3
happyReduction_3 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll

happyReduce_4 = happySpecReduce_0  4 happyReduction_4
happyReduction_4  =  HappyAbsSyn4
		 ([]
	)

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn5
		 (Fresh happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll

happyReduce_7 = happySpecReduce_2  6 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 (Apply happy_var_1 happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll

happyReduce_8 = happyReduce 4 6 happyReduction_8
happyReduction_8 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Lam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  6 happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn6
		 (ExAtom happy_var_1
	)
happyReduction_9 _  = notHappyAtAll

happyReduce_10 = happySpecReduce_1  6 happyReduction_10
happyReduction_10 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn6
		 (ExVar happy_var_1
	)
happyReduction_10 _  = notHappyAtAll

happyReduce_11 = happySpecReduce_3  6 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_11 _ _ _  = notHappyAtAll

happyReduce_12 = happySpecReduce_2  6 happyReduction_12
happyReduction_12 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal (TokenFn happy_var_1))
	 =  HappyAbsSyn6
		 (Fn happy_var_1 happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll

happyReduce_13 = happySpecReduce_0  7 happyReduction_13
happyReduction_13  =  HappyAbsSyn7
		 ([]
	)

happyReduce_14 = happySpecReduce_2  7 happyReduction_14
happyReduction_14 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1:happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll

happyReduce_15 = happySpecReduce_2  8 happyReduction_15
happyReduction_15 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (BaseApply happy_var_1 happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll

happyReduce_16 = happySpecReduce_1  8 happyReduction_16
happyReduction_16 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (BExAtom happy_var_1
	)
happyReduction_16 _  = notHappyAtAll

happyReduce_17 = happyReduce 5 9 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Swap happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_1  10 happyReduction_18
happyReduction_18 (HappyTerminal (TokenAt happy_var_1))
	 =  HappyAbsSyn10
		 (At happy_var_1
	)
happyReduction_18 _  = notHappyAtAll

happyReduce_19 = happySpecReduce_1  11 happyReduction_19
happyReduction_19 (HappyTerminal (TokenEx happy_var_1))
	 =  HappyAbsSyn11
		 (ExN happy_var_1
	)
happyReduction_19 _  = notHappyAtAll

happyNewToken action sts stk [] =
	action 23 23 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenAt happy_dollar_dollar -> cont 12;
	TokenEq -> cont 13;
	TokenFresh -> cont 14;
	TokenComma -> cont 15;
	TokenOB -> cont 16;
	TokenCB -> cont 17;
	TokenLam -> cont 18;
	TokenTo -> cont 19;
	TokenFn happy_dollar_dollar -> cont 20;
	TokenEx happy_dollar_dollar -> cont 21;
	TokenSep -> cont 22;
	_ -> happyError' (tk:tks)
	}

happyError_ 23 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

calc tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError tokens = error ("Remaing Tokens: " ++ (show tokens))

data UpEntry
      = Eq Exp Exp
      | Fresh Atom Exp
      deriving Show

data Exp
      = Apply Swap Exp
      | ExAtom Atom
      | ExVar ExpressionName
      | Lam BExp Exp
      | Fn String [Exp]
      deriving Show

data BExp
      = BaseApply Swap BExp
      | BExAtom Atom
      deriving Show

data Swap
      = Swap Atom Atom
      deriving Show

data Atom
      = At String
      deriving Show

data FunctionName
      = FnN String
      deriving Show

data ExpressionName
      = ExN String
      deriving Show

data Token
      = TokenAt String
      | TokenEq
      | TokenFresh
      | TokenComma
      | TokenOB
      | TokenCB
      | TokenLam
      | TokenTo
      | TokenFn String
      | TokenEx String
      | TokenSep
      deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSep c = TokenSep : lexer cs
      | isSpace c = lexer cs
      | isAlpha c = lexName (c:cs)
      | c == '$' = lexName (c:cs)

lexer ('=':cs) = TokenEq : lexer cs
lexer ('#':cs) = TokenFresh : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer ('\\':cs) = TokenLam : lexer cs
lexer ('-':'>':cs) = TokenTo : lexer cs

isSep ';' = True
isSep '\r' = True
isSep '\n' = True
isSep c = False

lexName (c:cs)
    | isLower c = TokenAt var : lexer rest
    | isUpper c = TokenEx var : lexer rest
    | c == '$' && isAlpha (head cs) = TokenFn var : lexer rest
     where
        (var, rest) | c == '$' = span isNameChar cs | otherwise = span isNameChar (c:cs)
        isNameChart '_' = True
        isNameChar = isAlphaNum

parse = calc . lexer
main = do
          inp <- readFile "Input.up"
          let res = parse inp in
             putStr (show res)

{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 10 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc9827_0/ghc_2.h" #-}


































































































































































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

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

{-# LINE 155 "templates/GenericTemplate.hs" #-}

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
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $
        happyError_ i tk

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
happyFail  i tk (HappyState (action)) sts stk =
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

{-# LINE 322 "templates/GenericTemplate.hs" #-}
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
