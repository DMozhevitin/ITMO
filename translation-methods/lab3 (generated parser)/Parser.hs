{-# OPTIONS_GHC -w #-}
module Parser where
import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13
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
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,189) ([57344,57343,1,16384,0,0,0,65280,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3968,512,15872,2048,63488,8192,57344,32771,0,0,0,65472,0,65280,3,0,0,0,0,15872,2048,63488,8192,57344,32771,32768,15,2,62,8,248,32,0,128,0,0,65024,2303,0,0,57344,36863,32768,15,2,62,8,248,32,992,128,3968,512,15872,2048,0,1023,0,4092,32768,15,2,62,8,248,32,992,128,65408,1855,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,65504,479,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Root","If","Statement","Print","Expr","Logic","LogicUnary","Arithm","Cmp","Unary","'+'","'-'","'*'","'/'","cnst","'|'","'&'","'True'","'False'","'>'","'<'","'<='","'>='","'=='","'!='","'if'","'else'","'='","var","'print'","%eof"]
        bit_start = st * 34
        bit_end = (st + 1) * 34
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..33]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (14) = happyShift action_13
action_0 (15) = happyShift action_14
action_0 (16) = happyShift action_15
action_0 (17) = happyShift action_16
action_0 (18) = happyShift action_17
action_0 (19) = happyShift action_18
action_0 (20) = happyShift action_19
action_0 (21) = happyShift action_20
action_0 (22) = happyShift action_21
action_0 (23) = happyShift action_22
action_0 (24) = happyShift action_23
action_0 (25) = happyShift action_24
action_0 (26) = happyShift action_25
action_0 (27) = happyShift action_26
action_0 (28) = happyShift action_27
action_0 (29) = happyShift action_3
action_0 (31) = happyShift action_28
action_0 (32) = happyShift action_29
action_0 (33) = happyShift action_30
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (8) = happyGoto action_7
action_0 (9) = happyGoto action_8
action_0 (10) = happyGoto action_9
action_0 (11) = happyGoto action_10
action_0 (12) = happyGoto action_11
action_0 (13) = happyGoto action_12
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (29) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (19) = happyShift action_18
action_3 (20) = happyShift action_19
action_3 (21) = happyShift action_20
action_3 (22) = happyShift action_21
action_3 (23) = happyShift action_22
action_3 (24) = happyShift action_23
action_3 (25) = happyShift action_24
action_3 (26) = happyShift action_25
action_3 (27) = happyShift action_26
action_3 (28) = happyShift action_27
action_3 (9) = happyGoto action_45
action_3 (10) = happyGoto action_9
action_3 (12) = happyGoto action_11
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (34) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_2

action_6 _ = happyReduce_6

action_7 _ = happyReduce_7

action_8 _ = happyReduce_11

action_9 _ = happyReduce_15

action_10 _ = happyReduce_10

action_11 _ = happyReduce_12

action_12 _ = happyReduce_22

action_13 (14) = happyShift action_13
action_13 (15) = happyShift action_14
action_13 (16) = happyShift action_15
action_13 (17) = happyShift action_16
action_13 (18) = happyShift action_17
action_13 (32) = happyShift action_29
action_13 (11) = happyGoto action_44
action_13 (13) = happyGoto action_12
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (14) = happyShift action_13
action_14 (15) = happyShift action_14
action_14 (16) = happyShift action_15
action_14 (17) = happyShift action_16
action_14 (18) = happyShift action_17
action_14 (32) = happyShift action_29
action_14 (11) = happyGoto action_43
action_14 (13) = happyGoto action_12
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (14) = happyShift action_13
action_15 (15) = happyShift action_14
action_15 (16) = happyShift action_15
action_15 (17) = happyShift action_16
action_15 (18) = happyShift action_17
action_15 (32) = happyShift action_29
action_15 (11) = happyGoto action_42
action_15 (13) = happyGoto action_12
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (14) = happyShift action_13
action_16 (15) = happyShift action_14
action_16 (16) = happyShift action_15
action_16 (17) = happyShift action_16
action_16 (18) = happyShift action_17
action_16 (32) = happyShift action_29
action_16 (11) = happyGoto action_41
action_16 (13) = happyGoto action_12
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_29

action_18 (19) = happyShift action_18
action_18 (20) = happyShift action_19
action_18 (21) = happyShift action_20
action_18 (22) = happyShift action_21
action_18 (23) = happyShift action_22
action_18 (24) = happyShift action_23
action_18 (25) = happyShift action_24
action_18 (26) = happyShift action_25
action_18 (27) = happyShift action_26
action_18 (28) = happyShift action_27
action_18 (9) = happyGoto action_40
action_18 (10) = happyGoto action_9
action_18 (12) = happyGoto action_11
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (19) = happyShift action_18
action_19 (20) = happyShift action_19
action_19 (21) = happyShift action_20
action_19 (22) = happyShift action_21
action_19 (23) = happyShift action_22
action_19 (24) = happyShift action_23
action_19 (25) = happyShift action_24
action_19 (26) = happyShift action_25
action_19 (27) = happyShift action_26
action_19 (28) = happyShift action_27
action_19 (9) = happyGoto action_39
action_19 (10) = happyGoto action_9
action_19 (12) = happyGoto action_11
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_16

action_21 _ = happyReduce_17

action_22 (14) = happyShift action_13
action_22 (15) = happyShift action_14
action_22 (16) = happyShift action_15
action_22 (17) = happyShift action_16
action_22 (18) = happyShift action_17
action_22 (32) = happyShift action_29
action_22 (11) = happyGoto action_38
action_22 (13) = happyGoto action_12
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (14) = happyShift action_13
action_23 (15) = happyShift action_14
action_23 (16) = happyShift action_15
action_23 (17) = happyShift action_16
action_23 (18) = happyShift action_17
action_23 (32) = happyShift action_29
action_23 (11) = happyGoto action_37
action_23 (13) = happyGoto action_12
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (14) = happyShift action_13
action_24 (15) = happyShift action_14
action_24 (16) = happyShift action_15
action_24 (17) = happyShift action_16
action_24 (18) = happyShift action_17
action_24 (32) = happyShift action_29
action_24 (11) = happyGoto action_36
action_24 (13) = happyGoto action_12
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (14) = happyShift action_13
action_25 (15) = happyShift action_14
action_25 (16) = happyShift action_15
action_25 (17) = happyShift action_16
action_25 (18) = happyShift action_17
action_25 (32) = happyShift action_29
action_25 (11) = happyGoto action_35
action_25 (13) = happyGoto action_12
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (14) = happyShift action_13
action_26 (15) = happyShift action_14
action_26 (16) = happyShift action_15
action_26 (17) = happyShift action_16
action_26 (18) = happyShift action_17
action_26 (32) = happyShift action_29
action_26 (11) = happyGoto action_34
action_26 (13) = happyGoto action_12
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (14) = happyShift action_13
action_27 (15) = happyShift action_14
action_27 (16) = happyShift action_15
action_27 (17) = happyShift action_16
action_27 (18) = happyShift action_17
action_27 (32) = happyShift action_29
action_27 (11) = happyGoto action_33
action_27 (13) = happyGoto action_12
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (32) = happyShift action_32
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_30

action_30 (14) = happyShift action_13
action_30 (15) = happyShift action_14
action_30 (16) = happyShift action_15
action_30 (17) = happyShift action_16
action_30 (18) = happyShift action_17
action_30 (19) = happyShift action_18
action_30 (20) = happyShift action_19
action_30 (21) = happyShift action_20
action_30 (22) = happyShift action_21
action_30 (23) = happyShift action_22
action_30 (24) = happyShift action_23
action_30 (25) = happyShift action_24
action_30 (26) = happyShift action_25
action_30 (27) = happyShift action_26
action_30 (28) = happyShift action_27
action_30 (32) = happyShift action_29
action_30 (8) = happyGoto action_31
action_30 (9) = happyGoto action_8
action_30 (10) = happyGoto action_9
action_30 (11) = happyGoto action_10
action_30 (12) = happyGoto action_11
action_30 (13) = happyGoto action_12
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_9

action_32 (14) = happyShift action_13
action_32 (15) = happyShift action_14
action_32 (16) = happyShift action_15
action_32 (17) = happyShift action_16
action_32 (18) = happyShift action_17
action_32 (19) = happyShift action_18
action_32 (20) = happyShift action_19
action_32 (21) = happyShift action_20
action_32 (22) = happyShift action_21
action_32 (23) = happyShift action_22
action_32 (24) = happyShift action_23
action_32 (25) = happyShift action_24
action_32 (26) = happyShift action_25
action_32 (27) = happyShift action_26
action_32 (28) = happyShift action_27
action_32 (32) = happyShift action_29
action_32 (8) = happyGoto action_59
action_32 (9) = happyGoto action_8
action_32 (10) = happyGoto action_9
action_32 (11) = happyGoto action_10
action_32 (12) = happyGoto action_11
action_32 (13) = happyGoto action_12
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (14) = happyShift action_13
action_33 (15) = happyShift action_14
action_33 (16) = happyShift action_15
action_33 (17) = happyShift action_16
action_33 (18) = happyShift action_17
action_33 (32) = happyShift action_29
action_33 (11) = happyGoto action_58
action_33 (13) = happyGoto action_12
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (14) = happyShift action_13
action_34 (15) = happyShift action_14
action_34 (16) = happyShift action_15
action_34 (17) = happyShift action_16
action_34 (18) = happyShift action_17
action_34 (32) = happyShift action_29
action_34 (11) = happyGoto action_57
action_34 (13) = happyGoto action_12
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (14) = happyShift action_13
action_35 (15) = happyShift action_14
action_35 (16) = happyShift action_15
action_35 (17) = happyShift action_16
action_35 (18) = happyShift action_17
action_35 (32) = happyShift action_29
action_35 (11) = happyGoto action_56
action_35 (13) = happyGoto action_12
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (14) = happyShift action_13
action_36 (15) = happyShift action_14
action_36 (16) = happyShift action_15
action_36 (17) = happyShift action_16
action_36 (18) = happyShift action_17
action_36 (32) = happyShift action_29
action_36 (11) = happyGoto action_55
action_36 (13) = happyGoto action_12
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (14) = happyShift action_13
action_37 (15) = happyShift action_14
action_37 (16) = happyShift action_15
action_37 (17) = happyShift action_16
action_37 (18) = happyShift action_17
action_37 (32) = happyShift action_29
action_37 (11) = happyGoto action_54
action_37 (13) = happyGoto action_12
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (14) = happyShift action_13
action_38 (15) = happyShift action_14
action_38 (16) = happyShift action_15
action_38 (17) = happyShift action_16
action_38 (18) = happyShift action_17
action_38 (32) = happyShift action_29
action_38 (11) = happyGoto action_53
action_38 (13) = happyGoto action_12
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (19) = happyShift action_18
action_39 (20) = happyShift action_19
action_39 (21) = happyShift action_20
action_39 (22) = happyShift action_21
action_39 (23) = happyShift action_22
action_39 (24) = happyShift action_23
action_39 (25) = happyShift action_24
action_39 (26) = happyShift action_25
action_39 (27) = happyShift action_26
action_39 (28) = happyShift action_27
action_39 (9) = happyGoto action_52
action_39 (10) = happyGoto action_9
action_39 (12) = happyGoto action_11
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (19) = happyShift action_18
action_40 (20) = happyShift action_19
action_40 (21) = happyShift action_20
action_40 (22) = happyShift action_21
action_40 (23) = happyShift action_22
action_40 (24) = happyShift action_23
action_40 (25) = happyShift action_24
action_40 (26) = happyShift action_25
action_40 (27) = happyShift action_26
action_40 (28) = happyShift action_27
action_40 (9) = happyGoto action_51
action_40 (10) = happyGoto action_9
action_40 (12) = happyGoto action_11
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (14) = happyShift action_13
action_41 (15) = happyShift action_14
action_41 (16) = happyShift action_15
action_41 (17) = happyShift action_16
action_41 (18) = happyShift action_17
action_41 (32) = happyShift action_29
action_41 (11) = happyGoto action_50
action_41 (13) = happyGoto action_12
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (14) = happyShift action_13
action_42 (15) = happyShift action_14
action_42 (16) = happyShift action_15
action_42 (17) = happyShift action_16
action_42 (18) = happyShift action_17
action_42 (32) = happyShift action_29
action_42 (11) = happyGoto action_49
action_42 (13) = happyGoto action_12
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (14) = happyShift action_13
action_43 (15) = happyShift action_14
action_43 (16) = happyShift action_15
action_43 (17) = happyShift action_16
action_43 (18) = happyShift action_17
action_43 (32) = happyShift action_29
action_43 (11) = happyGoto action_48
action_43 (13) = happyGoto action_12
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (14) = happyShift action_13
action_44 (15) = happyShift action_14
action_44 (16) = happyShift action_15
action_44 (17) = happyShift action_16
action_44 (18) = happyShift action_17
action_44 (32) = happyShift action_29
action_44 (11) = happyGoto action_47
action_44 (13) = happyGoto action_12
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (14) = happyShift action_13
action_45 (15) = happyShift action_14
action_45 (16) = happyShift action_15
action_45 (17) = happyShift action_16
action_45 (18) = happyShift action_17
action_45 (19) = happyShift action_18
action_45 (20) = happyShift action_19
action_45 (21) = happyShift action_20
action_45 (22) = happyShift action_21
action_45 (23) = happyShift action_22
action_45 (24) = happyShift action_23
action_45 (25) = happyShift action_24
action_45 (26) = happyShift action_25
action_45 (27) = happyShift action_26
action_45 (28) = happyShift action_27
action_45 (31) = happyShift action_28
action_45 (32) = happyShift action_29
action_45 (33) = happyShift action_30
action_45 (6) = happyGoto action_46
action_45 (7) = happyGoto action_6
action_45 (8) = happyGoto action_7
action_45 (9) = happyGoto action_8
action_45 (10) = happyGoto action_9
action_45 (11) = happyGoto action_10
action_45 (12) = happyGoto action_11
action_45 (13) = happyGoto action_12
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (30) = happyShift action_60
action_46 _ = happyReduce_5

action_47 _ = happyReduce_18

action_48 _ = happyReduce_19

action_49 _ = happyReduce_20

action_50 _ = happyReduce_21

action_51 _ = happyReduce_14

action_52 _ = happyReduce_13

action_53 _ = happyReduce_24

action_54 _ = happyReduce_23

action_55 _ = happyReduce_26

action_56 _ = happyReduce_25

action_57 _ = happyReduce_27

action_58 _ = happyReduce_28

action_59 _ = happyReduce_8

action_60 (14) = happyShift action_13
action_60 (15) = happyShift action_14
action_60 (16) = happyShift action_15
action_60 (17) = happyShift action_16
action_60 (18) = happyShift action_17
action_60 (19) = happyShift action_18
action_60 (20) = happyShift action_19
action_60 (21) = happyShift action_20
action_60 (22) = happyShift action_21
action_60 (23) = happyShift action_22
action_60 (24) = happyShift action_23
action_60 (25) = happyShift action_24
action_60 (26) = happyShift action_25
action_60 (27) = happyShift action_26
action_60 (28) = happyShift action_27
action_60 (29) = happyShift action_3
action_60 (31) = happyShift action_28
action_60 (32) = happyShift action_29
action_60 (33) = happyShift action_30
action_60 (5) = happyGoto action_61
action_60 (6) = happyGoto action_62
action_60 (7) = happyGoto action_6
action_60 (8) = happyGoto action_7
action_60 (9) = happyGoto action_8
action_60 (10) = happyGoto action_9
action_60 (11) = happyGoto action_10
action_60 (12) = happyGoto action_11
action_60 (13) = happyGoto action_12
action_60 _ = happyFail (happyExpListPerState 60)

action_61 _ = happyReduce_3

action_62 _ = happyReduce_4

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (RootIf happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (RootSt happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happyReduce 5 5 happyReduction_3
happyReduction_3 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (IfElifExpr happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 5 5 happyReduction_4
happyReduction_4 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (IfExpr happy_var_2 happy_var_3 (Just happy_var_5)
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (IfExpr happy_var_2 happy_var_3 Nothing
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (StPrint  happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 (StExpr   happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_3)
	(HappyTerminal (TVar happy_var_2))
	_
	 =  HappyAbsSyn6
		 (StAssign happy_var_2 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  7 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Print happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn8
		 (Arithmetic happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  8 happyReduction_11
happyReduction_11 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (Logic  happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  9 happyReduction_12
happyReduction_12 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  9 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2 :&: happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  9 happyReduction_14
happyReduction_14 (HappyAbsSyn9  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2 :|: happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  9 happyReduction_15
happyReduction_15 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn10
		 (BoolValue True
	)

happyReduce_17 = happySpecReduce_1  10 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn10
		 (BoolValue False
	)

happyReduce_18 = happySpecReduce_3  11 happyReduction_18
happyReduction_18 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2 :+: happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  11 happyReduction_19
happyReduction_19 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2 :-: happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  11 happyReduction_20
happyReduction_20 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2 :*: happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  11 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2 :/: happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  11 happyReduction_22
happyReduction_22 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  12 happyReduction_23
happyReduction_23 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2 :<: happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  12 happyReduction_24
happyReduction_24 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2 :>: happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  12 happyReduction_25
happyReduction_25 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2 :>=: happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  12 happyReduction_26
happyReduction_26 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2 :<=: happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  12 happyReduction_27
happyReduction_27 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2 :==: happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  12 happyReduction_28
happyReduction_28 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2 :!=: happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  13 happyReduction_29
happyReduction_29 (HappyTerminal (TConst happy_var_1))
	 =  HappyAbsSyn13
		 (Const happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  13 happyReduction_30
happyReduction_30 (HappyTerminal (TVar happy_var_1))
	 =  HappyAbsSyn13
		 (Var   happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 34 34 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TPlus -> cont 14;
	TMinus -> cont 15;
	TMul -> cont 16;
	TDiv -> cont 17;
	TConst happy_dollar_dollar -> cont 18;
	TOr -> cont 19;
	TAnd -> cont 20;
	TTrue -> cont 21;
	TFalse -> cont 22;
	TG -> cont 23;
	TL -> cont 24;
	TLE -> cont 25;
	TGE -> cont 26;
	TEQ -> cont 27;
	TNEQ -> cont 28;
	TIF -> cont 29;
	TELSE -> cont 30;
	TAssign -> cont 31;
	TVar happy_dollar_dollar -> cont 32;
	TPrint -> cont 33;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 34 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data IfExpr = IfExpr LogicExpr Statement (Maybe Statement)
            | IfElifExpr LogicExpr Statement IfExpr
            deriving (Eq)

data Statement = StPrint Print
               | StExpr Expr
               | StAssign String Expr
               deriving (Eq)

data Print = Print Expr
           deriving (Eq)

data ArithmExpr = ArithmExpr :+: ArithmExpr 
                | ArithmExpr :-: ArithmExpr 
                | ArithmExpr :*: ArithmExpr 
                | ArithmExpr :/: ArithmExpr 
                | Const Int 
                | Var String
                deriving (Eq)

data LogicExpr = LogicExpr :&: LogicExpr
               | LogicExpr :|: LogicExpr
               | BoolValue Bool 
               | ArithmExpr :<: ArithmExpr
               | ArithmExpr :>: ArithmExpr
               | ArithmExpr :>=: ArithmExpr
               | ArithmExpr :<=: ArithmExpr
               | ArithmExpr :==: ArithmExpr
               | ArithmExpr :!=: ArithmExpr
               deriving (Eq)

data Expr     = Arithmetic ArithmExpr
              | Logic LogicExpr
              deriving (Eq)

data Root = RootSt Statement
         | RootIf IfExpr
         deriving (Eq)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































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









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

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
