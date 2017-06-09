module Main exposing (..)

import Data.Integer exposing (..)
import Expect
import Fuzz exposing (Fuzzer, conditional, int, tuple)
import Test exposing (..)
import Maybe exposing (Maybe)


integer : Fuzzer Integer
integer =
    int |> Fuzz.map fromInt


nonZeroInteger : Fuzzer Integer
nonZeroInteger =
    conditional { retries = 16, fallback = (+) 1, condition = (/=) 0 } int
        |> Fuzz.map fromInt


smallInt : Fuzzer Int
smallInt =
    conditional { retries = 16, fallback = always 0, condition = (>=) maxDigitValue << Basics.abs } int


addTests : Test
addTests =
    describe "addition"
        [ fuzz (tuple ( smallInt, smallInt )) "add x y = x + y for small numbers" <|
            \( x, y ) ->
                add (fromInt x) (fromInt y)
                    |> Expect.equal (fromInt (x + y))
        , fuzz (tuple ( integer, integer )) "a + b = b + a" <|
            \( a, b ) -> Expect.equal (add a b) (add b a)
        ]


negateTests : Test
negateTests =
    describe "negate"
        [ fuzz int "negate x = -x; x >= 0" <|
            \x ->
                let
                    y =
                        Basics.abs x
                in
                    fromInt y
                        |> Data.Integer.negate
                        |> Expect.equal (fromInt (-1 * y))
        , fuzz int "negate (-x) = x; x >= 0" <|
            \x ->
                let
                    y =
                        Basics.abs x * -1
                in
                    fromInt y
                        |> Data.Integer.negate
                        |> Expect.equal (fromInt (-1 * y))
        , fuzz integer "negate (negate x) = x" <|
            \a ->
                a
                    |> Data.Integer.negate
                    |> Data.Integer.negate
                    |> Expect.equal a
        ]


subTests : Test
subTests =
    describe "subtraction"
        [ fuzz (tuple ( integer, integer )) "x - y = x + -y" <|
            \( x, y ) ->
                Expect.equal (sub x y) (add x (Data.Integer.negate y))
        , fuzz (tuple ( integer, integer )) "a - b = -(b - a)" <|
            \( a, b ) -> Expect.equal (sub a b) (Data.Integer.negate (sub b a))
        ]


mulTests : Test
mulTests =
    describe "Mul testsuite"
        [ fuzz (tuple ( smallInt, smallInt )) "mult x y = x * y for small numbers" <|
            \( x, y ) ->
                mul (fromInt x) (fromInt y)
                    |> Expect.equal (fromInt (x * y))
        , fuzz (tuple ( integer, integer )) "Conmutative multiplication" <|
            \( a, b ) -> Expect.equal (mul a b) (mul b a)
        ]


divmodTests : Test
divmodTests =
    describe "divmod testsuite"
        [ test "divmod 2000000001 2 = Just (1000000000, 1)" <|
            \_ ->
                Expect.equal
                    (divmod (fromInt 2000000001) (fromInt 2))
                    (Just ( fromInt 1000000000, fromInt 1 ))
        , test "divmod 2000000002 2 = Just (1000000001, 0)" <|
            \_ ->
                Expect.equal
                    (divmod (fromInt 2000000002) (fromInt 2))
                    (Just ( fromInt 1000000001, fromInt 0 ))
        , test "divmod 20 0 == Nothing" <|
            \_ ->
                Expect.equal
                    (divmod (fromInt 20) (fromInt 0))
                    Nothing
        ]


qcDivMod : Test
qcDivMod =
    describe "Quickcheck divmod"
        [ fuzz (tuple ( integer, nonZeroInteger ))
            "divmod definition"
          <|
            \( a, b ) ->
                let
                    ( c, r ) =
                        unsafeDivmod a b
                in
                    add (mul c b) r
                        |> Expect.equal a
        ]


qcAbs : Test
qcAbs =
    describe "Quickcheck abs"
        [ fuzz integer "abs is always positive" <|
            \a ->
                Expect.true "Expected |x| >= 0" (gte (Data.Integer.abs a) (fromInt 0))
        , fuzz integer "abs definition" <|
            \a ->
                if gte a (fromInt 0) then
                    Expect.true "Expected |x| = x; x >= 0" (eq (Data.Integer.abs a) a)
                else
                    Expect.true "Expected |x| = -x; x < 0" (eq (Data.Integer.abs a) (Data.Integer.negate a))
        ]


qcSign : Test
qcSign =
    describe "Quickcheck sign"
        [ fuzz integer "sign definition" <|
            \a ->
                if gte a zero then
                    Expect.equal (sign a) Positive
                else
                    Expect.equal (sign a) Negative
        ]


toStringTests : Test
toStringTests =
    describe "toString testsuite"
        [ test "toString 3345 = \"3345\"" <|
            \_ -> Expect.equal (Data.Integer.toString (fromInt 3345)) "3345"
        , test "toString -3345 = \"-3345\"" <|
            \_ -> Expect.equal (Data.Integer.toString (fromInt (-3345))) "-3345"
        ]


fromStringTests : Test
fromStringTests =
    describe "fromString testsuite"
        [ test "fromString \"1\" = Just 1" <| \_ -> Expect.equal (Just (fromInt 1)) (fromString "1")
        , test "fromString \"-1\" = Just -1" <| \_ -> Expect.equal (Just (fromInt -1)) (fromString "-1")
        , test "fromString \"a\" = Nothing" <| \_ -> Expect.equal (fromString "-a") Nothing
        , test "fromString \"1234567890\" = Just 1234567890" <|
            \_ -> Expect.equal (fromString "1234567890") (Just (fromInt 1234567890))
        , test "fromString \"+1234567890\" = 1234567890" <|
            \_ -> Expect.equal (fromString "+1234567890") (Just (fromInt 1234567890))
        ]


minMaxTests : Test
minMaxTests =
    describe "min-max testsuite"
        [ test "max 1234567890 3 = 1234567890" <|
            \_ ->
                Expect.equal (Data.Integer.max (fromInt 1234567890) (fromInt 3)) (fromInt 1234567890)
        , test "max 1 3 = 3" <| \_ -> Expect.equal (Data.Integer.max (fromInt 1) (fromInt 3)) (fromInt 3)
        , test "min 1234567890 3 = 3" <|
            \_ -> Expect.equal (Data.Integer.min (fromInt 1234567890) (fromInt 3)) (fromInt 3)
        , test "min 1 3 = 1" <| \_ -> Expect.equal (Data.Integer.min (fromInt 1) (fromInt 3)) (fromInt 1)
        ]


maxDigitValueTests : Test
maxDigitValueTests =
    describe "maxDigitValue testsuite"
        [ test "maxDigitValue ^ 2 /= 1 + maxDigitValue ^ 2" <|
            \_ ->
                Expect.notEqual (maxDigitValue * maxDigitValue) ((maxDigitValue * maxDigitValue) + 1)
        ]


compareTests : Test
compareTests =
    describe "compare testsuite"
        [ test "compare 1234567890 3 = GT" <|
            \_ -> Expect.equal (Data.Integer.compare (fromInt 1234567890) (fromInt 3)) GT
        , test "compare 3 3 = EQ" <|
            \_ -> Expect.equal (Data.Integer.compare (fromInt 3) (fromInt 3)) EQ
        , test "compare 1 3 = LT" <|
            \_ -> Expect.equal (Data.Integer.compare (fromInt 1) (fromInt 3)) LT
        , test "1234567890 > 3" <|
            \_ -> Expect.equal (gt (fromInt 1234567890) (fromInt 3)) True
        , test "not ( 3 > 3 )" <|
            \_ -> Expect.equal (gt (fromInt 3) (fromInt 3)) False
        , test "not ( 1 > 3 )" <|
            \_ -> Expect.equal (gt (fromInt 1) (fromInt 3)) False
        , test "1234567890 >= 3" <|
            \_ -> Expect.equal (gte (fromInt 1234567890) (fromInt 3)) True
        , test "3 >= 3" <|
            \_ -> Expect.equal (gte (fromInt 3) (fromInt 3)) True
        , test "not ( 1 >= 3 )" <|
            \_ -> Expect.equal (gte (fromInt 1) (fromInt 3)) False
        , test "not ( 1234567890 = 3 )" <|
            \_ -> Expect.equal (eq (fromInt 1234567890) (fromInt 3)) False
        , test "3 = 3" <|
            \_ -> Expect.equal (eq (fromInt 3) (fromInt 3)) True
        , test "not ( 1234567890 < 3 )" <|
            \_ -> Expect.equal (lt (fromInt 1234567890) (fromInt 3)) False
        , test "not ( 3 < 3 )" <|
            \_ -> Expect.equal (lt (fromInt 3) (fromInt 3)) False
        , test "1 < 3" <|
            \_ -> Expect.equal (lt (fromInt 1) (fromInt 3)) True
        , test "not ( 1234567890 <= 3 )" <|
            \_ -> Expect.equal (lte (fromInt 1234567890) (fromInt 3)) False
        , test "3 <= 3" <|
            \_ -> Expect.equal (lte (fromInt 3) (fromInt 3)) True
        , test "1 <= 3" <|
            \_ -> Expect.equal (lte (fromInt 1) (fromInt 3)) True
        ]
