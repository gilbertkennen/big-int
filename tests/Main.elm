module Main exposing (..)

import Data.Integer exposing (..)
import Expect
import Fuzz exposing (Fuzzer, conditional, int, tuple)
import String
import Test exposing (..)
import Maybe exposing (Maybe)


singleInteger : Fuzzer Integer
singleInteger =
    Fuzz.map fromInt int


integer : Fuzzer Integer
integer =
    Fuzz.map2 mul singleInteger singleInteger


singleNonZeroInteger : Fuzzer Integer
singleNonZeroInteger =
    conditional { retries = 16, fallback = add one, condition = not << eq zero } singleInteger


nonZeroInteger : Fuzzer Integer
nonZeroInteger =
    Fuzz.map2 mul singleNonZeroInteger singleNonZeroInteger


smallInt : Fuzzer Int
smallInt =
    conditional { retries = 16, fallback = always 0, condition = (>=) 1000000 << Basics.abs } int


addTests : Test
addTests =
    describe "addition"
        [ fuzz (tuple ( smallInt, smallInt )) "add x y = x + y for small numbers" <|
            \( x, y ) ->
                add (fromInt x) (fromInt y)
                    |> Expect.equal (fromInt (x + y))
        , fuzz (tuple ( integer, integer )) "x + y + (-y) = x" <|
            \( x, y ) ->
                add x y
                    |> add (Data.Integer.negate y)
                    |> Expect.equal x
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
        , fuzz (tuple ( integer, nonZeroInteger )) "(x * y) / y = x" <|
            \( x, y ) ->
                mul x y
                    |> (\n -> divmod n y)
                    |> Expect.equal (Just ( x, zero ))
        , fuzz (tuple ( integer, integer )) "x * y = y * x" <|
            \( x, y ) ->
                Expect.equal (mul x y) (mul y x)
        ]


divmodTests : Test
divmodTests =
    describe "divmod"
        [ fuzz (tuple ( integer, nonZeroInteger )) "definition" <|
            \( x, y ) ->
                case divmod x y of
                    Nothing ->
                        Expect.equal y (fromInt 0)

                    Just ( c, r ) ->
                        mul c y
                            |> add r
                            |> Expect.equal x
        ]


absTests : Test
absTests =
    describe "abs"
        [ fuzz integer "|x| = x; x >= 0 and |x| = -x; x < 0" <|
            \x ->
                if gte x zero then
                    Expect.equal (Data.Integer.abs x) x
                else
                    Expect.equal (Data.Integer.abs x) (Data.Integer.negate x)
        ]


signTests : Test
signTests =
    describe "sign"
        [ fuzz integer "sign x = Positive; x >0 and sign x = Negative; x < 0 and sign x = Zero; x = 0" <|
            \x ->
                if eq x zero then
                    Expect.equal (sign x) Zero
                else if gt x zero then
                    Expect.equal (sign x) Positive
                else
                    Expect.equal (sign x) Negative
        ]


stringTests : Test
stringTests =
    describe "toString and fromString"
        [ fuzz integer "fromString (toString x) = Just x" <|
            \x ->
                fromString (Data.Integer.toString x)
                    |> Expect.equal (Just x)
        , fuzz smallInt "match string formatting from core" <|
            \x ->
                Data.Integer.toString (fromInt x)
                    |> Expect.equal (Basics.toString x)
        , fuzz integer "accept '+' at the beginning of the string" <|
            \x ->
                let
                    y =
                        x
                            |> Data.Integer.abs
                            |> Data.Integer.toString
                in
                    String.cons '+' y
                        |> fromString
                        |> Expect.equal (fromString y)
        ]


minTests : Test
minTests =
    describe "min"
        [ fuzz (tuple ( integer, integer )) "min x y = x; x <= y and min x y = y; x > y" <|
            \( x, y ) ->
                case Data.Integer.compare x y of
                    GT ->
                        Expect.equal (Data.Integer.min x y) y

                    _ ->
                        Expect.equal (Data.Integer.min x y) x
        ]


maxTests : Test
maxTests =
    describe "max"
        [ fuzz (tuple ( integer, integer )) "min x y = y; x <= y and min x y = x; x > y" <|
            \( x, y ) ->
                case Data.Integer.compare x y of
                    LT ->
                        Expect.equal (Data.Integer.max x y) y

                    _ ->
                        Expect.equal (Data.Integer.max x y) x
        ]


compareTests : Test
compareTests =
    describe "compare"
        [ fuzz integer "x = x" <|
            \x ->
                Expect.true "apparently x /= x" (eq x x)
        , fuzz (tuple ( integer, integer )) "x <= x + y; y >= 0" <|
            \( x, y ) ->
                Expect.true "apparently !(x <= x + y); y >= 0"
                    (lte x (add x (Data.Integer.abs y)))
        , fuzz (tuple ( integer, integer )) "x >= x + y; y <= 0" <|
            \( x, y ) ->
                Expect.true "apparently !(x >= x + y); y <= 0"
                    (gte x (add x (Data.Integer.abs y |> Data.Integer.negate)))
        , fuzz (tuple ( integer, nonZeroInteger )) "x < x + y; y > 0" <|
            \( x, y ) ->
                Expect.true "apparently !(x < x + y); y > 0"
                    (lt x (add x (Data.Integer.abs y)))
        , fuzz (tuple ( integer, nonZeroInteger )) "x > x + y; y < 0" <|
            \( x, y ) ->
                Expect.true "apparently !(x > x + y); y < 0"
                    (gt x (add x (Data.Integer.abs y |> Data.Integer.negate)))
        ]
