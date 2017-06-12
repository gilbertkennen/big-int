module BigIntTests exposing (..)

import BigInt exposing (..)
import Expect
import Fuzz exposing (Fuzzer, conditional, int, tuple)
import String
import Test exposing (..)
import Maybe exposing (Maybe)


singleInteger : Fuzzer BigInt
singleInteger =
    Fuzz.map fromInt int


integer : Fuzzer BigInt
integer =
    Fuzz.map2 mul singleInteger singleInteger


singleNonZeroInteger : Fuzzer BigInt
singleNonZeroInteger =
    conditional { retries = 16, fallback = add one, condition = not << (==) zero } singleInteger


nonZeroInteger : Fuzzer BigInt
nonZeroInteger =
    Fuzz.map2 mul singleNonZeroInteger singleNonZeroInteger


zero : BigInt
zero =
    fromInt 0


one : BigInt
one =
    fromInt 1


minusOne : BigInt
minusOne =
    fromInt -1


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
                    |> add (BigInt.negate y)
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
                        |> BigInt.negate
                        |> Expect.equal (fromInt (-1 * y))
        , fuzz int "negate (-x) = x; x >= 0" <|
            \x ->
                let
                    y =
                        Basics.abs x * -1
                in
                    fromInt y
                        |> BigInt.negate
                        |> Expect.equal (fromInt (-1 * y))
        , fuzz integer "negate (negate x) = x" <|
            \a ->
                a
                    |> BigInt.negate
                    |> BigInt.negate
                    |> Expect.equal a
        ]


subTests : Test
subTests =
    describe "subtraction"
        [ fuzz (tuple ( integer, integer )) "x - y = x + -y" <|
            \( x, y ) ->
                Expect.equal (sub x y) (add x (BigInt.negate y))
        , fuzz (tuple ( integer, integer )) "a - b = -(b - a)" <|
            \( a, b ) -> Expect.equal (sub a b) (BigInt.negate (sub b a))
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
                    Expect.equal (BigInt.abs x) x
                else
                    Expect.equal (BigInt.abs x) (BigInt.negate x)
        ]


signTests : Test
signTests =
    describe "sign"
        [ fuzz integer "sign x = Positive; x >0 and sign x = Negative; x < 0 and sign x = Zero; x = 0" <|
            \x ->
                if x == zero then
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
                fromString (BigInt.toString x)
                    |> Expect.equal (Just x)
        , fuzz smallInt "match string formatting from core" <|
            \x ->
                BigInt.toString (fromInt x)
                    |> Expect.equal (Basics.toString x)
        , fuzz integer "accept '+' at the beginning of the string" <|
            \x ->
                let
                    y =
                        x
                            |> BigInt.abs
                            |> BigInt.toString
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
                case BigInt.compare x y of
                    GT ->
                        Expect.equal (BigInt.min x y) y

                    _ ->
                        Expect.equal (BigInt.min x y) x
        ]


maxTests : Test
maxTests =
    describe "max"
        [ fuzz (tuple ( integer, integer )) "min x y = y; x <= y and min x y = x; x > y" <|
            \( x, y ) ->
                case BigInt.compare x y of
                    LT ->
                        Expect.equal (BigInt.max x y) y

                    _ ->
                        Expect.equal (BigInt.max x y) x
        ]


compareTests : Test
compareTests =
    describe "compare"
        [ fuzz integer "x = x" <|
            \x ->
                Expect.true "apparently x /= x" (x == x)
        , fuzz (tuple ( integer, integer )) "x <= x + y; y >= 0" <|
            \( x, y ) ->
                Expect.true "apparently !(x <= x + y); y >= 0"
                    (lte x (add x (BigInt.abs y)))
        , fuzz (tuple ( integer, integer )) "x >= x + y; y <= 0" <|
            \( x, y ) ->
                Expect.true "apparently !(x >= x + y); y <= 0"
                    (gte x (add x (BigInt.abs y |> BigInt.negate)))
        , fuzz (tuple ( integer, nonZeroInteger )) "x < x + y; y > 0" <|
            \( x, y ) ->
                Expect.true "apparently !(x < x + y); y > 0"
                    (lt x (add x (BigInt.abs y)))
        , fuzz (tuple ( integer, nonZeroInteger )) "x > x + y; y < 0" <|
            \( x, y ) ->
                Expect.true "apparently !(x > x + y); y < 0"
                    (gt x (add x (BigInt.abs y |> BigInt.negate)))
        ]
