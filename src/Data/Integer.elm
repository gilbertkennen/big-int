module Data.Integer
    exposing
        ( Integer
        , Sign(Positive, Negative)
        , sign
        , fromInt
        , fromString
        , toString
        , add
        , sub
        , negate
        , mul
        , divmod
        , unsafeDivmod
        , abs
        , compare
        , gt
        , gte
        , lt
        , lte
        , eq
        , neq
        , max
        , min
        , zero
        , one
        , minusOne
        )

{-| Infinite digits integers
# The datatype
@docs Integer, Sign

# From/To
@docs fromInt, fromString, toString

# Common operations
@docs add, sub, negate, mul, divmod, unsafeDivmod, abs, sign

# Comparison
@docs compare, gt, gte, lt, lte, eq, neq, max, min

# Common numbers
@docs zero, one, minusOne

-}

import Basics
import Char
import Debug
import List.Extra
import Maybe exposing (Maybe)
import Maybe.Extra
import Result exposing (Result)
import String


{-| The sign of the integer
-}
type Sign
    = Positive
    | Negative


signProduct : Sign -> Sign -> Sign
signProduct x y =
    if x == y then
        Positive
    else
        Negative


signNegate : Sign -> Sign
signNegate sign =
    case sign of
        Positive ->
            Negative

        Negative ->
            Positive


signFromInt : Int -> Sign
signFromInt x =
    if x < 0 then
        Negative
    else
        Positive


type alias Digit =
    Int



{- From smallest to largest digit, all the digits are positive, no leading zeros -}


type Magnitude
    = Magnitude (List Digit)


type MagnitudeNotNormalised
    = MagnitudeNotNormalised (List Digit)


{-| Integer type
-}
type Integer
    = Integer ( Sign, Magnitude )


type IntegerNotNormalised
    = IntegerNotNormalised ( Sign, MagnitudeNotNormalised )


{-| Enough to hold digit * digit without overflowing to double.
-}
maxDigitValue : Int
maxDigitValue =
    1000000


maxDigitMagnitude : Int
maxDigitMagnitude =
    6


{-| Makes an Integer from an Int
-}
fromInt : Int -> Integer
fromInt x =
    (normalise <| IntegerNotNormalised ( signFromInt x, MagnitudeNotNormalised [ Basics.abs x ] ))


{-| Makes an Integer from a String
-}
fromString : String -> Maybe Integer
fromString x =
    case String.toList x of
        [] ->
            Just (fromInt 0)

        '-' :: xs ->
            fromString_ xs
                |> Maybe.map (Integer << (,) Negative)

        '+' :: xs ->
            fromString_ xs
                |> Maybe.map (Integer << (,) Positive)

        xs ->
            fromString_ xs
                |> Maybe.map (Integer << (,) Positive)


fromString_ : List Char -> Maybe Magnitude
fromString_ x =
    if not <| List.all Char.isDigit x then
        Nothing
    else
        List.reverse x
            |> List.Extra.greedyGroupsOf maxDigitMagnitude
            |> List.map (List.reverse >> String.fromList >> String.toInt >> Result.toMaybe)
            |> Maybe.Extra.combine
            |> Maybe.map (emptyZero << Magnitude)


emptyZero : Magnitude -> Magnitude
emptyZero (Magnitude xs) =
    case List.Extra.dropWhile ((==) 0) xs of
        [] ->
            Magnitude []

        _ ->
            Magnitude xs


type MagnitudePair
    = MagnitudePair (List ( Digit, Digit ))


sameSizeNormalized : Magnitude -> Magnitude -> MagnitudePair
sameSizeNormalized (Magnitude xs) (Magnitude ys) =
    MagnitudePair (sameSizeRaw xs ys)


sameSizeNotNormalized : MagnitudeNotNormalised -> MagnitudeNotNormalised -> MagnitudePair
sameSizeNotNormalized (MagnitudeNotNormalised xs) (MagnitudeNotNormalised ys) =
    MagnitudePair (sameSizeRaw xs ys)


sameSizeRaw : List Int -> List Int -> List ( Int, Int )
sameSizeRaw =
    greedyZip (\x y -> ( Maybe.withDefault 0 x, Maybe.withDefault 0 y ))


greedyZip : (Maybe a -> Maybe b -> c) -> List a -> List b -> List c
greedyZip f =
    let
        go acc lefts rights =
            case ( lefts, rights ) of
                ( [], [] ) ->
                    List.reverse acc

                ( x :: xs, [] ) ->
                    go (f (Just x) Nothing :: acc) xs []

                ( [], y :: ys ) ->
                    go (f Nothing (Just y) :: acc) [] ys

                ( x :: xs, y :: ys ) ->
                    go (f (Just x) (Just y) :: acc) xs ys
    in
        go []


normalise : IntegerNotNormalised -> Integer
normalise (IntegerNotNormalised ( sign, x )) =
    let
        nmagnitude =
            normaliseMagnitude x
    in
        if isNegativeMagnitude nmagnitude then
            normalise (IntegerNotNormalised ( signNegate sign, reverseMagnitude nmagnitude ))
        else
            Integer ( sign, nmagnitude )


reverseMagnitude : Magnitude -> MagnitudeNotNormalised
reverseMagnitude (Magnitude xs) =
    MagnitudeNotNormalised (List.map ((*) -1) xs)


isNegativeMagnitude : Magnitude -> Bool
isNegativeMagnitude (Magnitude xs) =
    case List.Extra.last xs of
        Nothing ->
            False

        Just x ->
            x < 0


normaliseDigit : Int -> ( Int, Digit )
normaliseDigit d =
    if d < 0 then
        let
            ( carry, dPrime ) =
                normaliseDigit (d + maxDigitValue)
        in
            ( carry - 1, dPrime )
    else
        ( d // maxDigitValue, rem d maxDigitValue )


normaliseDigitList : List Int -> List Digit
normaliseDigitList x =
    case x of
        [] ->
            []

        d :: [] ->
            let
                ( c, dPrime ) =
                    normaliseDigit d
            in
                [ dPrime, c ]

        d :: d2 :: xs ->
            let
                ( c, dPrime ) =
                    normaliseDigit d
            in
                dPrime :: normaliseDigitList (d2 + c :: xs)


dropZeroes : List Digit -> List Digit
dropZeroes =
    List.reverse
        >> List.Extra.dropWhile ((==) 0)
        >> List.reverse


normaliseMagnitude : MagnitudeNotNormalised -> Magnitude
normaliseMagnitude (MagnitudeNotNormalised xs) =
    Magnitude (xs |> normaliseDigitList |> dropZeroes)


toPositiveSign : Integer -> IntegerNotNormalised
toPositiveSign (Integer ( sign, Magnitude xs )) =
    case sign of
        Positive ->
            IntegerNotNormalised ( Positive, MagnitudeNotNormalised xs )

        Negative ->
            IntegerNotNormalised ( Positive, reverseMagnitude (Magnitude xs) )


{-| Adds two Integers
-}
add : Integer -> Integer -> Integer
add a b =
    let
        (IntegerNotNormalised ( _, ma )) =
            toPositiveSign a

        (IntegerNotNormalised ( _, mb )) =
            toPositiveSign b

        (MagnitudePair p) =
            sameSizeNotNormalized ma mb

        added =
            List.map (\( x, y ) -> x + y) p
    in
        normalise (IntegerNotNormalised ( Positive, MagnitudeNotNormalised added ))


{-| Changes the sign of an Integer
-}
negate : Integer -> Integer
negate (Integer ( sign, xs )) =
    normalise (toPositiveSign (Integer ( signNegate sign, xs )))


{-| Absolute value
-}
abs : Integer -> Integer
abs (Integer ( s, m )) =
    Integer ( Positive, m )


{-| Substracts the second Integer from the first
-}
sub : Integer -> Integer -> Integer
sub a b =
    add a (negate b)


{-| Multiplies two Integers
-}
mul : Integer -> Integer -> Integer
mul (Integer ( sign1, m1 )) (Integer ( sign2, m2 )) =
    Integer ( signProduct sign1 sign2, mulMagnitudes m1 m2 )
        |> positiveZero


positiveZero : Integer -> Integer
positiveZero ((Integer ( _, Magnitude xs )) as int) =
    case xs of
        [] ->
            Integer ( Positive, Magnitude [] )

        _ ->
            int


mulMagnitudes : Magnitude -> Magnitude -> Magnitude
mulMagnitudes (Magnitude m1) (Magnitude m2) =
    case m1 of
        [] ->
            Magnitude []

        m :: [] ->
            mulSingleDigit (Magnitude m2) m

        m :: mx ->
            let
                accum =
                    mulSingleDigit (Magnitude m2) m

                (Magnitude rest) =
                    mulMagnitudes (Magnitude mx) (Magnitude m2)

                (Integer ( _, result )) =
                    add
                        (Integer ( Positive, accum ))
                        (Integer ( Positive, (Magnitude (0 :: rest)) ))
            in
                result


mulSingleDigit : Magnitude -> Digit -> Magnitude
mulSingleDigit (Magnitude xs) d =
    xs
        |> List.map ((*) d)
        |> MagnitudeNotNormalised
        |> normaliseMagnitude


{-| Compares two Integers
-}
compare : Integer -> Integer -> Order
compare (Integer ( sa, a )) (Integer ( sb, b )) =
    case ( sa, sb ) of
        ( Positive, Negative ) ->
            GT

        ( Negative, Positive ) ->
            LT

        _ ->
            let
                cr =
                    sameSizeNormalized a b
                        |> reverseMagnitudePair
                        |> compareMagnitude
            in
                if sa == Positive then
                    cr
                else
                    orderNegate cr


orderNegate : Order -> Order
orderNegate x =
    case x of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


{-| Equals
-}
eq : Integer -> Integer -> Bool
eq a b =
    case compare a b of
        EQ ->
            True

        _ ->
            False


{-| Not equals
-}
neq : Integer -> Integer -> Bool
neq a b =
    not (eq a b)


{-| Less than
-}
lt : Integer -> Integer -> Bool
lt a b =
    case compare a b of
        LT ->
            True

        _ ->
            False


{-| Greater than
-}
gt : Integer -> Integer -> Bool
gt a b =
    case compare a b of
        GT ->
            True

        _ ->
            False


{-| Greater than or equals
-}
gte : Integer -> Integer -> Bool
gte a b =
    case compare a b of
        GT ->
            True

        EQ ->
            True

        _ ->
            False


{-| Less than or equals
-}
lte : Integer -> Integer -> Bool
lte a b =
    case compare a b of
        LT ->
            True

        EQ ->
            True

        _ ->
            False


{-| Returns the largest of two Integers
-}
max : Integer -> Integer -> Integer
max a b =
    case compare a b of
        GT ->
            a

        EQ ->
            a

        LT ->
            b


{-| Returns the smallest of two Integers
-}
min : Integer -> Integer -> Integer
min a b =
    case compare a b of
        LT ->
            a

        EQ ->
            a

        GT ->
            b


type MagnitudePairReverseOrder
    = MagnitudePairReverseOrder (List ( Digit, Digit ))


reverseMagnitudePair : MagnitudePair -> MagnitudePairReverseOrder
reverseMagnitudePair (MagnitudePair x) =
    MagnitudePairReverseOrder <| List.reverse x


compareMagnitude : MagnitudePairReverseOrder -> Order
compareMagnitude (MagnitudePairReverseOrder m) =
    case m of
        [] ->
            EQ

        ( a, b ) :: xs ->
            if a == b then
                compareMagnitude (MagnitudePairReverseOrder xs)
            else
                Basics.compare a b


zeroes : Int -> String
zeroes n =
    String.repeat n "0"


fillZeroes : Digit -> String
fillZeroes d =
    let
        d_s =
            Basics.toString d

        len =
            String.length d_s
    in
        zeroes (maxDigitMagnitude - len) ++ d_s


revmagnitudeToString : List Digit -> String
revmagnitudeToString m =
    case m of
        [] ->
            "0"

        x :: xs ->
            (Basics.toString x) ++ String.concat (List.map fillZeroes xs)


{-| Converts the Integer to a String
-}
toString : Integer -> String
toString (Integer ( s, Magnitude m )) =
    let
        sign =
            if s == Positive then
                ""
            else
                "-"
    in
        sign ++ revmagnitudeToString (List.reverse m)


range : Int -> Int -> List Int
range a b =
    if a == b then
        [ a ]
    else
        a :: range (a + 1) b


dividers : List Integer
dividers =
    maxDigitValue
        |> Basics.toFloat
        |> Basics.logBase 2
        |> Basics.truncate
        |> (+) 1
        |> (List.reverse << range 0)
        |> List.map ((^) 2)
        |> List.map fromInt


padDigits : Int -> Integer
padDigits n =
    if n == 0 then
        fromInt 1
    else
        mul (padDigits (n - 1)) (fromInt maxDigitValue)


divmodDigit : Integer -> List Integer -> Integer -> Integer -> ( Integer, Integer )
divmodDigit padding to_test a b =
    case to_test of
        [] ->
            ( fromInt 0, a )

        x :: xs ->
            let
                candidate =
                    mul (mul x b) padding

                ( newdiv, newmod ) =
                    if lte candidate a then
                        ( mul x padding, sub a candidate )
                    else
                        ( fromInt 0, a )

                ( restdiv, restmod ) =
                    divmodDigit padding xs newmod b
            in
                ( add newdiv restdiv, restmod )


divMod_ : Int -> Integer -> Integer -> ( Integer, Integer )
divMod_ n a b =
    if n == 0 then
        divmodDigit (padDigits n) dividers a b
    else
        let
            ( cdiv, cmod ) =
                divmodDigit (padDigits n) dividers a b

            ( rdiv, rmod ) =
                divMod_ (n - 1) cmod b
        in
            ( add cdiv rdiv, rmod )


{-| Division and modulus
-}
divmod : Integer -> Integer -> Maybe ( Integer, Integer )
divmod a b =
    if eq b zero then
        Nothing
    else
        let
            (Integer ( s1, Magnitude m1 )) =
                a

            (Integer ( s2, Magnitude m2 )) =
                b

            cand_l =
                (List.length m1) - (List.length m2) + 1

            sign =
                signProduct s1 s2

            ( Integer ( _, d ), Integer ( _, m ) ) =
                divMod_ (Basics.max 0 cand_l) (abs a) (abs b)
        in
            Just
                ( Integer ( sign, d ) |> positiveZero
                , Integer ( s1, m ) |> positiveZero
                )


{-| divmod that returns the pair of values, or crashes if the divisor is zero
-}
unsafeDivmod : Integer -> Integer -> ( Integer, Integer )
unsafeDivmod a b =
    case divmod a b of
        Just r ->
            r

        Nothing ->
            Debug.crash "Divide by zero"


{-| Get the sign of the integer
-}
sign : Integer -> Sign
sign (Integer ( x, _ )) =
    x


{-| Number 0
-}
zero : Integer
zero =
    fromInt 0


{-| Number 1
-}
one : Integer
one =
    fromInt 1


{-| Number -1
-}
minusOne : Integer
minusOne =
    fromInt -1
