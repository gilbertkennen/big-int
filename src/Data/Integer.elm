module Data.Integer
    exposing
        ( Integer
        , Sign(Positive, Negative, Zero)
        , sign
        , fromInt
        , fromString
        , toString
        , add
        , sub
        , negate
        , mul
        , divmod
        , div
        , mod
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

@docs add, sub, negate, mul, div, mod, divmod, abs, sign


# Comparison

@docs compare, gt, gte, lt, lte, eq, neq, max, min


# Common numbers

@docs zero, one, minusOne

-}

import Basics
import Char
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
    | Zero


signProduct : Sign -> Sign -> Sign
signProduct x y =
    if x == Zero || y == Zero then
        Zero
    else if x == y then
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

        Zero ->
            Zero


signFromInt : Int -> Sign
signFromInt x =
    if x < 0 then
        Negative
    else if x > 0 then
        Positive
    else
        Zero


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
    = Pos Magnitude
    | Neg Magnitude
    | Zer


type IntegerNotNormalised
    = IntegerNotNormalised Sign MagnitudeNotNormalised


digits : Integer -> List Digit
digits integer =
    case integer of
        Zer ->
            []

        Pos (Magnitude digits) ->
            digits

        Neg (Magnitude digits) ->
            digits


{-| Six base-10 digits is the most we can have where x * x < the JS integer limit.
-}
maxDigitValue : Int
maxDigitValue =
    -1 + 10 ^ maxDigitMagnitude


maxDigitMagnitude : Int
maxDigitMagnitude =
    6


{-| Makes an Integer from an Int
-}
fromInt : Int -> Integer
fromInt x =
    (normalise <| IntegerNotNormalised (signFromInt x) (MagnitudeNotNormalised [ Basics.abs x ]))


mkInteger : Sign -> Magnitude -> Integer
mkInteger s ((Magnitude digits) as mag) =
    if List.isEmpty digits then
        Zer
    else
        case s of
            Zero ->
                Zer

            Positive ->
                Pos mag

            Negative ->
                Neg mag


{-| Makes an Integer from a String
-}
fromString : String -> Maybe Integer
fromString x =
    case String.toList x of
        [] ->
            Just zero

        '-' :: xs ->
            fromString_ xs
                |> Maybe.map (mkInteger Negative)

        '+' :: xs ->
            fromString_ xs
                |> Maybe.map (mkInteger Positive)

        xs ->
            fromString_ xs
                |> Maybe.map (mkInteger Positive)


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
normalise (IntegerNotNormalised s digits) =
    let
        (Magnitude normalisedMag) =
            normaliseMagnitude digits
    in
        if isNegativeMagnitude (normalisedMag) then
            normalise (mkIntegerNotNormalised (signNegate s) (reverseMagnitude normalisedMag))
        else
            mkInteger s (Magnitude normalisedMag)


normaliseDigitList : List Int -> List Digit
normaliseDigitList x =
    case x of
        [] ->
            []

        x1 :: xs ->
            let
                ( c, dPrime ) =
                    normaliseDigit x1
            in
                case xs of
                    [] ->
                        [ dPrime, c ]

                    x2 :: rest ->
                        dPrime :: normaliseDigitList (x2 + c :: rest)


normaliseDigit : Int -> ( Int, Digit )
normaliseDigit x =
    if x < 0 then
        let
            ( carry, dPrime ) =
                normaliseDigit (x + maxDigitValue)
        in
            ( carry - 1, dPrime )
    else
        ( x // maxDigitValue, rem x maxDigitValue )


dropZeroes : List Digit -> List Digit
dropZeroes =
    List.reverse
        >> List.Extra.dropWhile ((==) 0)
        >> List.reverse


normaliseMagnitude : MagnitudeNotNormalised -> Magnitude
normaliseMagnitude (MagnitudeNotNormalised xs) =
    Magnitude (xs |> normaliseDigitList |> dropZeroes)


reverseMagnitude : List Digit -> List Digit
reverseMagnitude =
    List.map ((*) -1)


isNegativeMagnitude : List Digit -> Bool
isNegativeMagnitude digits =
    case List.Extra.last digits of
        Nothing ->
            False

        Just x ->
            x < 0


toPositiveSign : Integer -> IntegerNotNormalised
toPositiveSign integer =
    case integer of
        Zer ->
            mkIntegerNotNormalised Zero []

        Neg (Magnitude digits) ->
            mkIntegerNotNormalised Positive (reverseMagnitude digits)

        Pos (Magnitude digits) ->
            mkIntegerNotNormalised Positive digits


mkIntegerNotNormalised : Sign -> List Digit -> IntegerNotNormalised
mkIntegerNotNormalised s digits =
    IntegerNotNormalised s (MagnitudeNotNormalised digits)


{-| Adds two Integers
-}
add : Integer -> Integer -> Integer
add a b =
    let
        (IntegerNotNormalised _ ma) =
            toPositiveSign a

        (IntegerNotNormalised _ mb) =
            toPositiveSign b

        (MagnitudePair p) =
            sameSizeNotNormalized ma mb

        added =
            List.map (\( x, y ) -> x + y) p
    in
        normalise <| IntegerNotNormalised Positive (MagnitudeNotNormalised added)


{-| Changes the sign of an Integer
-}
negate : Integer -> Integer
negate integer =
    case integer of
        Zer ->
            Zer

        Pos mag ->
            Neg mag

        Neg mag ->
            Pos mag


{-| Absolute value
-}
abs : Integer -> Integer
abs integer =
    case integer of
        Zer ->
            Zer

        Neg mag ->
            Pos mag

        i ->
            i


{-| Substracts the second Integer from the first
-}
sub : Integer -> Integer -> Integer
sub a b =
    add a (negate b)


{-| Multiplies two Integers
-}
mul : Integer -> Integer -> Integer
mul int1 int2 =
    mkInteger
        (signProduct (sign int1) (sign int2))
        (mulMagnitudes (magnitude int1) (magnitude int2))


magnitude : Integer -> Magnitude
magnitude integer =
    case integer of
        Zer ->
            Magnitude []

        Pos mag ->
            mag

        Neg mag ->
            mag


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

                integer =
                    add
                        (mkInteger Positive accum)
                        (mkInteger Positive (Magnitude (0 :: rest)))
            in
                magnitude integer


mulSingleDigit : Magnitude -> Digit -> Magnitude
mulSingleDigit (Magnitude xs) d =
    xs
        |> List.map ((*) d)
        |> MagnitudeNotNormalised
        |> normaliseMagnitude


{-| Compares two Integers
-}
compare : Integer -> Integer -> Order
compare int1 int2 =
    case ( int1, int2 ) of
        ( Zer, Pos _ ) ->
            LT

        ( Zer, Zer ) ->
            EQ

        ( Zer, Neg _ ) ->
            GT

        ( Pos _, Zer ) ->
            GT

        ( Pos _, Neg _ ) ->
            GT

        ( Neg _, Pos _ ) ->
            LT

        ( Neg _, Zer ) ->
            LT

        ( Pos mag1, Pos mag2 ) ->
            sameSizeNormalized mag1 mag2
                |> reverseMagnitudePair
                |> compareMagnitude

        ( Neg mag1, Neg mag2 ) ->
            sameSizeNormalized mag1 mag2
                |> reverseMagnitudePair
                |> compareMagnitude
                |> orderNegate


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


revMagnitudeToString : Magnitude -> String
revMagnitudeToString (Magnitude digits) =
    case List.reverse digits of
        [] ->
            "0"

        x :: xs ->
            (Basics.toString x) ++ String.concat (List.map fillZeroes xs)


{-| Converts the Integer to a String
-}
toString : Integer -> String
toString integer =
    case integer of
        Zer ->
            "0"

        Pos mag ->
            revMagnitudeToString mag

        Neg mag ->
            "-" ++ revMagnitudeToString mag


dividers : List Integer
dividers =
    maxDigitValue
        |> Basics.toFloat
        |> Basics.logBase 2
        |> Basics.truncate
        |> (+) 1
        |> List.range 0
        |> List.reverse
        |> List.map ((^) 2)
        |> List.map fromInt


padDigits : Int -> Integer
padDigits n =
    if n == 0 then
        fromInt 1
    else
        mul (padDigits (n - 1)) (fromInt maxDigitValue)


{-| Integer division. Produces 0 when dividing by 0 (like (//)).
-}
div : Integer -> Integer -> Integer
div num den =
    divmod num den
        |> Maybe.map Tuple.first
        |> Maybe.withDefault zero


{-| Modulus. Crashes on zero (like (%)).
-}
mod : Integer -> Integer -> Integer
mod num den =
    case divmod num den |> Maybe.map Tuple.second of
        Nothing ->
            Debug.crash "Cannot perform mod 0. Division by zero error."

        Just x ->
            x


{-| Division and modulus
-}
divmod : Integer -> Integer -> Maybe ( Integer, Integer )
divmod int1 int2 =
    if eq int2 zero then
        Nothing
    else
        let
            cand_l =
                (List.length (digits int1)) - (List.length (digits int2)) + 1

            s =
                signProduct (sign int1) (sign int2)

            ( d, m ) =
                divMod_ (Basics.max 0 cand_l) (abs int1) (abs int2)
        in
            Just ( mkInteger s (magnitude d), mkInteger (sign int1) (magnitude m) )


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


{-| Get the sign of the integer
-}
sign : Integer -> Sign
sign integer =
    case integer of
        Zer ->
            Zero

        Pos _ ->
            Positive

        Neg _ ->
            Negative


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
