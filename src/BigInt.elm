module BigInt
    exposing
        ( BigInt
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

@docs BigInt, Sign


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
import Result exposing (Result)
import Result.Extra
import String


{-| The sign of the bigInt
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



{- From smallest to largest digit, all the digits are positive, no leading zeros -}


{-| BigInt type
-}
type BigInt
    = Pos Magnitude
    | Neg Magnitude
    | Zer


type Magnitude
    = Magnitude (List Int)


mkBigInt : Sign -> Magnitude -> BigInt
mkBigInt s ((Magnitude digits) as mag) =
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


type BigIntNotNormalised
    = BigIntNotNormalised Sign MagnitudeNotNormalised


type MagnitudeNotNormalised
    = MagnitudeNotNormalised (List Int)


mkBigIntNotNormalised : Sign -> List Int -> BigIntNotNormalised
mkBigIntNotNormalised s digits =
    BigIntNotNormalised s (MagnitudeNotNormalised digits)


digits : BigInt -> List Int
digits bigInt =
    case bigInt of
        Zer ->
            []

        Pos (Magnitude digits) ->
            digits

        Neg (Magnitude digits) ->
            digits


{-| Seven base-10 digits is the most we can have where x * x < the JS bigInt limit.
99999999 > sqrt(MAX_SAFE_INTEGER) > 9999999
A slightly higher number is possible, but would require a major reworking of the string functions.
-}
maxDigitValue : Int
maxDigitValue =
    -1 + 10 ^ maxDigitMagnitude


maxDigitMagnitude : Int
maxDigitMagnitude =
    7


{-| Makes an BigInt from an Int
-}
fromInt : Int -> BigInt
fromInt x =
    normalise <| BigIntNotNormalised (signFromInt x) (MagnitudeNotNormalised [ Basics.abs x ])


{-| Makes an BigInt from a String
-}
fromString : String -> Maybe BigInt
fromString x =
    case String.toList x of
        [] ->
            Just zero

        '-' :: xs ->
            fromString_ xs
                |> Maybe.map (mkBigInt Negative)

        '+' :: xs ->
            fromString_ xs
                |> Maybe.map (mkBigInt Positive)

        xs ->
            fromString_ xs
                |> Maybe.map (mkBigInt Positive)


fromString_ : List Char -> Maybe Magnitude
fromString_ x =
    if not <| List.all Char.isDigit x then
        Nothing
    else
        List.reverse x
            |> List.Extra.greedyGroupsOf maxDigitMagnitude
            |> List.map (List.reverse >> String.fromList >> String.toInt)
            |> Result.Extra.combine
            |> Result.toMaybe
            |> Maybe.map (emptyZero << Magnitude)


emptyZero : Magnitude -> Magnitude
emptyZero (Magnitude xs) =
    case List.Extra.dropWhile ((==) 0) xs of
        [] ->
            Magnitude []

        _ ->
            Magnitude xs


type MagnitudePair
    = MagnitudePair (List ( Int, Int ))


sameSizeNormalized : Magnitude -> Magnitude -> MagnitudePair
sameSizeNormalized (Magnitude xs) (Magnitude ys) =
    MagnitudePair (sameSizeRaw xs ys)


sameSizeNotNormalized : MagnitudeNotNormalised -> MagnitudeNotNormalised -> MagnitudePair
sameSizeNotNormalized (MagnitudeNotNormalised xs) (MagnitudeNotNormalised ys) =
    MagnitudePair (sameSizeRaw xs ys)


sameSizeRaw : List Int -> List Int -> List ( Int, Int )
sameSizeRaw xs ys =
    case ( xs, ys ) of
        ( [], [] ) ->
            []

        ( x :: xs_, [] ) ->
            ( x, 0 ) :: sameSizeRaw xs_ []

        ( [], y :: ys_ ) ->
            ( 0, y ) :: sameSizeRaw [] ys_

        ( x :: xs_, y :: ys_ ) ->
            ( x, y ) :: sameSizeRaw xs_ ys_


normalise : BigIntNotNormalised -> BigInt
normalise (BigIntNotNormalised s digits) =
    let
        (Magnitude normalisedMag) =
            normaliseMagnitude digits
    in
        if isNegativeMagnitude (normalisedMag) then
            normalise (mkBigIntNotNormalised (signNegate s) (reverseMagnitude normalisedMag))
        else
            mkBigInt s (Magnitude normalisedMag)


normaliseMagnitude : MagnitudeNotNormalised -> Magnitude
normaliseMagnitude (MagnitudeNotNormalised xs) =
    Magnitude (xs |> normaliseDigitList 0 |> dropZeroes)


normaliseDigitList : Int -> List Int -> List Int
normaliseDigitList carry xs =
    case xs of
        [] ->
            [ carry ]

        x :: xs_ ->
            let
                ( newCarry, x_ ) =
                    normaliseDigit (x + carry)
            in
                x_ :: normaliseDigitList newCarry xs_


normaliseDigit : Int -> ( Int, Int )
normaliseDigit x =
    if x < 0 then
        normaliseDigit (x + maxDigitValue)
            |> Tuple.mapFirst ((+) -1)
    else
        ( x // maxDigitValue, rem x maxDigitValue )


dropZeroes : List Int -> List Int
dropZeroes =
    List.Extra.dropWhileRight ((==) 0)


toPositiveSign : BigInt -> BigIntNotNormalised
toPositiveSign bigInt =
    case bigInt of
        Zer ->
            mkBigIntNotNormalised Zero []

        Neg (Magnitude digits) ->
            mkBigIntNotNormalised Positive (reverseMagnitude digits)

        Pos (Magnitude digits) ->
            mkBigIntNotNormalised Positive digits


isNegativeMagnitude : List Int -> Bool
isNegativeMagnitude digits =
    case List.Extra.last digits of
        Nothing ->
            False

        Just x ->
            x < 0


reverseMagnitude : List Int -> List Int
reverseMagnitude =
    List.map ((*) -1)


{-| Adds two BigInts
-}
add : BigInt -> BigInt -> BigInt
add a b =
    let
        (BigIntNotNormalised _ ma) =
            toPositiveSign a

        (BigIntNotNormalised _ mb) =
            toPositiveSign b

        (MagnitudePair p) =
            sameSizeNotNormalized ma mb

        added =
            List.map (\( x, y ) -> x + y) p
    in
        normalise <| BigIntNotNormalised Positive (MagnitudeNotNormalised added)


{-| Changes the sign of an BigInt
-}
negate : BigInt -> BigInt
negate bigInt =
    case bigInt of
        Zer ->
            Zer

        Pos mag ->
            Neg mag

        Neg mag ->
            Pos mag


{-| Absolute value
-}
abs : BigInt -> BigInt
abs bigInt =
    case bigInt of
        Zer ->
            Zer

        Neg mag ->
            Pos mag

        i ->
            i


{-| Substracts the second BigInt from the first
-}
sub : BigInt -> BigInt -> BigInt
sub a b =
    add a (negate b)


{-| Multiplies two BigInts
-}
mul : BigInt -> BigInt -> BigInt
mul int1 int2 =
    mkBigInt
        (signProduct (sign int1) (sign int2))
        (mulMagnitudes (magnitude int1) (magnitude int2))


magnitude : BigInt -> Magnitude
magnitude bigInt =
    case bigInt of
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

                bigInt =
                    add
                        (mkBigInt Positive accum)
                        (mkBigInt Positive (Magnitude (0 :: rest)))
            in
                magnitude bigInt


mulSingleDigit : Magnitude -> Int -> Magnitude
mulSingleDigit (Magnitude xs) d =
    xs
        |> List.map ((*) d)
        |> MagnitudeNotNormalised
        |> normaliseMagnitude


{-| Compares two BigInts
-}
compare : BigInt -> BigInt -> Order
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
eq : BigInt -> BigInt -> Bool
eq a b =
    case compare a b of
        EQ ->
            True

        _ ->
            False


{-| Not equals
-}
neq : BigInt -> BigInt -> Bool
neq a b =
    not (eq a b)


{-| Less than
-}
lt : BigInt -> BigInt -> Bool
lt a b =
    case compare a b of
        LT ->
            True

        _ ->
            False


{-| Greater than
-}
gt : BigInt -> BigInt -> Bool
gt a b =
    case compare a b of
        GT ->
            True

        _ ->
            False


{-| Greater than or equals
-}
gte : BigInt -> BigInt -> Bool
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
lte : BigInt -> BigInt -> Bool
lte a b =
    case compare a b of
        LT ->
            True

        EQ ->
            True

        _ ->
            False


{-| Returns the largest of two BigInts
-}
max : BigInt -> BigInt -> BigInt
max a b =
    case compare a b of
        GT ->
            a

        EQ ->
            a

        LT ->
            b


{-| Returns the smallest of two BigInts
-}
min : BigInt -> BigInt -> BigInt
min a b =
    case compare a b of
        LT ->
            a

        EQ ->
            a

        GT ->
            b


type MagnitudePairReverseOrder
    = MagnitudePairReverseOrder (List ( Int, Int ))


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


fillZeroes : Int -> String
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


{-| Converts the BigInt to a String
-}
toString : BigInt -> String
toString bigInt =
    case bigInt of
        Zer ->
            "0"

        Pos mag ->
            revMagnitudeToString mag

        Neg mag ->
            "-" ++ revMagnitudeToString mag


{-| BigInt division. Produces 0 when dividing by 0 (like (//)).
-}
div : BigInt -> BigInt -> BigInt
div num den =
    divmod num den
        |> Maybe.map Tuple.first
        |> Maybe.withDefault zero


{-| Modulus. Crashes on zero (like (%)).
-}
mod : BigInt -> BigInt -> BigInt
mod num den =
    case divmod num den |> Maybe.map Tuple.second of
        Nothing ->
            Debug.crash "Cannot perform mod 0. Division by zero error."

        Just x ->
            x


{-| Division and modulus
-}
divmod : BigInt -> BigInt -> Maybe ( BigInt, BigInt )
divmod num den =
    if eq den zero then
        Nothing
    else
        let
            cand_l =
                (List.length (digits num)) - (List.length (digits den)) + 1

            ( d, m ) =
                divMod_
                    (Basics.max 0 cand_l)
                    (abs num)
                    (abs den)
        in
            Just
                ( mkBigInt (signProduct (sign num) (sign den)) (magnitude d)
                , mkBigInt (sign num) (magnitude m)
                )


divmodDigit : BigInt -> BigInt -> BigInt -> ( BigInt, BigInt )
divmodDigit padding x y =
    divmodDigit_ (2 ^ maxDigitBits) padding x y


divmodDigit_ : Int -> BigInt -> BigInt -> BigInt -> ( BigInt, BigInt )
divmodDigit_ to_test padding num den =
    if to_test == 0 then
        ( fromInt 0, num )
    else
        let
            x =
                fromInt to_test

            candidate =
                mul (mul x den) padding

            ( newdiv, newmod ) =
                if lte candidate num then
                    ( mul x padding, sub num candidate )
                else
                    ( zero, num )

            ( restdiv, restmod ) =
                divmodDigit_ (to_test // 2) padding newmod den
        in
            ( add newdiv restdiv, restmod )


divMod_ : Int -> BigInt -> BigInt -> ( BigInt, BigInt )
divMod_ n num den =
    if n == 0 then
        divmodDigit (padDigits n) num den
    else
        let
            ( cdiv, cmod ) =
                divmodDigit (padDigits n) num den

            ( rdiv, rmod ) =
                divMod_ (n - 1) cmod den
        in
            ( add cdiv rdiv, rmod )


maxDigitBits : Int
maxDigitBits =
    maxDigitValue
        |> toFloat
        |> logBase 2
        |> ceiling


padDigits : Int -> BigInt
padDigits n =
    repeatedly (mul (fromInt maxDigitValue)) one n


repeatedly : (a -> a) -> a -> Int -> a
repeatedly f x n =
    List.foldl (always f) x (List.range 1 n)


{-| Get the sign of the bigInt
-}
sign : BigInt -> Sign
sign bigInt =
    case bigInt of
        Zer ->
            Zero

        Pos _ ->
            Positive

        Neg _ ->
            Negative


{-| Number 0
-}
zero : BigInt
zero =
    fromInt 0


{-| Number 1
-}
one : BigInt
one =
    fromInt 1


{-| Number -1
-}
minusOne : BigInt
minusOne =
    fromInt -1
