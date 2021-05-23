module RefinementProofs.Proofs.NumberProofs exposing
    ( Even
    , Negative
    , Odd
    , Positive
    , Zero
    , ZeroOrGreater
    , proveEven
    , proveNegative
    , proveOdd
    , provePositive
    , proveZeroOrGreater
    , allPositivesAreZeroOrGreater
    )

{-| Some basic number proofs


# Definition

@docs Even
@docs Negative
@docs Odd
@docs Positive
@docs Zero
@docs ZeroOrGreater


# Proofs

@docs proveEven
@docs proveNegative
@docs proveOdd
@docs provePositive
@docs proveZeroOrGreater


# Implications

@docs allPositivesAreZeroOrGreater

-}

import RefinementProofs.Knowledge
    exposing
        ( Implies
        , NoDomainKnowledge
        , NoNamedKnowledge
        , Not
        , Or
        , WithKnowledge(..)
        , axiomaticValueKnowledge
        , imply
        , or
        , v_inverse
        , v_makeOr
        )


{-| A positive number
-}
type Positive
    = Positive


{-| A number that is Zero
-}
type Zero
    = Zero


{-| type alias for `Or Zero Positive`. If used with an Int this the number is 0 or 1 or 2 or 3....
-}
type alias ZeroOrGreater =
    Or Zero Positive


{-| A negative number. Alias for `Not ZeroOrGreater`
-}
type alias Negative =
    Not ZeroOrGreater


{-| An even Int
-}
type Even
    = Even


{-| alias for `Not Even`
-}
type alias Odd =
    Not Even


{-| Prove that a number equals to zero
-}
proveZero : number -> Maybe (WithKnowledge number Zero NoDomainKnowledge NoNamedKnowledge)
proveZero x =
    if x == 0 then
        Just <| axiomaticValueKnowledge Zero x

    else
        Nothing


{-| Prove that a number is positive (>0)
-}
provePositive : number -> Maybe (WithKnowledge number Positive NoDomainKnowledge NoNamedKnowledge)
provePositive x =
    if x > 0 then
        Just <| axiomaticValueKnowledge Positive x

    else
        Nothing


{-| Prove that a number is either zero or greater
-}
proveZeroOrGreater : number -> Maybe (WithKnowledge number ZeroOrGreater NoDomainKnowledge NoNamedKnowledge)
proveZeroOrGreater =
    v_makeOr proveZero provePositive


{-| Prove a number is negative
-}
proveNegative : number -> Maybe (WithKnowledge number Negative NoDomainKnowledge NoNamedKnowledge)
proveNegative =
    v_inverse (or Zero Positive) proveZeroOrGreater


{-| Prove that a number is even
-}
proveEven : Int -> Maybe (WithKnowledge Int Even NoDomainKnowledge NoNamedKnowledge)
proveEven x =
    if modBy 2 x == 0 then
        Just <| axiomaticValueKnowledge Even x

    else
        Nothing


{-| Prove that a number is odd
-}
proveOdd : Int -> Maybe (WithKnowledge Int Odd NoDomainKnowledge NoNamedKnowledge)
proveOdd =
    v_inverse Even proveEven


{-| A simple implication.
Note this is strictly not needed since this implication is clear in the types - `ZeroOrGreater = Or Zero Positive`.
An alternative is to use `introOrL` or `or`.
-}
allPositivesAreZeroOrGreater : Implies Positive ZeroOrGreater
allPositivesAreZeroOrGreater =
    imply Positive (or Zero Positive)
