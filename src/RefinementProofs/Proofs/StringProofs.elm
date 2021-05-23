module RefinementProofs.Proofs.StringProofs exposing
    ( NonEmptyString
    , TrimmedString
    , NonEmptyTrimmedString
    , mkTrimmedString
    , proveNonEmptyString
    , proveNonEmptyTrimmedString
    , proveTrimmedString
    , lengthOfNonEmptyString
    )

{-| Some basic string proofs


# Definition

@docs NonEmptyString
@docs TrimmedString
@docs NonEmptyTrimmedString


# Proofs

@docs mkTrimmedString
@docs proveNonEmptyString
@docs proveNonEmptyTrimmedString
@docs proveTrimmedString


# Functions

@docs lengthOfNonEmptyString

-}

import RefinementProofs.Knowledge
    exposing
        ( And
        , NoDomainKnowledge
        , NoNamedKnowledge
        , WithKnowledge(..)
        , axiomaticValueKnowledge
        , forget
        , v_makeAnd
        )
import RefinementProofs.Proofs.NumberProofs
    exposing
        ( Positive
        , provePositive
        )


{-| A non-empty string
-}
type NonEmptyString
    = NonEmptyString


{-| A Trimmed string
-}
type TrimmedString
    = TrimmedString


{-| A string that are both nonEmpty and trimmed, alias for "And NonEmptyString TrimmedString"
-}
type alias NonEmptyTrimmedString =
    And NonEmptyString TrimmedString


{-| Prove that a string is non-empty
-}
proveNonEmptyString : String -> Maybe (WithKnowledge String NonEmptyString NoDomainKnowledge NoNamedKnowledge)
proveNonEmptyString x =
    if String.length x > 0 then
        Just <| axiomaticValueKnowledge NonEmptyString x

    else
        Nothing


{-| Make a trimmed string from a string. Note: will trim a non-trimmed string
-}
mkTrimmedString : String -> WithKnowledge String TrimmedString NoDomainKnowledge NoNamedKnowledge
mkTrimmedString =
    axiomaticValueKnowledge TrimmedString << String.trim


{-| Prove that a string is trimmed
-}
proveTrimmedString : String -> Maybe (WithKnowledge String TrimmedString NoDomainKnowledge NoNamedKnowledge)
proveTrimmedString x =
    if String.trim x == x then
        Just <| axiomaticValueKnowledge TrimmedString x

    else
        Nothing


{-| Prove that a string is both non-empty and trimmed
-}
proveNonEmptyTrimmedString : String -> Maybe (WithKnowledge String NonEmptyTrimmedString NoDomainKnowledge NoNamedKnowledge)
proveNonEmptyTrimmedString =
    v_makeAnd proveNonEmptyString proveTrimmedString


{-| Get the length of a non-empty string
-}
lengthOfNonEmptyString : WithKnowledge String NonEmptyString NoDomainKnowledge NoNamedKnowledge -> WithKnowledge Int Positive NoDomainKnowledge NoNamedKnowledge
lengthOfNonEmptyString x =
    case provePositive << String.length <| forget x of
        Just p ->
            p

        Nothing ->
            Debug.todo "absurd"
