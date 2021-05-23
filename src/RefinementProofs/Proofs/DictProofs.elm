module RefinementProofs.Proofs.DictProofs exposing
    ( IsInDict
    , proveKeyIsInDict
    , takeValueFromDict
    )

{-| Some basic dictionary proofs


# Definition

@docs IsInDict


# Functions

@docs proveKeyIsInDict
@docs takeValueFromDict

-}

import Dict exposing (Dict)
import RefinementProofs.Knowledge
    exposing
        ( A
        , And
        , NoDomainKnowledge
        , NoNamedKnowledge
        , Proof
        , WithKnowledge(..)
        , axiomaticValueKnowledge
        , forget
        , makeProof
        , raw
        , the
        , v_makeAnd
        )
import RefinementProofs.Proofs.NumberProofs
    exposing
        ( Positive
        , provePositive
        )


{-| Knowledge about that `key` is a key in `dict`
-}
type IsInDict key dict
    = IsInDict


{-| Prove that a value is a key in a given dictionary.

Note that both the key and the dict must be named using the `name`function in Knowledge.WithKnowledge

-}
proveKeyIsInDict : A comparable keyName -> A (Dict comparable v) dictName -> Maybe (Proof (IsInDict keyName dictName))
proveKeyIsInDict tk tdict =
    if Dict.member (the tk) (the tdict) then
        Just <| makeProof IsInDict

    else
        Nothing


{-| Take the value corresponding to a key. Since we know that the key exist in the dictionary this is safe.
-}
takeValueFromDict :
    WithKnowledge (A comparable keyName) anyValueKnowledge anyDomainKnowledge (IsInDict keyName dictName)
    -> A (Dict comparable v) dictName
    -> v
takeValueFromDict ptkey tdict =
    case Dict.get (raw ptkey) (the tdict) of
        Nothing ->
            Debug.todo "absurd"

        Just v ->
            v
