module Animals exposing (AllKnownAnimals, addNewKnownAnimal, initAllKnownAnimals, lookupRealAnimal)

import Dict exposing (Dict)
import RefinementProofs.Knowledge exposing ( A
    , NoDomainKnowledge
    , NoNamedKnowledge
    , NoValueKnowledge
    , WithKnowledge
    , axiomaticallySetDomainKnowledge
    , forget
    , setNamedKnowledge
    , withName
    , withNoKnowledge
    )
import RefinementProofs.Proofs.DictProofs exposing (IsInDict, proveKeyIsInDict)
import RefinementProofs.Proofs.NumberProofs exposing (Positive, provePositive)
import RefinementProofs.Proofs.NumberProofs exposing (proveZero)
import RefinementProofs.Knowledge exposing (v_makeOr)
import RefinementProofs.Knowledge exposing (Or)
import RefinementProofs.Proofs.NumberProofs exposing (Zero)


type AllKnownAnimals
    = AllKnownAnimals


initAllKnownAnimals : WithKnowledge (Dict Int String) NoValueKnowledge AllKnownAnimals NoNamedKnowledge
initAllKnownAnimals =
    axiomaticallySetDomainKnowledge AllKnownAnimals <| withNoKnowledge <| Dict.fromList [ ( 1, "Wombat" ), ( 2, "Horse" ), ( -1, "Unicorn" ) ]


addNewKnownAnimal :
    Int
    -> String
    -> WithKnowledge (Dict Int String) NoValueKnowledge AllKnownAnimals NoNamedKnowledge
    -> WithKnowledge (Dict Int String) NoValueKnowledge AllKnownAnimals NoNamedKnowledge
addNewKnownAnimal animalId animalName animals =
    axiomaticallySetDomainKnowledge AllKnownAnimals <|
        withNoKnowledge <|
            Dict.insert animalId animalName <|
                forget animals


lookupRealAnimal :
    WithKnowledge (A Int animalId) NoValueKnowledge NoDomainKnowledge NoNamedKnowledge
    -> WithKnowledge (A (Dict Int String) animals) NoValueKnowledge AllKnownAnimals NoNamedKnowledge
    -> Maybe (WithKnowledge (A Int animalId) (Or Positive Zero) NoDomainKnowledge (IsInDict animalId animals))
lookupRealAnimal namedWantedKey namedDict =
    case ( proveKeyIsInDict (forget namedWantedKey) (forget namedDict), withName (v_makeOr provePositive proveZero) (forget namedWantedKey) ) of
        ( Just isInDictProof, Just isPositive ) ->
            let
                keyWithAllNeededProofs : WithKnowledge (A Int animalId) (Or Positive Zero) NoDomainKnowledge (IsInDict animalId animals)
                keyWithAllNeededProofs =
                    setNamedKnowledge isInDictProof isPositive
            in
            Just keyWithAllNeededProofs

        _ ->
            Nothing
