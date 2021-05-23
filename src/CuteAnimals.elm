module CuteAnimals exposing (IsAValidCuteAnimal, proveCuteness)

import Dict exposing (Dict)
import RefinementProofs.Knowledge exposing (A, NoDomainKnowledge, NoNamedKnowledge, NoValueKnowledge, WithKnowledge, axiomaticallySetDomainKnowledge, forget, forgetNamedKnowledge, raw)
import RefinementProofs.Proofs.DictProofs exposing (IsInDict)
import RefinementProofs.Proofs.NumberProofs exposing (Positive, provePositive)
import RefinementProofs.Proofs.DictProofs exposing (takeValueFromDict)
import Animals exposing (AllKnownAnimals)


type IsAValidCuteAnimal
    = IsAValidCuteAnimal

proveCuteness :
    WithKnowledge (A (Dict Int String) animals) NoValueKnowledge AllKnownAnimals NoNamedKnowledge
    -> WithKnowledge (A Int animalId) Positive NoDomainKnowledge (IsInDict animalId animals)
    -> Maybe (WithKnowledge (A Int animalId) Positive IsAValidCuteAnimal NoNamedKnowledge)
proveCuteness allAnimals animalId =
  let
    animalCommonName = takeValueFromDict animalId (forget allAnimals)
  in
  -- No animals with q can ever be cute, nor any animals with an id over 40, that is just common sense
  if String.contains "q" animalCommonName || raw animalId > 40 then
    Nothing
  else
    Just <| axiomaticallySetDomainKnowledge IsAValidCuteAnimal <| forgetNamedKnowledge animalId

