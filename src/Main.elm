module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, pre, text)
import Html.Events exposing (onClick)
import Http
import RefinementProofs.Knowledge exposing (A, NoDomainKnowledge, NoNamedKnowledge, NoValueKnowledge, Proof, WithKnowledge, axiomaticallySetDomainKnowledge, forget, forgetNamedKnowledge, forgetNamedKnowledgeAndName, name, name2, name2WithKnowledge, raw, setNamedKnowledge, withName, withNoKnowledge)
import RefinementProofs.Proofs.DictProofs exposing (IsInDict, proveKeyIsInDict)
import RefinementProofs.Proofs.NumberProofs exposing (Positive, provePositive)
import RefinementProofs.Proofs.StringProofs exposing (NonEmptyString, proveNonEmptyString)
import Dict
import RefinementProofs.Proofs.DictProofs exposing (takeValueFromDict)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


--Should be in a separate module to protect ctor
type IsAValidCuteAnimal
    = IsAValidCuteAnimal


--Should be in a separate module to protect ctor
type AllKnownAnimals
    = AllKnownAnimals


type alias Model =
    { animals : WithKnowledge (Dict Int String) NoValueKnowledge AllKnownAnimals NoNamedKnowledge
    , selectedAnimalId : Maybe (WithKnowledge Int Positive IsAValidCuteAnimal NoNamedKnowledge)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { animals = axiomaticallySetDomainKnowledge AllKnownAnimals <| withNoKnowledge <| Dict.fromList [ ( 1, "Wombat" ), ( 2, "Unicorn" ), (-1, "Imaginary horse")]
      , selectedAnimalId = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SetCuteAnimal Int
    | AddToKnownAnimals Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetCuteAnimal i ->
            case Maybe.map forgetNamedKnowledgeAndName <| name2WithKnowledge (withNoKnowledge i) model.animals handleSetCuteAnimal of
                Just newId ->
                    ( { model | selectedAnimalId = Just newId }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )
        AddToKnownAnimals animalId animalName ->
          -- This should be in another module to hide ctors and important logic
          let
            newKnownAnimals = axiomaticallySetDomainKnowledge AllKnownAnimals <| withNoKnowledge <|
              Dict.insert animalId animalName
              <| forget model.animals
          in
          ( { model | animals = newKnownAnimals }, Cmd.none )


handleSetCuteAnimal :
    WithKnowledge (A Int animalId) NoValueKnowledge NoDomainKnowledge NoNamedKnowledge
    -> WithKnowledge (A (Dict Int String) animals) NoValueKnowledge AllKnownAnimals NoNamedKnowledge
    -> Maybe (WithKnowledge (A Int animalId) Positive IsAValidCuteAnimal NoNamedKnowledge)
handleSetCuteAnimal namedId namedDict =
    lookupRealAnimal namedId (forget namedDict) |>
    Maybe.andThen (ensureCuteness namedDict) |>
    Maybe.map forgetNamedKnowledge


lookupRealAnimal :
    WithKnowledge (A Int animalId) NoValueKnowledge NoDomainKnowledge NoNamedKnowledge
    -> A (Dict Int String) animals
    -> Maybe (WithKnowledge (A Int animalId) Positive NoDomainKnowledge (IsInDict animalId animals))
lookupRealAnimal namedWantedKey namedDict =
    case ( proveKeyIsInDict (forget namedWantedKey) namedDict, withName provePositive (forget namedWantedKey) ) of
        ( Just isInDictProof, Just isPositive ) ->
            let
                keyWithAllNeededProofs : WithKnowledge (A Int animalId) Positive NoDomainKnowledge (IsInDict animalId animals)
                keyWithAllNeededProofs =
                    setNamedKnowledge isInDictProof isPositive
            in
            Just keyWithAllNeededProofs

        _ ->
            Nothing

ensureCuteness :
    WithKnowledge (A (Dict Int String) animals) NoValueKnowledge AllKnownAnimals NoNamedKnowledge
    -> WithKnowledge (A Int animalId) Positive NoDomainKnowledge (IsInDict animalId animals)
    -> Maybe (WithKnowledge (A Int animalId) Positive IsAValidCuteAnimal NoNamedKnowledge)
ensureCuteness allAnimals animalId =
  let
    animalCommonName = takeValueFromDict animalId (forget allAnimals)
  in
  -- No animals with q can ever be cute, nor any animals with an id over 40, that is just common sense
  if String.contains "q" animalCommonName || raw animalId > 40 then
    Nothing
  else
    Just <| axiomaticallySetDomainKnowledge IsAValidCuteAnimal <| forgetNamedKnowledge animalId



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    pre []
        [ text "current animal: "
        , text <| Debug.toString model.selectedAnimalId
        , text "\n"
        , button
            [ onClick (SetCuteAnimal -1)
            ]
            [ text "set animal to -1 (should not work, negative)" ]
        , text "\n\n"
        , button [ onClick (SetCuteAnimal 1) ] [ text "set animal to 1" ]
        , text "\n\n"
        , button [ onClick (SetCuteAnimal 4) ] [ text "set animal to 4 (should not work since it is not part of the known animals)" ]
        , text "\n\n"
        , button [ onClick (SetCuteAnimal 10) ] [ text "set animal to 10 (should work if you have added it to known animals)" ]
        , text "\n\n"
        , button [ onClick (SetCuteAnimal 10) ] [ text "set animal to 90 (should not work - even if it is added - since oozing death monster is not a cute animal,decided by domain logic)" ]
        , text "\n\n"
        , text <| "Known animals: " ++ (Debug.toString model.animals) ++ "\n\n"
        , button [ onClick (AddToKnownAnimals 10 "Ticklemonster") ] [ text "Add Ticklemonster, id 10 to the list of known animals" ]
        , text "\n\n"
        , button [ onClick (AddToKnownAnimals 90 "Oozing death monster") ] [ text "Add Oozing death monster, id 90 to the list of known animals" ]
        , text "\n\n"
        ]
