module Main exposing (main)

import Animals exposing (AllKnownAnimals, addNewKnownAnimal, initAllKnownAnimals, lookupRealAnimal)
import Browser
import CuteAnimals exposing (IsAValidCuteAnimal, proveCuteness)
import Dict exposing (Dict)
import Html exposing (Html, button, pre, text)
import Html.Events exposing (onClick)
import Http
import RefinementProofs.Knowledge exposing (A, NoDomainKnowledge, NoNamedKnowledge, NoValueKnowledge, Proof, WithKnowledge, axiomaticallySetDomainKnowledge, forget, forgetNamedKnowledge, forgetNamedKnowledgeAndName, name, name2, name2WithKnowledge, raw, setNamedKnowledge, withName, withNoKnowledge)
import RefinementProofs.Proofs.DictProofs exposing (IsInDict, proveKeyIsInDict, takeValueFromDict)
import RefinementProofs.Proofs.NumberProofs exposing (Positive, provePositive)
import RefinementProofs.Knowledge exposing (Or)
import RefinementProofs.Proofs.NumberProofs exposing (Zero)
import Birds exposing (IsBird)
import Birds exposing (proveIsBird)
import RefinementProofs.Knowledge exposing (d_makeOr)
import RefinementProofs.Knowledge exposing (k_d_makeOr)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { animals : WithKnowledge (Dict Int String) NoValueKnowledge AllKnownAnimals NoNamedKnowledge
    , selectedAnimalId : Maybe (WithKnowledge Int (Or Positive Zero) (Or IsAValidCuteAnimal IsBird) NoNamedKnowledge)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { animals = initAllKnownAnimals
      , selectedAnimalId = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SelectAnimal Int
    | AddToKnownAnimals Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectAnimal i ->
            case Maybe.map forgetNamedKnowledgeAndName <| name2WithKnowledge (withNoKnowledge i) model.animals handleSelectAnimal of
                Just newId ->
                    ( { model | selectedAnimalId = Just newId }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        AddToKnownAnimals animalId animalName ->
            ( { model | animals = addNewKnownAnimal animalId animalName model.animals }, Cmd.none )


handleSelectAnimal :
    WithKnowledge (A Int animalId) NoValueKnowledge NoDomainKnowledge NoNamedKnowledge
    -> WithKnowledge (A (Dict Int String) animals) NoValueKnowledge AllKnownAnimals NoNamedKnowledge
    -> Maybe (WithKnowledge (A Int animalId) (Or Positive Zero) (Or IsAValidCuteAnimal IsBird) NoNamedKnowledge)
handleSelectAnimal namedId namedDict =
    lookupRealAnimal namedId namedDict
        |> Maybe.andThen (k_d_makeOr (proveCuteness namedDict) (proveIsBird namedDict))
        |> Maybe.map forgetNamedKnowledge



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
            [ onClick (SelectAnimal -1)
            ]
            [ text "set animal to -1 (should not work, negative)" ]
        , text "\n\n"
        , button [ onClick (SelectAnimal 1) ] [ text "set animal to 1" ]
        , text "\n\n"
        , button [ onClick (SelectAnimal 4) ] [ text "set animal to 4 (should not work since it is not part of the known animals)" ]
        , text "\n\n"
        , button [ onClick (SelectAnimal 10) ] [ text "set animal to 10 (should work if you have added it to known animals)" ]
        , text "\n\n"
        , button [ onClick (SelectAnimal 10) ] [ text "set animal to 90 (should not work - even if it is added - since oozing death monster is not a cute animal,decided by domain logic)" ]
        , text "\n\n"
        , button [ onClick (SelectAnimal 100) ] [ text "set animal to 100 (should work after 'horrible howler bird' is added, since it is not cute but a bird)" ]
        , text "\n\n"
        , text <| "Known animals: " ++ Debug.toString model.animals ++ "\n\n"
        , button [ onClick (AddToKnownAnimals 10 "Ticklemonster") ] [ text "Add Ticklemonster, id 10 to the list of known animals" ]
        , text "\n\n"
        , button [ onClick (AddToKnownAnimals 90 "Oozing death monster") ] [ text "Add Oozing death monster, id 90 to the list of known animals" ]
        , text "\n\n"
        , button [ onClick (AddToKnownAnimals 100 "horrible howler bird") ] [ text "Add horrible howler bird, id 100 to the list of known animals" ]
        , text "\n\n"
        ]
