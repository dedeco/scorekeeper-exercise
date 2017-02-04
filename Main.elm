module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


--model


type alias Player =
    { id : Int
    , name : String
    , points : Int
    }


type alias Play =
    { id : Int
    , playerId : Int
    , name : String
    , points : Int
    }


type alias Model =
    { players : List Player
    , name : String
    , playerId : Maybe Int
    , plays : List Play
    , editing : Bool
    }


initModel : Model
initModel =
    { players = []
    , name = ""
    , playerId = Nothing
    , plays = []
    , editing = False
    }



-- update


type Msg
    = Edit Player
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | Delete Play


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input name ->
            { model | name = name }

        Save ->
            if (String.isEmpty model.name) then
                model
            else
                save model

        Cancel ->
            { model | name = "", playerId = Nothing }

        Score player pts ->
            score model player pts

        Edit player ->
            { model
                | name = player.name
                , playerId = Just player.id
                , editing = True
            }

        Delete play ->
            delete model play


delete : Model -> Play -> Model
delete model shot =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == shot.playerId then
                        { player
                            | points = player.points - shot.points
                        }
                    else
                        player
                )
                model.players

        newplays =
            List.filter (\p -> p.id /= shot.id) model.plays
    in
        { model
            | players = newPlayers
            , plays = newplays
        }


score : Model -> Player -> Int -> Model
score model scorer pts =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == scorer.id then
                        { player
                            | points = player.points + pts
                        }
                    else
                        player
                )
                model.players

        play =
            Play (List.length model.plays) scorer.id scorer.name pts
    in
        { model
            | players = newPlayers
            , plays = play :: model.plays
        }


save : Model -> Model
save model =
    case model.playerId of
        Just id ->
            edit model id

        Nothing ->
            add model


add : Model -> Model
add model =
    let
        newPlayer =
            Player (List.length model.players) model.name 0
    in
        { model
            | players = newPlayer :: model.players
            , name = ""
        }


edit : Model -> Int -> Model
edit model id =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == id then
                        { player | name = model.name }
                    else
                        player
                )
                model.players

        newPlays =
            List.map
                (\play ->
                    if play.playerId == id then
                        { play | name = model.name }
                    else
                        play
                )
                model.plays
    in
        { model
            | players = newPlayers
            , plays = newPlays
            , name = ""
            , playerId = Nothing
            , editing = False
        }



-- view


view : Model -> Html Msg
view model =
    div [ class "scoreboard" ]
        [ h1 [] [ text "Score Keeper" ]
        , playerSection model
        , playerForm model
        , playSection model
        ]


playSection : Model -> Html Msg
playSection model =
    div []
        [ playListHeader
        , playList model
        ]


playListHeader : Html Msg
playListHeader =
    header []
        [ div [] [ text "Plays" ]
        , div [] [ text "Points" ]
        ]


playList : Model -> Html Msg
playList model =
    model.plays
        |> List.map play
        |> ul []


play : Play -> Html Msg
play shot =
    li []
        [ i
            [ class "remove"
            , onClick (Delete shot)
            ]
            []
        , div [] [ text (shot.name) ]
        , div [] [ text (toString shot.points) ]
        ]


playerSection : Model -> Html Msg
playerSection model =
    div []
        [ playerListHeader
        , playerList model
        , pointsTotal model
        ]


playerListHeader : Html Msg
playerListHeader =
    header []
        [ div [] [ text "Name" ]
        , div [] [ text "Points" ]
        ]


playerList : Model -> Html Msg
playerList model =
    model.players
        |> List.sortBy .name
        |> List.map player
        |> ul []


player : Player -> Html Msg
player player =
    li []
        [ i
            [ class "edit"
            , onClick (Edit player)
            ]
            []
        , div [ editStyle False ]
            [ text player.name
            ]
        , button
            [ type_ "button"
            , onClick (Score player 2)
            ]
            [ text "2 pts"
            ]
        , button
            [ type_ "button"
            , onClick (Score player 3)
            ]
            [ text "3 pts"
            ]
        , div []
            [ text (toString player.points) ]
        ]


pointsTotal : Model -> Html Msg
pointsTotal model =
    let
        total =
            List.map .points model.players
                |> List.sum
    in
        footer []
            [ div []
                [ text "Total:" ]
            , div
                []
                [ text (toString total) ]
            ]


editStyle : Bool -> Attribute msg
editStyle b =
    if b == True then
        style
            [ ( "backgroundColor", "#F0F8FF" )
            ]
    else
        style
            []


playerForm : Model -> Html Msg
playerForm model =
    Html.form [ onSubmit Save ]
        [ input
            [ type_ "text"
            , placeholder "Add/Edit Player..."
            , onInput Input
            , value model.name
            , editStyle model.editing
            ]
            []
        , button [ type_ "submit" ] [ text "Save" ]
        , button [ type_ "button", onClick Cancel ] [ text "Cancel" ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
