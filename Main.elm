import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Char exposing (isDigit, isUpper, isLower)

main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model =
  { name : String,
    password : String,
    passwordAgain : String
  }

model : Model
model =
  Model "" "" ""


-- UPDATE

type Msg
  = Name String
  | Password String
  | PasswordAgain String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }
    Password password ->
      { model | password = password }
    PasswordAgain password ->
      { model | passwordAgain = password }


-- VIEW

view : Model -> Html Msg
view model =
  div []
      [ buildInput "text" "Name" Name
      , buildInput "password" "Password" Password
      , buildInput "password" "Password Confirmation" PasswordAgain
      , viewValidation model
      ]


buildInput : String -> String -> (String -> Msg) -> Html Msg
buildInput t p m =
  input [ type_ t, placeholder p, onInput m] []


viewValidation : Model -> Html Msg
viewValidation model =
  let
    (color, message) =
      if validate model.password 8 then
        ("green", "OK")
      else
        ("red", "Password do not match!")
  in
    div [ style [("color", color)] ] [ text message ]


validate : String -> Int -> Bool
validate password chars =
  let
    funcs = [hasLowerCase, hasUpperCase, hasNumber, containsNumberOfCharacters 8]
  in
    List.all (\result -> result == True) <| List.map (\func -> func password) funcs


containsNumberOfCharacters : Int -> String -> Bool
containsNumberOfCharacters chars password = String.length password >= chars


hasLowerCase : String -> Bool
hasLowerCase password = String.any isLower password


hasUpperCase : String -> Bool
hasUpperCase password = String.any isUpper password


hasNumber : String -> Bool
hasNumber password = String.any isDigit password
