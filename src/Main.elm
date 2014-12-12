import Graphics.Element (Element)
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Signal
import String (toInt, left)
import Time (fps, inSeconds)
import Window

main : Signal Element
main = Signal.map3 sceneElement mmrSignal inchesSignal Window.dimensions

sceneElement : Int -> Float -> (Int, Int) -> Element
sceneElement mmr inches (w, h) = toElement w h (scene mmr inches)

scene : Int -> Float -> Html
scene mmr inches = div
          [ id "content" ]
          [ h1 [ id "heading" ] [ text "Epeen Calculator" ]
          , mmrInput mmr
          , if inches > 0 then display inches else text ""
          , case choosePicture mmr of
             Nothing -> text ""
             Just url -> img [ src url, class "wow" ] []
          , footer ]

mmrInput : Int -> Html
mmrInput mmr = input [ id "mmrInput"
                     , placeholder "Enter your MMR"
                     , value (if mmr == 0 then "" else toString mmr)
                     , on "input" targetValue (Signal.send mmrChannel)
                     ] []

display : Float -> Html
display inches = div [] [ div  [ id "inchResult" ] [text <| (left 4 <| toString <| inches) ++ "in"]
                        , div  [ id "ruler", style [("width", (toString inches) ++ "in")] ]  []
                        ]

footer : Html
footer = div [ id "footer" ] [ text "Sources:"
                             , br [] []
                             , a [ href "http://www.reddit.com/r/DotA2/comments/2124az/ranked_mmr_survey_results_update/" ] [ text "What-A-Baller's MMR survey"]
                             , br [] []
                             , a [ href "http://www.ncbi.nlm.nih.gov/pubmed/23841855" ] [ text "Herbenick et al. (2013)"]
                             ]

mmrChannel : Signal.Channel String
mmrChannel = Signal.channel ""

isOk : Result a b -> Bool
isOk r = case r of
          Ok _  -> True
          Err _ -> False

undefined = undefined

fromOk : Result err a -> a
fromOk r = case r of
            Ok s  -> s
            Err _ -> undefined

mmrSignal : Signal Int
mmrSignal = (Signal.subscribe mmrChannel)
              |> Signal.map (\s -> if s == "" then "0" else s)
              |> Signal.map toInt
              |> Signal.keepIf isOk (Ok 0)
              |> Signal.map fromOk

smoothFollow : Signal Float -> Signal Float
smoothFollow s = let towards (target, delta) current = current + (target - current) * 1.2 * (inSeconds delta)
                 in Signal.foldp towards 0 <| Signal.map2 (,) s (fps 25)

-- Sources:
--
-- Herbenick et al. (2013)
mmrToInch : Float -> Float
mmrToInch mmr = clamp 0 20 <| ((mmr - 3586) / 627) * 1.04 + 5.57

inchesSignal : Signal Float
inchesSignal = smoothFollow <| Signal.map (toFloat >> mmrToInch) mmrSignal

choosePicture : Int -> Maybe String
choosePicture mmr = case mmr of
                      322 -> Just "images/solo.png"
                      9001 -> Just "images/vegeta.png"
                      _ -> if mmr < 3000
                           then Nothing
                           else case mmr % 7 of
                                  0 -> Just "images/bruno.png"
                                  1 -> Just "images/godz.png"
                                  2 -> Just "images/ld.png"
                                  3 -> Just "images/basskip.png"
                                  4 -> Just "images/synderen.png"
                                  5 -> Just "images/dendi.png"
                                  6 -> Just "images/2gd.png"
