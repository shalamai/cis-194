import CodeWorld

translatedVerticallyN :: [Picture] -> Picture
translatedVerticallyN [] = blank
translatedVerticallyN (pHead : pTail) = pHead & translated 0 (-2.5) (translatedVerticallyN pTail)

data LightMode = Allow | Warn | WarnForbid | Forbid | Off

trafficLighter :: LightMode -> Picture
trafficLighter mode =
  let greenLight = colored green blackLight
      yellowLight = colored yellow blackLight
      redLight = colored red blackLight
      blackLight = solidCircle 1
      border = translated 0 (-2.5) (rectangle 3 8)
   in case mode of
        Allow -> translatedVerticallyN [greenLight, blackLight, blackLight] & border
        Warn -> translatedVerticallyN [blackLight, yellowLight, blackLight] & border
        WarnForbid -> translatedVerticallyN [blackLight, yellowLight, redLight] & border
        Forbid -> translatedVerticallyN [blackLight, blackLight, redLight] & border
        Off -> translatedVerticallyN [blackLight, blackLight, blackLight] & border

trafficController :: Double -> Picture
trafficController time
  | (round time `mod` 8) < 3 = trafficLighter Allow
  | (round time `mod` 8) < 4 = trafficLighter Warn
  | (round time `mod` 8) < 7 = trafficLighter Forbid
  | (round time `mod` 8) < 8 = trafficLighter WarnForbid
  | otherwise = trafficLighter Off

main :: IO ()
main = animationOf trafficController
