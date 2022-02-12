{-# LANGUAGE RecursiveDo #-}

module Main where

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

-- {-----------------------------------------------------------------------------
--     Main
-- ------------------------------------------------------------------------------}

main :: IO ()
main = startGUI defaultConfig setupCoinMachine


-- {-----------------------------------------------------------------------------
--     Coin Machine
-- ------------------------------------------------------------------------------}

setupCoinMachine :: Window -> UI ()
setupCoinMachine w = void $ do
  w # set' title "HSGy 11"

  let initialMoney = 5 :: Integer
      initialBananas = 0 :: Integer
      initialBananaPrice = 7 :: Integer
      minBananaPrice = 1 :: Integer
      maxBananaPrice = 20 :: Integer

  money <- UI.input # set value (show initialMoney) # set UI.enabled False
  bananas <- UI.input # set value (show initialBananas) # set UI.enabled False
  priceSlider <- UI.input 
                # set (strAttr "type") "range" 
                # set (intAttr "min") minBananaPrice
                # set (intAttr "max") maxBananaPrice
                # set value (show initialBananaPrice) 
  price <- rwString (show initialBananaPrice)

  addButton <- UI.button #. "button" #+ [string "Insert coin"]
  buyButton <- UI.button #. "button" #+ [string "Buy banana"]

  (bMoney, bBananas, bCanBuy, bPrice) <- mdo
    let bCanBuy = (>=) <$> bMoney <*> bPrice
        bCanPriceIncrease = (maxBananaPrice >) <$> bPrice
        eBananaSold = whenE bCanBuy (UI.click buyButton)
        ePriceIncreased = whenE bCanPriceIncrease eBananaSold
    bMoney <-
      accumB
        initialMoney
        ( fmap concatenate . unions $
            [ (+ 1) <$ UI.click addButton,
              subtract <$> bPrice <@ eBananaSold
            ]
        )
    bBananas <- accumB initialBananas ((+ 1) <$ eBananaSold)
    bPrice <- 
      accumB 
        initialBananaPrice
        ( fmap concatenate . unions $
            [ (+ 1) <$ ePriceIncreased,
              (\v -> const $ fromMaybe 0 (readMaybe v :: Maybe Integer)) <$> UI.valueChange priceSlider
            ]
        )
    pure (bMoney, bBananas, bCanBuy, bPrice)

  _ <- sink value (show <$> bMoney) (pure money)
  _ <- sink value (show <$> bBananas) (pure bananas)
  _ <- sink value (show <$> bPrice) (pure priceSlider)
  _ <- sink rwText (show <$> bPrice) (pure price)
  _ <- sink UI.enabled bCanBuy (pure buyButton)

  getBody w
    #+ [ UI.h1 #+ [string "Banana for sCale"],
         UI.div #+ [string "Money: ", pure money],
         UI.div #+ [string "Bananas: ", pure bananas],
         UI.div #+ [string "Price: ", pure priceSlider, pure price],
         UI.div #+ [element addButton],
         UI.div #+ [element buyButton]
       ]

-- {-----------------------------------------------------------------------------
--     Utilities
-- ------------------------------------------------------------------------------}

-- | Text content of an element.
rwText :: ReadWriteAttr Element String String
rwText = mkReadWriteAttr readF writeF
  where
    readF el = callFunction $ ffi "$(%1).text()" el
    writeF s el = runFunction $ ffi "$(%1).text(%2)" el s

-- | Make a @span@ element with a given text content.
rwString :: String -> UI Element
rwString s = mkElement "span" # set rwText s

strAttr :: String -> WriteAttr Element String
strAttr name = mkWriteAttr (set' (attr name))

intAttr :: String -> WriteAttr Element Integer
intAttr name = mkWriteAttr (set' (attr name) . show)