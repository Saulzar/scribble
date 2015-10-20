{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex
import Reflex.Gloss.Scene
import Reflex.Monad.Time
import Reflex.Animation


import Reflex.Monad

import Graphics.Gloss

import Widgets


renderCanvas :: Vector -> [Picture] -> Picture
renderCanvas (sx, sy) drawings = mconcat 
  [ color black $ rectangleWire sx sy  
  , mconcat drawings
  ]
    
    
canvas :: Reflex t => Vector -> Scene t ()
canvas  size = do
  drawings <- hold [] never
  target <- targetRect (constant size)
  render $ renderCanvas <$> pure size <*> drawings
  
 

widget :: Reflex t => Scene t  ()
widget = canvas (300, 300)


main = playSceneGraph display background frequency widget
  where 
    display = InWindow "Scribble1" (600, 600) (0, 0)
    background = white
    frequency = 30