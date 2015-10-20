{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex
import Reflex.Gloss.Scene
import Reflex.Monad.Time
import Reflex.Animation


import Reflex.Monad

import Graphics.Gloss

import Widgets


foldMerge :: MonadReflex t m => a -> [Event t (a -> a)] -> m (Dynamic t a)
foldMerge initial events = foldDyn ($) initial (mergeWith (.) events) 

renderCanvas :: Vector -> [Picture] -> Picture
renderCanvas (sx, sy) drawings = mconcat 
  [ color black $ rectangleWire sx sy  
  , mconcat drawings
  ]
  
  
pen :: Reflex t => Vector -> Scene t (Event t Picture)
pen size = mdo
  start <- mouseDown LeftButton <$> targetRect (constant size)
  stop <-  mouseUp LeftButton <$> targetWindow

  drawing <- widgetHold (return never) $ leftmost
    [ scrible stop <$ start
    , return never <$ stop 
    ]
  
  return (line <$> switch (current drawing))
    
  where
    scrible stop = do
      updates <- observe =<< localMouse
      points <- current <$> foldDyn (:) [] updates
      
      render $ line <$> points
      
      return (tag points stop) 
      

    
canvas :: Reflex t => Vector -> Scene t ()
canvas  size = do
  
  scribble <- pen size
  
  drawings <- current <$> foldDyn (:) [] scribble
  render $ renderCanvas <$> pure size <*> drawings
  
 

widget :: Reflex t => Scene t  ()
widget = canvas (300, 300)


main = playSceneGraph display background frequency widget
  where 
    display = InWindow "Scribble2" (600, 600) (0, 0)
    background = white
    frequency = 30