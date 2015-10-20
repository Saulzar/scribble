{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex
import Reflex.Gloss.Scene
import Reflex.Monad.Time
import Reflex.Animation

import Data.Foldable
import Data.Traversable
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
  
  
pen :: Reflex t => Vector -> Behavior t Color -> Scene t (Event t Picture)
pen size currentCol = mdo
  start <- mouseDown LeftButton <$> targetRect (constant size)
  stop <-  mouseUp LeftButton <$> targetWindow

  drawing <- widgetHold (return never) $ leftmost
    [ scrible stop <$ start
    , return never <$ stop 
    ]
  
  return (switch (current drawing))
    
  where
    scrible stop = do
      updates <- observe =<< localMouse
      points <- current <$> foldDyn (:) [] (clampSize <$> updates)
      
      let drawing = drawPath <$> points <*> currentCol
      
      render $ drawing
      return (tag drawing stop) 
      
    clampSize (x, y) = (clamp (-w/2, w/2) x, clamp (-h/2, h/2) y)
      where (w, h) = size

    drawPath points c = color c $ line points
      
palette :: Reflex t => [Color] -> Scene t (Behavior t Color)
palette colors = do
  
  clicks <- forM indexed $ \(i, c) -> ith i $ do
    target <- targetRect (pure (30, 30))
    render $ pure (drawColor c)
    (click, _) <- clicked LeftButton target
    return (c <$ click)
 
  currentCol <- hold (head colors) (leftmost clicks)
  render (drawSelected <$> currentCol)
  return currentCol
      
  where
    height = fromIntegral (length colors) * 40 + 50
    ith i = translation (0, i * 40 - height/2)
    indexed = zip [1..] colors
    
    drawColor c    =  color c $ rectangleSolid 30 30   
    drawSelected c = translate 0 (-(height/2) - 25) $ color c $ rectangleSolid 40 40
    
    
    
    
canvas :: Reflex t => Vector -> Scene t ()
canvas  size = do
  
  currentCol <- translation (200,  0) 
      $ palette [black, red, green, blue]
      
  scribble <- pen size currentCol
  
  drawings <- current <$> foldDyn (:) [] scribble
  render $ renderCanvas <$> pure size <*> drawings
  
  
 

widget :: Reflex t => Scene t  ()
widget = canvas (300, 300)


main = playSceneGraph display background frequency widget
  where 
    display = InWindow "Scribble3" (600, 600) (0, 0)
    background = white
    frequency = 30