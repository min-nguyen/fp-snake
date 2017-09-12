{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Foreign.C.Types
import Data.StateVar
import Control.Applicative
import Control.Exception
import Control.Monad (forM, unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans
import Data.Bits
import Data.Data (Data)
import Data.Foldable
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid (First(..))
import Data.Text (Text)
import Data.Typeable
import Debug.Trace
import SDL.Vect
import qualified SDL as SDL
import System.FilePath.Posix
import System.Random
import Control.Monad.State as ST
import qualified SDL.Raw.Timer as Raw
import qualified SDL.Raw.Types as Raw

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 800)

cellWidth, cellHeight :: CInt 
(cellWidth, cellHeight) = (50, 50)


-- SDL.Rectangle  = Point (x,y) -> (w,h) -> Rectangle Int
-- SDL.Rectangle :: Point V2 a -> V2 a -> SDL.Rectangle a
-- P  :: f a -> Point f a
-- V2 :: a -> a -> V2 a

-- SDL.Texture, (Width, Height)
data Texture = Texture SDL.Texture (V2 CInt)

data Game = Game { getTime :: Int, hitCoin :: Bool, hitBody :: Bool } 
type Time = Int
-- Char
data Character = Character Texture CharState 
newtype Sprite = Sprite {runSprite :: Direction -> (Texture, [SDL.Rectangle CInt])} 
type SpriteIndex = Int
data Direction = U | D | L | R 
type CharState = ((Sprite, Direction, SpriteIndex, Game), Body)
type Body = [(CInt, CInt)]

-- Coin
type Coin = ((CInt, CInt), Int)


updateState :: Char -> State CharState Body
updateState c = do
  ((sprite, state, index, game), xs) <- ST.get
  let spriteIndexSize = length . snd $ runSprite sprite state
      nextIndex       = if index >= (spriteIndexSize - 1) 
                        then 0 
                        else index + 1
  case c of 
    'a' -> put((sprite, L, nextIndex, game), xs)
    'd' -> put((sprite, R, nextIndex, game), xs)
    's' -> put((sprite, U, nextIndex, game), xs)
    'w' -> put((sprite, D, nextIndex, game), xs)
  (_, (xs')) <-  ST.get
  return (xs')

updatePosition :: Int -> State CharState Body 
updatePosition ticks = do   
  ((sprite, direction, index, game), xs) <- ST.get
  let elapsedTime = (fromIntegral ticks) - (getTime game)
              -- Restart game timer relative to Raw.getTicks
      game' = if elapsedTime > 100 
                    then game {getTime = (fromIntegral ticks)}
                    else game
              -- Update body 
      (xss, g)  = if getTime game' /= getTime game 
                        then updateBody direction game' xs
                        else (xs, game')
  put((sprite, direction, index, g), xss)        
  (_, xs') <- ST.get
  return xs'

loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture r filePath = do
  surface <- SDL.loadBMP filePath
  size <- SDL.surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  SDL.surfaceColorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  return (Texture t size)

createSprite :: Direction -> Texture -> [SDL.Rectangle CInt] -> Sprite
createSprite s t xs = Sprite $ \s -> (t, xs)


-- Renderer-> Texture -> Point(drawX, drawY) -> Maybe (SDL.Rectangle CInt) -> IO ()
renderTexture :: SDL.Renderer -> Texture -> Point V2 CInt -> Maybe (SDL.Rectangle CInt) -> IO ()
renderTexture r (Texture t size) xy clip =
  let dstSize = maybe size (\(SDL.Rectangle _ size') ->  size') clip
  in SDL.copy r t clip (Just (SDL.Rectangle xy dstSize))
  
processEvents' :: CharState -> [SDL.Event] -> IO (CharState)
processEvents' state events = do 
    ticks <- Raw.getTicks
    let x = foldr (\ev st -> let eventType = SDL.eventPayload ev in case eventType of 
                    SDL.KeyboardEvent eventType -> 
                      processKeyboard' st (SDL.keyboardEventKeysym eventType) 
                        (fromIntegral ticks)
                    _ -> st) state events
        x' = execState (updatePosition $ fromIntegral ticks) x
    return x'

processKeyboard' :: CharState -> SDL.Keysym -> Int -> CharState
processKeyboard' state keySym ticks = 
  case SDL.keysymKeycode keySym of
               SDL.KeycodeUp   -> execState ((updateState 'w')) state
               SDL.KeycodeLeft -> execState ((updateState 'a')) state
               SDL.KeycodeRight-> execState ((updateState 'd')) state
               SDL.KeycodeDown -> execState ((updateState 's')) state
               _ -> state


gameOver :: IO ()     
gameOver = do
  putStrLn "GAME OVER"
  gameOver

loop :: SDL.Renderer -> Character -> Coin -> SDL.Window -> IO ()
loop r (Character texture state) ((coinX, coinY), seed) window  = do
  poll <- SDL.pollEvents
  -- If elapsed time > 100, update game
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload poll
  SDL.rendererDrawColor r $= V4 maxBound maxBound maxBound maxBound
  SDL.clear r
  -- Update next sprite animation -- Update draw position
  ((sprite, state', index, game), xs) <- processEvents' state poll
 
      -- Get texture and sprite from current state
  let (texture', rects) = runSprite sprite state' 
      toDraw = map (\(x, y) -> P $ V2 x y) xs
      rect = Just $ rects !! index
      -- Check if head has hit coin
      head = (drop (length xs - 1) xs) !! 0
      (((coinX', coinY'), seed'), game') = 
        if (fst head == coinX) && (snd head == coinY) 
        then (spawnCoin xs seed, game {hitCoin = True})
        else (((coinX, coinY), seed), game)

  -- Draw sprite at current position
  sequence $ map (\pv2 -> renderTexture r texture pv2 rect) toDraw

  -- Debug printing
  -- sequence $ map (\(g, f) -> (print (g, f))) xs
  -- print ((coinX', coinY'), seed')
  c <- if hitBody game' then gameOver else return ()

  -- Draw Coin
  renderTexture r texture (P $ V2 coinX' coinY') (Just $ 
          SDL.Rectangle (P $ V2 100 100) (V2 cellWidth cellHeight) ) 

  SDL.present r
  unless quit $ loop r (Character texture ((sprite, state', index, game'), xs)) 
                  ((coinX', coinY'), seed') window

boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"

-- to fix : doesn't work with length == 1
updateBody :: Direction -> Game -> Body -> (Body, Game)
updateBody direction game [] = ([], game)
updateBody direction game (x:xs) = 
      (newBody, game'')
      where newBody = (newTail ++ xs ++ newHead)
            newHead = (map f $ drop (length xs - 1) xs)
            f = \(a, b) -> case direction of 
                                U -> (a, b+cellHeight)
                                D -> (a, b-cellHeight)
                                L -> (a-cellWidth, b)
                                R -> (a+cellWidth, b)
            (newTail, game') =  if (hitCoin game) 
                                then ([x], game {hitCoin = False}) 
                                else ([], game)
            game'' = if (newHead !! 0) `elem` xs
                     then game' {hitBody = True}
                     else game' 
            

freeCells :: Body -> Body
freeCells body =  [   (height*cellHeight, width*cellWidth) | 
                      height <- [0..16], 
                      width <- [0..16], 
                      bodyXY <- body,
                      fst bodyXY /= height*cellHeight || snd bodyXY /= width*cellWidth  ]

spawnCoin :: Body -> Int -> ((CInt, CInt), Int)
spawnCoin body seed = 
  let availableCells = freeCells body 
      l = length availableCells
      (st, x) = runState (myRandom $ l) seed
      (st', y) = runState (myRandom $ l) st
  in (availableCells !! ((x + y) `mod` l), st')

myRandom :: Int -> State Int Int
myRandom availableCells = do 
  b <- ST.get 
  put (b * 3 + 9)
  return ((b * 3 + 9) `mod` availableCells)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear


  window  <- SDL.createWindow "My Window" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window
  -- Fill window
  surface <- SDL.getWindowSurface window
  let white =  V4 maxBound maxBound maxBound maxBound 
  SDL.surfaceFillRect surface Nothing white
  -- Renderer
  renderer <- SDL.createRenderer window (-1) SDL.RendererConfig { 
    SDL.rendererType = SDL.AcceleratedRenderer,
    SDL.rendererTargetTexture = False
  }
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  --
  -- Load Texture
  spriteSheetTexture <- loadTexture renderer "../assets/SF.bmp"
  
  let draw = renderTexture renderer
      game = Game 0 False False
      coin = ((500, 500), 2)
      -- Load Snake
      spriteSize = V2 cellWidth cellHeight
      clip1 = SDL.Rectangle (P (V2 0 10)) spriteSize
      clip2 = SDL.Rectangle (P (V2 50 10)) spriteSize
      clip3 = SDL.Rectangle (P (V2 100 10)) spriteSize
      clip4 = SDL.Rectangle (P (V2 150 10)) spriteSize
      --Define character sprite loading functions
      sprite = Sprite $ \state -> case state of 
        _ -> (spriteSheetTexture, [clip1, clip2, clip3, clip4])



  -- loop renderer (True, (0,0)) window 
  loop renderer 
    (Character spriteSheetTexture ((sprite, U, 0, game), [(50, 50),(100, 100)])) 
      coin window


  SDL.destroyWindow window
  SDL.quit
