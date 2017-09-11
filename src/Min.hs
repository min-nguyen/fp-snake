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
import Control.Monad.State as ST
import qualified SDL.Raw.Timer as Raw
import qualified SDL.Raw.Types as Raw

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)
-- SDL.Rectangle  = Point (x,y) -> (w,h) -> Rectangle Int
-- SDL.Rectangle :: Point V2 a -> V2 a -> SDL.Rectangle a
-- P  :: f a -> Point f a
-- V2 :: a -> a -> V2 a


-- SDL.Texture, (Width, Height)
data Texture = Texture SDL.Texture (V2 CInt)
data Character = Character Texture CharState 
data Game = Game { getTime :: Int } 
type Time = Int

newtype Sprite = Sprite {runSprite :: CharSTATE -> (Texture, [SDL.Rectangle CInt])} 
type SpriteIndex = Int
data CharSTATE = U | D | L | R 
type CharState = ((Sprite, CharSTATE, SpriteIndex, Game), (CInt, CInt))
type CharPosition = (CInt, CInt)
type ElapsedTime = Int

updateState :: Char -> State CharState CharPosition
updateState c = do
  ((sprite, state, index, game), (x, y)) <- ST.get
  let spriteIndexSize = length . snd $ runSprite sprite state
      nextIndex       = if index >= (spriteIndexSize - 1) 
                        then 0 
                        else index + 1
  case c of 
    'a' -> put((sprite, L, nextIndex, game), (x, y))
    'd' -> put((sprite, R, nextIndex, game), (x, y))
    's' -> put((sprite, U, nextIndex, game), (x, y))
    'w' -> put((sprite, D, nextIndex, game), (x, y))

  (_, (x', y')) <-  ST.get
  return (x', y')

updatePosition :: Int -> State CharState CharPosition 
updatePosition ticks = do   
  ((sprite, state, index, game), (x, y)) <- ST.get
  let elapsedTime = (fromIntegral ticks) - (getTime game)
      game' = if elapsedTime > 500 
              then Game $ fromIntegral ticks
              else game
      (x', y') =  if getTime game' /= getTime game 
                  then (50, 50)
                  else (0, 0)
  case state of 
    L -> put((sprite, L, index, game'), (x - x', y))
    R -> put((sprite, R, index, game'), (x + x', y))
    D -> put((sprite, D, index, game'), (x, y + y'))
    U -> put((sprite, U, index, game'), (x, y - y'))           
  (_, (newX, newY)) <- ST.get
  Debug.Trace.trace (show newX) (return (newX, newY))

loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture r filePath = do
  surface <- SDL.loadBMP filePath
  size <- SDL.surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  SDL.surfaceColorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  return (Texture t size)

createSprite :: CharSTATE -> Texture -> [SDL.Rectangle CInt] -> Sprite
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
               SDL.KeycodeUp   -> execState ((updateState 's')) state
               SDL.KeycodeLeft -> execState ((updateState 'a')) state
               SDL.KeycodeRight-> execState ((updateState 'd')) state
               SDL.KeycodeDown -> execState ((updateState 'w')) state
               _ -> state
 

loop :: SDL.Renderer -> Character -> SDL.Window -> IO ()
loop r (Character texture state) window  = do
  poll <- SDL.pollEvents
  -- If elapsed time > 100, update game
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload poll
  SDL.rendererDrawColor r $= V4 maxBound maxBound maxBound maxBound
  SDL.clear r
  -- Update next sprite animation
  ((sprite, state, index, game), (x, y)) <- processEvents' state poll
  -- Update draw position
  
  -- putStrLn (show $ (index, (x', y')))

  -- Get texture and sprite from current state
  let (texture', rects) = runSprite sprite state 
  -- Draw sprite at current position
  renderTexture r texture (P $ V2 x y) (Just $ rects !! index)
  SDL.present r
  unless quit $ loop r (Character texture ((sprite, state, index, game), (x, y))) window




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
  -- Load image
  spriteSheetTexture <- loadTexture renderer "../assets/SF.bmp"

  let draw = renderTexture renderer
  let game = Game 0
  let spriteSize = V2 50 50
      --Define idle clips
      clip1 = SDL.Rectangle (P (V2 0 10)) spriteSize
      clip2 = SDL.Rectangle (P (V2 50 10)) spriteSize
      clip3 = SDL.Rectangle (P (V2 100 10)) spriteSize
      clip4 = SDL.Rectangle (P (V2 150 10)) spriteSize
      --Define character sprite loading functions
      sprite = Sprite $ \state -> case state of 
        _ -> (spriteSheetTexture, [clip1, clip2, clip3, clip4])


  -- loop renderer (True, (0,0)) window 
  loop renderer (Character spriteSheetTexture ((sprite, U, 0, game), (50, 50))) window


  SDL.destroyWindow window
  SDL.quit
