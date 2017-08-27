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


screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)
-- SDL.Rectangle  = Point (x,y) -> (w,h) -> Rectangle Int
-- SDL.Rectangle :: Point V2 a -> V2 a -> SDL.Rectangle a
-- P  :: f a -> Point f a
-- V2 :: a -> a -> V2 a


-- SDL.Texture, (Width, Height)
data Texture = Texture SDL.Texture (V2 CInt)
data Character = Character Texture CharState 


newtype Sprite = Sprite {runSprite :: CharSTATE -> (Texture, [SDL.Rectangle CInt])} 
type SpriteIndex = Int
data CharSTATE = IDLE
type CharState = ((Sprite, CharSTATE, SpriteIndex), (CInt, CInt))
type CharPosition = (CInt, CInt)

updatePosition :: Char -> State CharState CharPosition
updatePosition c = do
  ((sprite, state, index), (x, y)) <- ST.get
  let spriteIndexSize = length . snd $ runSprite sprite state
      nextIndex       = if index >= (spriteIndexSize - 1) 
                        then 0 
                        else index + 1
  case c of 
    'a' -> put((sprite, state, nextIndex), (x-5, y))
    'd' -> put((sprite, state, nextIndex), (x+5, y))
    's' -> put((sprite, state, nextIndex), (x, y-5))
    'w' -> put((sprite, state, nextIndex), (x, y+5))
  (_, (st, xy)) <-  ST.get
  traceShow (st, xy) ( return (st, xy))


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
    let x = foldr (\ev st -> let eventType = SDL.eventPayload ev in case eventType of 
                    SDL.KeyboardEvent eventType -> processKeyboard' st $ SDL.keyboardEventKeysym eventType
                    _ -> st) state events 
    return x

processKeyboard' :: CharState -> SDL.Keysym -> CharState
processKeyboard' state keySym = 
  case SDL.keysymKeycode keySym of
    SDL.KeycodeUp   -> execState (updatePosition 's') state
    SDL.KeycodeLeft -> execState (updatePosition 'a') state
    SDL.KeycodeRight-> execState (updatePosition 'd') state
    SDL.KeycodeDown -> execState (updatePosition 'w') state
    _ -> state

loop :: SDL.Renderer -> Character -> SDL.Window -> IO ()
loop r (Character texture state) window = do
  poll <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload poll
  SDL.rendererDrawColor r $= V4 maxBound maxBound maxBound maxBound
  SDL.clear r
  -- Update next sprite animation, index and draw position
  ((sprite, state, index), (x, y)) <- processEvents' state poll
  putStrLn (show $ (index, (x, y)) )
  -- Get texture and sprite from current state
  let (texture', rects) = runSprite sprite state 
  -- Draw sprite at current position
  renderTexture r texture (P $ V2 x y) (Just $ rects !! index)
  SDL.present r
  unless quit $ loop r (Character texture ((sprite, state, index), (x, y))) window



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

  let spriteSize = V2 50 90
      --Define idle clips
      clip1 = SDL.Rectangle (P (V2 0 10)) spriteSize
      clip2 = SDL.Rectangle (P (V2 50 10)) spriteSize
      clip3 = SDL.Rectangle (P (V2 100 10)) spriteSize
      clip4 = SDL.Rectangle (P (V2 150 10)) spriteSize
      --Define character sprite loading functions
      sprite = Sprite $ \state -> case state of 
        IDLE -> (spriteSheetTexture, [clip1, clip2, clip3, clip4])


  -- loop renderer (True, (0,0)) window 
  loop renderer (Character spriteSheetTexture ((sprite, IDLE, 0), (50, 50))) window


  SDL.destroyWindow window
  SDL.quit
