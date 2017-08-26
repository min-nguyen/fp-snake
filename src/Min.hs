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
-- rect = Point (x,y) -> (w,h) -> Rectangle Int
-- SDL.Rectangle :: Point V2 a -> V2 a -> SDL.Rectangle a
-- P  :: f a -> Point f a
-- V2 :: a -> a -> V2 a


-- SDL.Texture, (Width, Height)
data Texture = Texture SDL.Texture (V2 CInt)
data Character = Character Texture CharState 


type CharState = (Bool, (CInt, CInt))
type CharPosition = (CInt, CInt)

updatePosition :: Char -> State CharState CharPosition
updatePosition c = do
  (state, (x, y)) <- ST.get
  case c of 
    'a' -> put(state, (x-1, y))
    'd' -> put(state, (x+1, y))
    's' -> put(state, (x, y-1))
    'w' -> put(state, (x, y+1))
  (_, (st, xy)) <-  ST.get
  traceShow (xy) ( return (st, xy))

test = putStrLn $ show $ evalState (updatePosition 'w') (True, (0,0))

loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture r filePath = do
  surface <- SDL.loadBMP filePath
  size <- SDL.surfaceDimensions surface
  let key = V4 0 maxBound maxBound maxBound
  SDL.surfaceColorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  return (Texture t size)

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
    SDL.KeycodeUp   -> execState (updatePosition 'w') state
    SDL.KeycodeLeft -> execState (updatePosition 'a') state
    SDL.KeycodeRight-> execState (updatePosition 'd') state
    SDL.KeycodeDown -> execState (updatePosition 's') state
    _ -> state

loop :: SDL.Renderer -> Character -> SDL.Window -> IO ()
loop r (Character texture state) window = do
  poll <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload poll
  SDL.rendererDrawColor r $= V4 maxBound maxBound maxBound maxBound
  SDL.clear r
  (bool, (x, y)) <- processEvents' state poll
  putStrLn (show (bool, (x, y)))
  renderTexture r texture (P $ V2 x y) (Just $ SDL.Rectangle (P (V2 0 0)) (V2 100 100) )
  SDL.present r
  traceShow state $ unless quit $ loop r (Character texture (bool, (x, y))) window


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

  let spriteSize = V2 100 100
      clip1 = SDL.Rectangle (P (V2 0 0)) spriteSize
      clip2 = SDL.Rectangle (P (V2 100 0)) spriteSize
      clip3 = SDL.Rectangle (P (V2 0 100)) spriteSize
      clip4 = SDL.Rectangle (P (V2 100 100)) spriteSize
  
  
  -- loop renderer (True, (0,0)) window 
  loop renderer (Character spriteSheetTexture (True, (50, 50))) window


  SDL.destroyWindow window
  SDL.quit
