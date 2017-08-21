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

type CharState = (Bool, (Int, Int))
type CharPosition = (Int, Int)




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


processEvents' :: CharState -> [SDL.Event] -> IO (CharState)
processEvents' state events = do 
    let x = foldr (\ev st -> let eventType = SDL.eventPayload ev in case eventType of 
                    SDL.KeyboardEvent eventType -> processKeyboard' st $ SDL.keyboardEventKeysym eventType
                    _ -> st) state events 
    return x

processKeyboard' :: CharState -> SDL.Keysym -> CharState
processKeyboard' state keySym = 
  case SDL.keysymKeycode keySym of
    SDL.KeycodeUp -> execState (updatePosition 'w') state
    _ -> state

processEvents :: CharState -> [SDL.Event] -> [IO ()]
processEvents state events = map (\event -> 
  let eventType = SDL.eventPayload event in 
  case eventType of 
    SDL.KeyboardEvent eventType -> processKeyboard state $ SDL.keyboardEventKeysym eventType
    _ -> return ()) events

processKeyboard :: CharState -> SDL.Keysym -> IO()
processKeyboard state keySym = 
  case SDL.keysymKeycode keySym of
    SDL.KeycodeUp -> return ()--return $ execState (updatePosition 'w') state
    _ -> return ()

loop :: CharState -> SDL.Window -> IO ()
loop state window = do
  poll <- SDL.pollEvents
  let quit = elem SDL.QuitEvent $ map SDL.eventPayload poll
  state' <- processEvents' state poll
  SDL.updateWindowSurface window
  traceShow state $ unless quit $ loop state' window

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window  <- SDL.createWindow "My Window" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window
  -- Fill window
  surface <- SDL.getWindowSurface window
  let white =  V4 maxBound maxBound maxBound maxBound 
  SDL.surfaceFillRect surface Nothing white
  -- Load image
  helloWorld <- SDL.loadBMP "../assets/SF.bmp"
  SDL.surfaceBlit helloWorld Nothing surface Nothing
  -- Viewport

  -- Update window
  let state = (True, (0,0))
  let event = SDL.Event
  loop state window
  -- let loop = do
  --     poll <- SDL.pollEvents
  --     let quit = elem SDL.QuitEvent $ map SDL.eventPayload poll
  --     --sequence $ processEvents state poll
  --     state <- processEvents' state poll
  --     SDL.updateWindowSurface window
  --     traceShow state $ unless quit loop
  -- loop

  SDL.destroyWindow window
  SDL.quit
