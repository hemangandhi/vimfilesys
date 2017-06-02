import UI.NCurses
import System.Directory
import System.Process
import Control.Monad.IO.Class
import Data.Char

data VimState = Normal { operator :: Char,
                         count :: Int }
              deriving Eq

data WindowState = WindowState { editor :: VimState,
                                 offset :: Integer,
                                 cursorPos :: (Integer, Integer)}
                 | Exit
                 deriving Eq

type EventHandler a = Event -> a -> Window -> Curses a

main = runCurses $ do
           setEcho False
           w <- defaultWindow
           drawLs 0 0 w
           handleEvents w (updateEditor jkHandler) (WindowState {editor = Normal { operator = 'e',
                                                                                   count = 0},
                                                                 offset = 0,
                                                                 cursorPos = (0, 0)})
                                                   Exit

drawLs :: Integer -> Integer -> Window -> Curses()
drawLs i off w = do ls <- liftIO (getCurrentDirectory >>= listDirectory >>= return . drop (fromInteger off))
                    updateWindow w clear
                    (height, _) <- screenSize
                    mapM (\(dir, idx) -> do isDir <- liftIO (makeAbsolute dir >>= doesDirectoryExist)
                                            updateWindow w $ do
                                                setAttribute AttributeUnderline (idx == i)
                                                setAttribute AttributeBold isDir
                                                moveCursor idx 20
                                                drawString dir
                                                moveCursor i 0)
                         $ zip ls [0..(height - 1)]
                    render

setWd :: FilePath -> Curses ()
setWd path = liftIO $ do abs_pth <- makeAbsolute path
                         isDir <- doesDirectoryExist abs_pth
                         if isDir then setCurrentDirectory abs_pth
                                  else (spawnCommand ("urxvt -e zsh -c \"vim "
                                                      ++ abs_pth ++ "\"")
                                        >> return ())

getCurrPath :: Integer -> Curses FilePath
getCurrPath i = liftIO $ getCurrentDirectory >>= listDirectory
                                             >>= return . (!! (fromInteger i))

heightOffset :: Integer -> Integer -> Curses (Integer, Integer)
heightOffset off curs = do (height, _) <- screenSize
                           if curs >= height
                           then return (off + 1, height - 1)
                           else if curs < 0
                                then if off == 0 then return (off, 0) else return (off - 1, 0)
                                else return (off, curs)

jkHandler :: Event -> WindowState -> Window -> Curses WindowState
jkHandler (EventCharacter 'j') (WindowState {offset=off,
                                             editor=ed,
                                             cursorPos=(x,i)}) w = heightOffset off (i + 1) >>=
                                                                   \(no, nc) ->
                                                                       drawLs nc no w >>
                                                                       return WindowState {offset=off,
                                                                                           editor=ed,
                                                                                           cursorPos=(x, nc)}
jkHandler (EventCharacter 'k') (WindowState {offset=off,
                                             editor=ed,
                                             cursorPos=(x,i)}) w = heightOffset off (i - 1) >>=
                                                                   \(no, nc) ->
                                                                       drawLs nc no w >>
                                                                       return WindowState {offset=off,
                                                                                           editor=ed, cursorPos=(x, nc)}
jkHandler (EventCharacter 'l') (WindowState {offset=off,
                                             editor=ed,
                                             cursorPos=(x,i)}) w = getCurrPath (i + off) >>= setWd >> drawLs 0 0 w >>
                                                                   return WindowState {offset=0, editor=ed, cursorPos=(x, 0)}
jkHandler (EventCharacter 'h') (WindowState {offset=off,
                                             editor=ed,
                                             cursorPos=(x,i)}) w = setWd ".." >> drawLs 0 0 w >>
                                                                   return WindowState {offset=0, editor=ed, cursorPos=(x, 0)}
jkHandler (EventCharacter 'q') (WindowState {offset=off,
                                             editor=ed,
                                             cursorPos=(x,i)}) w = return Exit
jkHandler (EventCharacter 'H') (WindowState {offset=off,
                                             editor=ed,
                                             cursorPos=(x,i)}) w = drawLs 0 off w >> return WindowState {offset=off,
                                                                                                         editor=ed,
                                                                                                         cursorPos=(x, 0)}
jkHandler (EventCharacter 'L') (WindowState {offset=off,
                                             editor=ed,
                                             cursorPos=(x,i)}) w = screenSize >>=
                                                                   \(h, _) ->
                                                                       drawLs 0 off w >> return WindowState {offset=off,
                                                                                                             editor=ed,
                                                                                                             cursorPos=(x, 0)}
jkHandler _                    state                           w = return state

updateEditor :: (EventHandler WindowState) -> EventHandler WindowState
updateEditor ev = \e s@(WindowState {editor=ed}) w ->
                      case e of
                          (EventCharacter ec) | isDigit ec -> return s {editor = ed {count = (count ed) * 10 + (digitToInt ec)}}
                                              | otherwise -> foldr (>>) (return . id) $
                                                                 map (\_ -> ev e s w >>=
                                                                      \n@(WindowState {offset=off, cursorPos=p}) ->
                                                                          return $ if n == s then s
                                                                                   else n {editor = Normal {operator = 'e',
                                                                                                            count = 0}})
                                                                 [1..(count ed)]

handleEvents :: (Eq a) => Window -> (EventHandler a) -> a -> a -> Curses ()
handleEvents w f start stop = loop start
                            where loop st = do
                                    ev <- getEvent w Nothing
                                    case ev of
                                        Nothing  -> loop st
                                        Just ev' -> let c_next = f ev' st w in do
                                            next <- c_next
                                            if next == stop
                                                then return ()
                                                else loop next
