import UI.NCurses
import System.Directory
import System.Process
import Control.Monad.IO.Class

data VimState = Normal { operator :: Char,
                         count :: Int }
              deriving Eq

data WindowState = WindowState { editor :: VimState,
                                 offset :: Integer,
                                 cursorPos :: (Integer, Integer)}
                 | Exit
                 deriving Eq

main = runCurses $ do
           setEcho False
           w <- defaultWindow
           drawLs 0 0 w
           handleEvents w jkHandler (WindowState {editor = Normal { operator = 'j',
                                                                    count = 1},
                                                 offset = 0,
                                                 cursorPos = (0, 0)})
                                    Exit

drawLs :: Integer -> Integer -> Window -> Curses()
drawLs i off w = do ls <- liftIO (getCurrentDirectory >>= listDirectory >>= return . drop (fromInteger off) . filter ((/= '.') . head))
                    updateWindow w clear
                    (_, height) <- screenSize
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

jkHandler :: Event -> WindowState -> Window -> Curses WindowState
jkHandler (EventCharacter 'j') (WindowState {offset=off, cursorPos=(x,i)}) w = drawLs (i + 1) off w >>
                                                                               return WindowState {offset=i + 1, cursorPos=(x, i + 1)}
jkHandler (EventCharacter 'k') (WindowState {offset=off, cursorPos=(x,i)}) w = drawLs (i - 1) off w >>
                                                                               return WindowState {offset=i - 1, cursorPos=(x, i - 1)}
jkHandler (EventCharacter 'l') (WindowState {offset=off, cursorPos=(x,i)}) w = getCurrPath (i + off) >>= setWd >> drawLs 0 0 w >>
                                                                               return WindowState {offset=0, cursorPos=(x, 0)}
jkHandler (EventCharacter 'h') (WindowState {offset=off, cursorPos=(x,i)}) w = setWd ".." >> drawLs 0 0 w >>
                                                                               return WindowState {offset=0, cursorPos=(x, 0)}
jkHandler (EventCharacter 'q') (WindowState {offset=off, cursorPos=(x,i)}) w = return Exit
jkHandler _                    state                                       w = return state

handleEvents :: (Eq a) => Window -> (Event -> a -> Window -> Curses a) -> a -> a -> Curses ()
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
