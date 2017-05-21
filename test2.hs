import UI.NCurses
import System.Directory
import System.Process
import Control.Monad.IO.Class

main = runCurses $ do
           setEcho False
           w <- defaultWindow
           drawLs 0 w
           handleEvents w jkHandler 0 (- 1)

drawLs :: Integer -> Window -> Curses()
drawLs i w = do ls <- liftIO (getCurrentDirectory >>= listDirectory)
                updateWindow w clear
                mapM (\(dir, idx) -> updateWindow w $ do
                                     setAttribute AttributeUnderline (idx == i)
                                     moveCursor idx 20
                                     drawString dir
                                     moveCursor i 0)
                     $ zip ls [0..]
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

jkHandler :: Event -> Integer -> Window -> Curses Integer
jkHandler (EventCharacter 'j') i w = drawLs (i + 1) w >> return (i + 1)
jkHandler (EventCharacter 'k') i w = drawLs (i - 1) w >> return (i - 1)
jkHandler (EventCharacter 'l') i w = getCurrPath i >>= setWd >> drawLs 0 w >> return 0
jkHandler (EventCharacter 'h') i w = setWd ".." >> drawLs 0 w >> return 0
jkHandler (EventCharacter 'q') i w = drawLs i w >> return (-1)
jkHandler _                    i w = return i

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
