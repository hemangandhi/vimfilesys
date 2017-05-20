import UI.NCurses
import System.Directory

main = do
    runCurses $ do
        setEcho False
        w <- defaultWindow
        handleEvents w jkHandler 1 (- 1)

drawLs :: Integer -> Window -> Curses (IO (Curses ()))
drawLs row w = return $ lsCwdUpdate >>= \cwd_ls -> return $ updateWindow w
                                                          $ foldr (>>) clear
                                                          $ map render_row
                                                          $ zip cwd_ls [0..]
             where render_row (path, idx) = do setAttribute AttributeUnderline (idx == row)
                                               moveCursor idx 20
                                               drawString path
                                               moveCursor 0 0
                   lsCwdUpdate = (getCurrentDirectory >>= listDirectory)

jkHandler :: Event -> Integer -> Window -> Curses Integer
jkHandler (EventCharacter 'j') i w = drawLs (i + 1) w >> return (i + 1)
jkHandler (EventCharacter 'k') i w = drawLs (i - 1) w >> return (i - 1)
jkHandler (EventCharacter 'q') i w = drawLs i w >> return (-1)

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
