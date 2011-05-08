{-# LANGUAGE CPP #-}
#if PRODUCTION
import Slides (withSlides)
import Network.Wai.Handler.Warp (run)
import Paths_yesod_slides

main :: IO ()
main = do
    binDir <- getBinDir
    withSlides binDir $ run 3000
#else
import Slides (withSlides)
import System.IO (hPutStrLn, stderr)
import Network.Wai.Middleware.Debug (debug)
import Network.Wai.Handler.Warp (run)
import Paths_yesod_slides

main :: IO ()
main = do
    let port = 3000
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    binDir <- getBinDir
    withSlides binDir $ run port . debug
#endif
