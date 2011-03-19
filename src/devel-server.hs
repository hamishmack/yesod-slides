import Yesod (develServer)
import System.Directory (doesFileExist)

main :: IO ()
main = do
    found <- doesFileExist "Slides.hs"
    if found
        then develServer 3000 "Slides" "withSlides"
        else putStrLn "Slides.hs not found.  Please run this in the 'src' directory."
