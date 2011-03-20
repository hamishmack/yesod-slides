import Slides (withSlides)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = withSlides $ run 3000
