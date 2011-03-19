import Slides (withSlide)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = withSlide $ run 3000
