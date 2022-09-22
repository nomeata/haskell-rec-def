import Test.DocTest
main :: IO ()
main = doctest ["--fast", "-package=QuickCheck","Data/"]
