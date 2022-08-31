import Test.DocTest
main = doctest ["--fast", "-package=QuickCheck", "-package=deepseq", "Data/"]
