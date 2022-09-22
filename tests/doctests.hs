import Test.DocTest
main = do
    -- Why do I have to call them separately?
    doctest ["--fast", "-package=QuickCheck","Data/Recursive/Bool.hs"]
    doctest ["--fast", "-package=QuickCheck","Data/Recursive/DualBool.hs"]
    doctest ["--fast", "-package=QuickCheck","Data/Recursive/Set.hs"]
    doctest ["--fast", "-package=QuickCheck","Data/Recursive/Map.hs"]
    doctest ["--fast", "-package=QuickCheck","Data/Recursive/Examples.hs"]
