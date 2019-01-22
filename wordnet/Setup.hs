import Distribution.Simple
import System.Directory (getCurrentDirectory)

main = do
    wd <- getCurrentDirectory

    defaultMainWithHooks simpleUserHooks
        { postBuild = \_ _ _ _ -> putStrLn wd
        }
