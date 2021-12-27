import Build_doctests
import Data.Foldable (traverse_)
import System.Environment.Compat (unsetEnv)
import Test.DocTest (doctest)

main :: IO ()
main = do
    doctest $ flags_exe_18A <> pkgs_exe_18A <> module_sources_exe_18A
    doctest $ flags_exe_18B <> pkgs_exe_18B <> module_sources_exe_18B
