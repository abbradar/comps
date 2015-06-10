import Data.Map (Map)
import qualified Data.Map as Map
import Parser
import Rules

trimEnd :: Rules -> Rules
trimEnd = Map.map removeEnd . simplifyAll
  where removeEnd (T a) = Empty
        removeEnd (Star a) = Star $ removeEnd a
        removeEnd (Sum a b) = Sum (removeEnd a) (removeEnd b)
        removeEnd (Product a b) = Product a (removeEnd b)
        removeEnd a = a

main :: IO ()
main = do
  rs <- readFile "rules_2"
  (Grammar _ rules) <- case parse grammar rs of
   Right r -> return r
   Left e -> fail $ show e
  putStrLn $ show $ Map.map toString $ simplifyAll rules
  putStrLn $ show $ Map.map toString $ simplifyAll $ trimEnd rules
