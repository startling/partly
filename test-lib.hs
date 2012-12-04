module Main where
-- base:
import Control.Applicative
import Control.Monad
import System.Exit
-- bytestring:
import qualified Data.ByteString.Lazy as L
-- binary:
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
-- QuickCheck:
import Test.QuickCheck
import Test.QuickCheck.Test
-- partly:
import System.Disk.Partitions.MBR

-- | Generate a random lazy ByteString of some N bytes.
genBytes :: Int -> Gen L.ByteString
genBytes = fmap L.pack . flip vectorOf arbitrary

-- | Test whether a Get succeeds for any bytestring of some n bytes.
succeeds :: Int -> Get a -> Gen Bool
succeeds n get = isRight . runGetOrFail get <$> genBytes n
  where isRight (Right _) = True; isRight _ = False;

-- | Test whether a getter and a putter are the exact inverses for
-- bytestrings of some length.
bijective :: Int -> Get b -> (b -> Put) -> Gen Bool
bijective n get put = (`fmap` genBytes n) $ \bs ->
  bs == (runPut . put . runGet get $ bs)

testSuccess :: String -> Int -> Get t -> IO Bool
testSuccess s i g = do
  putStrLn $ "--> " ++ s
  r <- quickCheckResult $ succeeds i g 
  return $ resultToBool r

testBijective :: String -> Int -> Get b -> (b -> Put) -> IO Bool
testBijective s i g p = do
  putStrLn $ "--> " ++ s
  r <- quickCheckResult $ bijective i g p
  return $ resultToBool r
           
resultToBool :: Result -> Bool
resultToBool (Success _ _ _) = True
resultToBool (GaveUp _ _ _) = False
resultToBool (Failure _ _ _ _ _ _ _) = False

main :: IO ()
main = do
  putStrLn "Testing whether 'runGet' succeeds for any set of bytes:"
  ss <- all id <$> sequence
    [ testSuccess "CHS" 3 (get :: Get CHS)
    , testSuccess "PartitionEntry" 16 (get :: Get PartitionEntry)
    , testSuccess "PartitionTable" 64 (get :: Get PartitionTable)
    , testSuccess "BootRecord" 512 (get :: Get BootRecord) ]
  putStrLn ""
  putStrLn "Testing that 'runGet get' and 'runPut . put' form an isomorphism:"
  bs <- all id <$> sequence
    [ testBijective "CHS" 3 (get :: Get CHS) put
    , testBijective "PartitionEntry" 16 (get :: Get PartitionEntry) put
    , testBijective "PartitionTable" 64 (get :: Get PartitionTable) put
    , testBijective "BootRecord" 512 (get :: Get BootRecord) put ]
  unless (ss && bs) exitFailure
  return ()
