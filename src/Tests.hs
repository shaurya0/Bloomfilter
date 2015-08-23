import Test.HUnit
import Data.Serialize
import Data.List
import JenkinsOneAtATime
import MurmurHash
import Data.Word(Word32)
import qualified Data.ByteString as BS

seed :: Word32
seed = 23

inputs :: [Word32]
inputs = [34234,92810, 98573, 579238, 878492, 34234,92810, 98573, 579238, 878492]

testOutputs :: [Word32]
testOutputs = [3794510033, 731782087, 2230277540, 1127214474, 1092818270,
                3259219417, 328606221, 3610261340, 957888653, 3723262236]

testText :: [String]
testText = ["for (jenkinsOneAtATime 34234),",
    "for (jenkinsOneAtATime 92810),",
    "for (jenkinsOneAtATime 98573),",
    "for (jenkinsOneAtATime 579238),",
    "for (jenkinsOneAtATime 878492),",
    "for (murmurHash2 34234),",
    "for (murmurHash2 92810),",
    "for (murmurHash2 98573),",
    "for (murmurHash2 579238),",
    "for (murmurHash2 878492),"]

testInputs :: [BS.ByteString]
testInputs = map (BS.reverse . encode) inputs

testCalls :: [(BS.ByteString -> Word32)]
testCalls =[jenkinsOneAtATime,
    jenkinsOneAtATime,
    jenkinsOneAtATime,
    jenkinsOneAtATime,
    murmurHash2 seed,
    murmurHash2 seed,
    murmurHash2 seed,
    murmurHash2 seed,
    murmurHash2 seed]

testCases :: [Test]
testCases = map (\(txt, outp, inp, fcn) -> TestCase $ (assertEqual txt outp $ fcn inp) ) $ zip4 testText testOutputs testInputs testCalls

testNames :: [String]
testNames = ["test1"
    ,"test2"
    ,"test3"
    ,"test4"
    ,"test5"
    ,"test6"
    ,"test7"
    ,"test8"
    ,"test9"
    ,"test10"]

tests = TestList $ map (\(name, tcase) -> TestLabel name tcase) $ zip testNames testCases

run = runTestTT tests
