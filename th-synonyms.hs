import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Char

type Synonyms = Map.Map String (Set.Set String)

mapSynonyms :: [(String, String)] -> Synonyms
mapSynonyms = mapsy' Map.empty
    where
        mapsy' mapped [] = mapped
        mapsy' mapped ((word1, word2):xs) = mapsy' (Map.alter (addToMaybeSet word2) word1 mapped) xs
        addToMaybeSet x (Just s) = Just (Set.insert x s)
        addToMaybeSet x Nothing = Just (Set.singleton x)

toLowerCaseWords :: String -> [String]
toLowerCaseWords = words . map toLower

gZip :: [a] -> [b] -> [(Maybe a, Maybe b)]
gZip a b = zip' a b []
    where
        zip' [] [] z = z
        zip' (a:_) [] z = (Just a, Nothing):z
        zip' [] (b:_) z = (Nothing, Just b):z
        zip' (a:as) (b:bs) z = (Just a, Just b):zip' as bs z

areSynonyms :: Synonyms -> (String, String) -> Bool
areSynonyms synonyms (word1, word2) = isSynonym word1 word2 || isSynonym word2 word1
    where
        isSynonym w s = Set.member s $ getSynonymsFor w
        getSynonymsFor w = Map.findWithDefault Set.empty w synonyms

isCorrect :: Synonyms -> (String, String) -> Bool
isCorrect synonyms (expected, actual) = all match pairedWords
    where
        match (Just a, Just b) = a == b || areSynonyms synonyms (a, b)
        match _ = False
        pairedWords = gZip (toLowerCaseWords expected) (toLowerCaseWords actual)

correctAnswers :: [(String, String)] -> [(String, String)] -> [Bool]
correctAnswers = map . isCorrect . mapSynonyms

{--
At Top Hat, an answer that doesn’t match exactly may be correct
if it uses synonyms of the right answer.

Given a list of tuples of words that are synonyms
and a list of expected/actual answer tuples,
return a list of booleans indicating if each answer is correct.


Given:
--}
synonyms = [("quick", "fast"), ("runs", "sprints"), ("runner", "athlete")]
answers = [
        ("Usain Bolt is a fast athlete", "Usain Bolt is a quick runner"),
        ("The runner sprints to the finish", "The athlete runs to the finish"),
        ("The long jump record is 8.9 metres", "The high jump record is 8.9 metres")
    ]
{--
Output: correctAnswers synonyms answers
[True,True,False]

### ASSUMPTIONS ###
# transitive i.e. a -> b -> c => a -> c (false)
# reflexive i.e. a -> b => b -> a       (true)
# always one word i.e. no phrases       (true)
# case sensitive                        (false)
--}
