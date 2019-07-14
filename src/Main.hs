module Main where

import Control.Monad (guard)

data Subject = Dutch 
             | Maths
             | Chemistry
             | History
             deriving (Eq, Show)

data FirstName = Simon
               | Peter
               | Steven
               | Karl
               deriving (Eq, Show)

data MrDutch = MrDutch FirstName Subject Subject deriving (Eq, Show)

data MrEnglish = MrEnglish FirstName Subject Subject deriving (Eq, Show)

data MrPainter = MrPainter FirstName Subject Subject deriving (Eq, Show)

data MrWriter = MrWriter FirstName Subject Subject deriving (Eq, Show)

data School = School MrDutch MrEnglish MrPainter MrWriter deriving (Eq, Show)


uniqueNames :: FirstName -> FirstName -> FirstName -> FirstName -> Bool
uniqueNames a b c d = (a /= b) && (a /= c) && (a /= d) && (b /= c) && (b /= d) && (c /= d)

differentSubjs :: Subject -> Subject -> Bool
differentSubjs s1 s2 = (s1 /= s2)

nameslist :: [FirstName]
nameslist = [Simon, Peter, Steven, Karl]

subjectslist :: [Subject]
subjectslist = [Dutch, Maths, Chemistry, History]

oneMathsTeacher :: Subject -> Subject -> Subject -> Subject -> Subject -> Subject -> Subject -> Subject -> Bool
oneMathsTeacher ds1 ds2 es1 es2 ps1 ps2 ws1 ws2 = let allSubjs = [ds1, ds2, es1, es2, ps1, ps2, ws1, ws2]
                                                   in (length $ (filter (\x -> x == Maths) allSubjs)) == 1

threeDutchTeachers :: Subject -> Subject -> Subject -> Subject -> Subject -> Subject -> Subject -> Subject -> Bool
threeDutchTeachers ds1 ds2 es1 es2 ps1 ps2 ws1 ws2 = let allSubjs = [ds1, ds2, es1, es2, ps1, ps2, ws1, ws2]
                                                      in (length $ (filter (\x -> x == Dutch) allSubjs)) == 3

twoChemistryTeachers :: Subject -> Subject -> Subject -> Subject -> Subject -> Subject -> Subject -> Subject -> Bool
twoChemistryTeachers ds1 ds2 es1 es2 ps1 ps2 ws1 ws2 = let allSubjs = [ds1, ds2, es1, es2, ps1, ps2, ws1, ws2]
                                                        in (length $ (filter (\x -> x == Chemistry) allSubjs)) == 2

twoHistoryTeachers :: Subject -> Subject -> Subject -> Subject -> Subject -> Subject -> Subject -> Subject -> Bool
twoHistoryTeachers ds1 ds2 es1 es2 ps1 ps2 ws1 ws2 = let allSubjs = [ds1, ds2, es1, es2, ps1, ps2, ws1, ws2]
                                                      in (length $ (filter (\x -> x == History) allSubjs)) == 2

englishIsNotSimon :: FirstName -> Bool
englishIsNotSimon en = en /= Simon

englishTeachesHistory :: Subject -> Subject -> Bool
englishTeachesHistory es1 es2 = (es1 == History) || (es2 == History)

simonTeachesHistory :: FirstName -> Subject -> Subject -> FirstName -> Subject -> Subject -> FirstName -> Subject -> Subject -> Bool
simonTeachesHistory dn ds1 ds2 pn ps1 ps2 wn ws1 ws2 = case dn of
                                                         Simon -> (ds1 == History) || (ds2 == History)
                                                         _ -> case pn of
                                                                Simon -> (ps1 == History) || (ps2 == History)
                                                                _ -> case wn of
                                                                       Simon -> (ws1 == History) || (ws2 == History)
                                                                       _ -> False

stevenTeachesChemistry :: FirstName -> Subject -> Subject -> Bool
stevenTeachesChemistry Steven Chemistry Chemistry = True
stevenTeachesChemistry Steven Chemistry _ = True
stevenTeachesChemistry Steven _ Chemistry = True
stevenTeachesChemistry Steven _ _ = False
stevenTeachesChemistry _ _ _ = True

peterDoesNotTeachDutch :: FirstName -> Subject -> Subject -> Bool
peterDoesNotTeachDutch Peter Dutch Dutch = False
peterDoesNotTeachDutch Peter Dutch _ = False
peterDoesNotTeachDutch Peter _ Dutch = False
peterDoesNotTeachDutch Peter _ _ = True
peterDoesNotTeachDutch _ _ _ = True

dutchDoesNotShareWithPainter :: Subject -> Subject -> Subject -> Subject -> Bool
dutchDoesNotShareWithPainter ds1 ds2 ps1 ps2 = (ds1 /= ps1) && (ds1 /= ps2) && (ds2 /= ps1) && (ds2 /= ps2)

dutchDoesNotShareWithKarl :: Subject -> Subject -> FirstName -> Subject -> Subject -> Bool
dutchDoesNotShareWithKarl ds1 ds2 Karl s1 s2 = (ds1 /= s1) && (ds1 /= s2) && (ds2 /= s1) && (ds2 /= s2)
dutchDoesNotShareWithKarl ds1 ds2 _ s1 s2 = True

solve :: [School]
solve = do
        dn <- nameslist
        en <- nameslist
        pn <- nameslist
        wn <- nameslist
        ds1 <- subjectslist
        ds2 <- subjectslist
        es1 <- subjectslist
        es2 <- subjectslist
        ps1 <- subjectslist
        ps2 <- subjectslist
        ws1 <- subjectslist
        ws2 <- subjectslist
        guard $ uniqueNames dn en pn wn
        guard $ englishIsNotSimon en
        guard $ englishTeachesHistory es1 es2
        guard $ twoHistoryTeachers ds1 ds2 es1 es2 ps1 ps2 ws1 ws2
        guard $ simonTeachesHistory dn ds1 ds2 pn ps1 ps2 wn ws1 ws2
        guard $ oneMathsTeacher ds1 ds2 es1 es2 ps1 ps2 ws1 ws2
        guard $ threeDutchTeachers ds1 ds2 es1 es2 ps1 ps2 ws1 ws2
        guard $ twoChemistryTeachers ds1 ds2 es1 es2 ps1 ps2 ws1 ws2
        guard $ differentSubjs ds1 ds2
        guard $ differentSubjs es1 es2
        guard $ differentSubjs ps1 ps2
        guard $ differentSubjs ws1 ws2
        guard $ stevenTeachesChemistry dn ds1 ds2
        guard $ stevenTeachesChemistry en es1 es2
        guard $ stevenTeachesChemistry pn ps1 ps2
        guard $ stevenTeachesChemistry wn ws1 ws2
        guard $ peterDoesNotTeachDutch dn ds1 ds2
        guard $ peterDoesNotTeachDutch en es1 es2
        guard $ peterDoesNotTeachDutch pn ps1 ps2
        guard $ peterDoesNotTeachDutch wn ws1 ws2
        guard $ dutchDoesNotShareWithPainter ds1 ds2 ps1 ps2
        guard $ dutchDoesNotShareWithKarl ds1 ds2 en es1 es2
        guard $ dutchDoesNotShareWithKarl ds1 ds2 pn ps1 ps2
        guard $ dutchDoesNotShareWithKarl ds1 ds2 wn ws1 ws2
        return $ (School (MrDutch dn ds1 ds2) (MrEnglish en es1 es2) (MrPainter pn ps1 ps2) (MrWriter wn ws1 ws2))

main :: IO ()
main = mapM_ print solve
