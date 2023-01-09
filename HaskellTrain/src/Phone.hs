module Phone where 
import Text.ParserCombinators.ReadP (char)

data DaPhone = DaPhone [(Digit ,[Char])]

m :: DaPhone
m = DaPhone [('1', []) , ('2', ['a','b','c']) , ('3', ['d','e','f']), ('4', ['g', 'h','i']), ('5', ['j','k','l']), ('6' , ['m','n','o']), ('7', ['p','q','r','s']), ('8', ['t','u','v']), ('9', ['w','x','y','z']), ('0', [' ', '+']), ('#' , ['.', ','])]

convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]
-- validButtons = "1234567890*#"
type Digit = Char
-- Valid presses: 1 and up
type Presses = Int


reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps p l= case p of 
    DaPhone [( x  , [l,_,_,_] )] -> [(x, 1)]
    DaPhone [( x  , [_,l,_,_] )] -> [(x, 2)]
    DaPhone [( x  , [_,_,l,_] )] -> [(x, 3)]
    DaPhone [( x  , [_,_,_,l] )] -> [(x, 4)]
    DaPhone _ -> [('!' , 1)]
    

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = undefined

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = undefined

mostPopularLetter :: String -> Char
mostPopularLetter = undefined

coolestLtr :: [String] -> Char
coolestLtr = undefined

coolestWord :: [String] -> String
coolestWord = undefined