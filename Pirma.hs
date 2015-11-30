module Pirma
where

type InternalMap = [(String, String)]
type ExternalMap = [(String, InternalMap)]

parseInt :: String -> Int
parseInt a = read a :: Int 

winner :: String -> Maybe Mark
winner msg = 
		let
			decodedMsg = decodeFullMsg msg 
		in checkColumns decodedMsg decodedMsg "0" Nothing 0

decodeFullMsg :: String -> ExternalMap
decodeFullMsg msg = decode msg []

saveKey :: String -> ExternalMap -> ExternalMap
saveKey "" c = decode "" c
saveKey ('e' : rest) c = decode rest c 
saveKey rest d  = 
				let
					b = takeWhile(/= ':') rest
					c = parseInt b
					res = drop (length b + 1) rest
					key = take c res
					res2 = drop (length key + 1) res
					(values, rest3) = decodeValues res2 []
				in decode rest3 ((key, values) : d) 
saveKey a c = decode a c -- ??

decode :: String -> ExternalMap -> ExternalMap
decode ('d' : rest) b = saveKey rest b
decode ('e' : rest) b = saveKey rest b 
decode "" d = d
decode a c = []

decodeValues :: String -> InternalMap -> (InternalMap, String)
decodeValues ('e' : rest) a = (a, ('e' : rest))
decodeValues msg b = 
				let
					(key, rest3) = decodeValue msg
					(val, rest) = decodeValue rest3
				in decodeValues rest ((key, val) : b)

decodeValue :: String -> (String, String)
decodeValue ('i' : rest) = decodeInt rest
decodeValue msg = 
				let
					a = takeWhile(/=':') msg
					b = parseInt a
					rest2 = drop (length a + 1) msg
					val = take b rest2
					rest  = drop (length val) rest2
				in (val, rest)

decodeInt :: String -> (String, String)
decodeInt msg = 
			let
				a = takeWhile(/= 'e') msg
				rest = drop (length a + 1) msg
			in (a, rest)




data Mark = O | X 
		deriving(Show, Eq)

data Criteria = Row | Column | DiagonalEq | DiagonalSumEq
		deriving(Show, Eq)

toMark :: Maybe String -> Maybe Mark
toMark (Just "x") = Just X
toMark (Just "X") = Just X
toMark (Just "o") = Just O
toMark (Just "O") = Just O
toMark Nothing = Nothing
toMark a = Nothing

checkColumns :: ExternalMap -> ExternalMap -> String -> Maybe Mark -> Int -> Maybe Mark
checkColumns fullMsg msg "3" Nothing markCounter = checkRows fullMsg fullMsg "0" Nothing 0
checkColumns fullMsg a b c 3 = c
checkColumns fullMsg [] columnNo markNotFound markCounter = 
					let 
						columnNr = parseInt columnNo
					in checkColumns fullMsg fullMsg (show (columnNr + 1)) Nothing 0
checkColumns fullMsg msg columnNo mark markCounter = 
					let
						(singleInternal, restExternal) = popInternalMap msg
						y = checkValue singleInternal "y"
						foundMark2 = if y == columnNo then lookup "v" singleInternal else Nothing
						foundMark = toMark foundMark2
					in 
					if foundMark == Nothing then checkColumns fullMsg restExternal columnNo mark markCounter
					else if mark == Nothing && markCounter == 0 then checkColumns fullMsg restExternal columnNo foundMark (markCounter + 1)
					else if foundMark == mark then checkColumns fullMsg restExternal columnNo mark (markCounter + 1)
					else checkColumns fullMsg [] columnNo Nothing markCounter

checkRows :: ExternalMap -> ExternalMap -> String -> Maybe Mark -> Int -> Maybe Mark
checkRows fullMsg msg "3" Nothing markCounter = checkDiagonalEq fullMsg fullMsg Nothing 0
checkRows fullMsg a b c 3 = c
checkRows fullMsg [] columnNo markNotFound markCounter = 
					let 
						columnNr = parseInt columnNo
					in checkRows fullMsg fullMsg (show (columnNr + 1)) Nothing 0
checkRows fullMsg msg columnNo mark markCounter = 
					let
						(singleInternal, restExternal) = popInternalMap msg
						x = checkValue singleInternal "x"
						foundMark2 = if x == columnNo then lookup "v" singleInternal else Nothing
						foundMark = toMark foundMark2
					in 
					if foundMark == Nothing then checkRows fullMsg restExternal columnNo mark markCounter
					else if mark == Nothing && markCounter == 0 then checkRows fullMsg restExternal columnNo foundMark (markCounter + 1)
					else if foundMark == mark then checkRows fullMsg restExternal columnNo mark (markCounter + 1)
					else checkRows fullMsg [] columnNo Nothing markCounter

checkDiagonalEq :: ExternalMap -> ExternalMap -> Maybe Mark -> Int -> Maybe Mark
checkDiagonalEq fullMsg a c 3 = c
checkDiagonalEq fullMsg [] markNotFound markCounter = checkDiagonalSumEq fullMsg Nothing 0
checkDiagonalEq fullMsg msg mark markCounter = 
					let
						(singleInternal, restExternal) = popInternalMap msg
						x = checkValue singleInternal "x"
						y = checkValue singleInternal "y"
						foundMark2 = if x == y then lookup "v" singleInternal else Nothing
						foundMark = toMark foundMark2
					in 
					if foundMark == Nothing then checkDiagonalEq fullMsg restExternal mark markCounter
					else if mark == Nothing && markCounter == 0 then checkDiagonalEq fullMsg restExternal foundMark (markCounter + 1)
					else if foundMark == mark then checkDiagonalEq fullMsg restExternal mark (markCounter + 1)
					else checkDiagonalEq fullMsg [] Nothing markCounter

checkDiagonalSumEq :: ExternalMap -> Maybe Mark -> Int -> Maybe Mark
checkDiagonalSumEq a c 3 = c
checkDiagonalSumEq [] markNotFound markCounter = Nothing --pabaiga
checkDiagonalSumEq msg mark markCounter = 
					let
						(singleInternal, restExternal) = popInternalMap msg
						x = checkValue singleInternal "x"
						xInt = parseInt x
						y = checkValue singleInternal "y"
						yInt = parseInt y
						foundMark2 = if (xInt + yInt) == 2 then lookup "v" singleInternal else Nothing
						foundMark = toMark foundMark2
					in 
					if foundMark == Nothing then checkDiagonalSumEq restExternal mark markCounter
					else if mark == Nothing && markCounter == 0 then checkDiagonalSumEq restExternal foundMark (markCounter + 1)
					else if foundMark == mark then checkDiagonalSumEq restExternal mark (markCounter + 1)
					else checkDiagonalSumEq [] Nothing markCounter

popInternalMap :: ExternalMap -> (InternalMap, ExternalMap)
popInternalMap ((a, b) : d) = (b, d)

checkValue :: InternalMap -> String -> String
checkValue a b =  case lookup b a of
						Just a -> a