--Constructors
---------------
data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)
data Output a = Out (a, Int) | NoOutput deriving (Show, Eq)
-------------------------------------------------------------------------------------------------------------------------------------------------
--General
----------
--1
convertBinToDec x = helperConvertBinToDec 1 0 x
helperConvertBinToDec _ acc 0 = acc
helperConvertBinToDec counter acc x | mod x 2 == 0 = helperConvertBinToDec (counter*2) acc (div x 10)
									| mod x 2 /=0 = helperConvertBinToDec (counter*2) (acc+counter) (div x 10)
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--2
replaceIthItem item l index = helperReplaceIthItem item l index 0 []
helperReplaceIthItem _ [] _ _ acc = acc
helperReplaceIthItem item (x:xs) index counter acc  | index==counter = helperReplaceIthItem item xs index (counter+1) (acc++[item])
													| index/=counter = helperReplaceIthItem item xs index (counter+1) (acc++[x])
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--3
splitEvery n l = helperSplitEvery n l 1 [] []
helperSplitEvery _ [] _ acc accOfAccs   | length acc == 0 =  accOfAccs
										| otherwise = accOfAccs++[acc]
helperSplitEvery n (x:xs) counter acc accOfAccs | counter == n = helperSplitEvery n xs 1 [] (accOfAccs++[acc++[x]])
												| counter /= n = helperSplitEvery n xs (counter+1) (acc++[x]) accOfAccs
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--4
getNumBits numOfSets cacheType cache	| cacheType == "fullyAssoc" = 0
										| cacheType == "setAssoc" =  ceiling (logBase2 numOfSets)
										| cacheType == "directMap" = ceiling (logBase2 (fromIntegral (length cache)))
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--5
logBase2 num = (log num) / (log 2)
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--6
fillZeros s 0 = s
fillZeros s n = fillZeros ("0"++s) (n-1)
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Get Data From Cache
---------------------
--Direct
getDataFromCache stringAddress cache "directMap" bitsNum = if tag == a && valid==True then Out(dat,0) else NoOutput
															where{
															(a,b) = convertAddress (read stringAddress::Int) bitsNum "directMap";
															(It (T tag) (D dat) valid order) = cache!!(convertBinToDec b)}														
--Fully
getDataFromCache stringAddress cache "fullyAssoc" bitsNum = helperGetDataFromCache (convertBinToDec (read stringAddress :: Int)) cache "fullyAssoc" bitsNum 0
--Set
getDataFromCache stringAddress cache "setAssoc" bitsNum = helperGetDataFromCache  (getTagHelper stringAddress bitsNum) ((splitEvery (2^bitsNum) cache) !! (getIndexHelper stringAddress bitsNum)) "fullyAssoc" bitsNum 0 
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Convert Address
-------------------
--Direct
convertAddress binAddress bitsNum "directMap" = (div binAddress (10^bitsNum),mod binAddress (10^bitsNum))
--Fully
convertAddress binAddress bitsNum "fullyAssoc" = (binAddress,0)
--Set
convertAddress binAddress bitsNum "setAssoc" = (div binAddress (10^bitsNum),mod binAddress (10^bitsNum))
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Replace In Cache
-------------------
--Direct
replaceInCache tag idx memory cache "directMap" bitsNum = (dat, replaceIthItem (It (T tag) (D dat) True 0) cache (convertBinToDec idx))
															where dat = getDataFromMem (convertBinToDec (read (show tag ++ (fillZeros (show idx) (bitsNum - (length (show idx)))))::Int)) memory
--Fully						
replaceInCache tag _ memory oldCache "fullyAssoc" bitsNum   | isThereInvalid oldCache == True = thereIsInvalid tag oldCache ((getDataFromMem (convertBinToDec tag) memory),[]) "fullyAssoc" bitsNum False
															| otherwise = thereIsNotInvalid tag oldCache ((getDataFromMem (convertBinToDec tag) memory),[]) (getMaxOrder oldCache) "fullyAssoc" bitsNum
--Set
replaceInCache tag idx memory cache "setAssoc" bitsNum   	| isThereInvalid ((splitEvery (2^bitsNum) cache) !!  (convertBinToDec idx)) == True = (a,getBeforeAndAfter b (splitEvery (2^bitsNum) cache) 0 (convertBinToDec idx)) 
															| otherwise = (c,getBeforeAndAfter d (splitEvery (2^bitsNum) cache) 0 (convertBinToDec idx))  
															where{
															(a,b) = thereIsInvalid tag ((splitEvery (2^bitsNum) cache) !!  (convertBinToDec idx)) ((getDataFromMem (convertBinToDec (read (show tag ++ (fillZeros (show idx) (bitsNum - (length (show idx)))))::Int)) memory),[]) "fullyAssoc" bitsNum False;
															(c,d) = thereIsNotInvalid tag ((splitEvery (2^bitsNum) cache) !!  (convertBinToDec idx)) ((getDataFromMem (convertBinToDec (read (show tag ++ (fillZeros (show idx) (bitsNum - (length (show idx)))))::Int)) memory),[]) (getMaxOrder ((splitEvery (2^bitsNum) cache) !! (convertBinToDec idx))) "fullyAssoc" bitsNum}
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Helpers
---------
isThereInvalid [] = False
isThereInvalid ((It _ _ valid _):xs) | valid== False = True
									 | otherwise = isThereInvalid xs
getDataFromMem memAddress (x:xs) = helperGetDataFromMem memAddress (x:xs) 0
helperGetDataFromMem memAddress (x:xs) currAddress  | currAddress == memAddress = x
													| otherwise = helperGetDataFromMem memAddress xs (currAddress+1)
thereIsInvalid _ [] result "fullyAssoc" _ _ = result
thereIsInvalid tag ((It (T oldTag) (D oldData) valid oldOrder):xs) (memData,newCache) "fullyAssoc" bitsNum isInserted 	| isInserted == False && valid == False = thereIsInvalid tag xs (memData,(newCache++[(It (T tag) (D memData) True 0)])) "fullyAssoc" bitsNum True
																														| valid == True = thereIsInvalid tag xs (memData,(newCache++[(It (T oldTag) (D oldData) valid (oldOrder+1))])) "fullyAssoc" bitsNum isInserted
																														| valid == False = thereIsInvalid tag xs (memData,(newCache++[(It (T oldTag) (D oldData) valid oldOrder)])) "fullyAssoc" bitsNum isInserted
getMaxOrder [] = 0																														
getMaxOrder ((It _ _ _ oldOrder):xs) = max oldOrder (getMaxOrder xs) 
thereIsNotInvalid _ [] result _ "fullyAssoc" _ = result
thereIsNotInvalid tag ((It (T oldTag) (D oldData) valid oldOrder):xs) (memData,newCache) maxOrder "fullyAssoc" bitsNum  | oldOrder == maxOrder = thereIsNotInvalid tag xs (memData,(newCache++[(It (T tag) (D memData) True 0)])) maxOrder "fullyAssoc" bitsNum
																														| otherwise = thereIsNotInvalid tag xs (memData,(newCache++[(It (T oldTag) (D oldData) valid (oldOrder+1))])) maxOrder "fullyAssoc" bitsNum	
helperGetDataFromCache _ [] "fullyAssoc" _ _ = NoOutput
helperGetDataFromCache decAddress ((It (T tag) (D d) valid _):xs) "fullyAssoc" bitsNum hopsNum  | decAddress == tag && valid == True = Out(d,hopsNum)
																								| otherwise = helperGetDataFromCache decAddress xs "fullyAssoc" bitsNum (hopsNum+1)
getTagHelper stringAddress bitsNum = convertBinToDec a where (a,b) = (convertAddress (read stringAddress :: Int) bitsNum "setAssoc") 
getIndexHelper stringAddress bitsNum = convertBinToDec b where (a,b) = (convertAddress (read stringAddress :: Int) bitsNum "setAssoc")
getBeforeAndAfter result cache setsCounter idx  = (getBefore [] cache setsCounter idx) ++ result ++ (getAfter [] cache 0 idx)
getBefore acc (x:xs) setsCounter idx | idx==setsCounter = acc
									 | otherwise = getBefore (acc++x) xs (setsCounter+1) idx 
getAfter acc [] _ _ = acc
getAfter acc (x:xs) setsCounter idx | setsCounter <= idx = getAfter acc xs (setsCounter+1) idx
									| otherwise = getAfter (acc ++ x) xs (setsCounter+1) idx												 
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--getData & runProgram
----------------------
--1
getData stringAddress cache memory cacheType bitsNum | x == NoOutput = replaceInCache tag index memory cache cacheType bitsNum
													 | otherwise = (getX x, cache)
													 where{
														x = getDataFromCache stringAddress cache cacheType bitsNum;
														address = read stringAddress:: Int; 
														(tag, index) = convertAddress address bitsNum cacheType;
														getX (Out (d, _)) = d}
--2
runProgram [] cache _ _ _ = ([], cache)
runProgram (addr: xs) cache memory cacheType numOfSets = ((d:prevData), finalCache)
	where{
		bitsNum = round(logBase2 numOfSets);
		(d, updatedCache) = getData addr cache memory cacheType bitsNum;
		(prevData, finalCache) = runProgram xs updatedCache memory cacheType numOfSets}