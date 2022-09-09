

data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)

--General:

convertBinToDec :: Integral a => a -> a

convertBinToDec x = helperBinToDec x 0

helperBinToDec :: Integral a => a -> a -> a
helperBinToDec x y | x>0 =( (mod x 10) * (2^y) ) + helperBinToDec (div x 10) (y+1)
                   | otherwise = 0




replaceIthItem :: t -> [t] -> Int -> [t]

replaceIthItem y (h:hs) x | x==0 = ( y: hs )
                          | otherwise = h : ( replaceIthItem y hs (x-1) )  

splitEvery :: (Num a1, Ord a1) => a1 -> [a] -> [[a]]
takeNum :: (Num a1, Ord a1) => a1->[t]->[t]
dropNum :: (Num a1, Ord a1) => a1->[t]->[t]

takeNum 0 _ = []
takeNum x (h:hs) = h: takeNum ( x-1 ) hs

dropNum _ [] = []
dropNum x (h:hs) | x > 0 = dropNum (x-1) hs
                 | otherwise = h:hs 

splitEvery x list | fromIntegral (length list) <= x  = [list]
                  | otherwise = [ takeNum x list ] ++ splitEvery x (dropNum x list) 

logBase2 :: Floating a => a -> a

logBase2 num = (log num) / (log 2)

logBase3 num | num==1 = 0 

logBase3 num = logBase3 (div num 2) +1


                 
--getNumBits :: (Integral a, RealFloat a1) => a1 -> [Char] -> [c] -> a

getNumBits num typo t | typo == "fullyAssoc" = 0
                      | typo == "setAssoc" = logBase3 num
                      | otherwise = logBase3 (length t) 



fillZeros :: (Eq a, Num a) => [Char] -> a -> [Char]

fillZeros (h:hs) x | x /=0 = '0' : ( fillZeros (h:hs) (x-1) )
                   | otherwise = h:hs

-- Part 1 Direct Mapping:

data Output a = Out (a, Int) | NoOutput deriving (Show, Eq)




stringToIntegral ::(Integral r)=> [Char] -> r

stringToIntegral [] = 0
stringToIntegral (h:hs) | h =='0' = stringToIntegral hs
                        | otherwise =  (10^(length hs)) + stringToIntegral hs  






--convertAddress :: (Integral b1, Integral b2) => b1 -> b2 -> p -> (b1, b1)

convertAddress binAddress bitsNum "directMap" = ((div binAddress (10^bitsNum)),(mod binAddress (10^bitsNum) ))
convertAddress binAddress bitsNum "fullyAssoc" = (binAddress,0)
convertAddress binAddress bitsNum "setAssoc" = (t,indx)
													where
														n=10^bitsNum
														t = div binAddress n 
														indx = mod binAddress n



















--getDataFromCache :: (Integral b, Eq a) =>[Char] -> [Item a] -> [Char] -> b -> Output a



getDataFromCache stringAddress cache "directMap" bitsNum | tag == x && valid==True  = Out(res,0)    
                                                         |otherwise = NoOutput
															where 
																It (T tag) (D res) valid __ = cache !! i
																i = convertBinToDec index
																(x,index) = convertAddress numAdd bitsNum "directMap"
																numAdd = stringToIntegral stringAddress 


getDataFromCache stringAddress [] "fullyAssoc" bitsNum = NoOutput
getDataFromCache stringAddress ((It (T a) (D d) x n):xs) "fullyAssoc" bitsNum | (read stringAddress :: Int) ==a && x==True = Out(d,(h+1))  
																			  |  otherwise = getDataFromCache stringAddress xs "fullyAssoc" bitsNum   where h = 0 


getDataFromCache stringAddress cache "setAssoc" bitsNum | dt =="No" = NoOutput
														| otherwise = Out(dt,hp)
                                                              where
																lc = length cache
																setsNum = getSetsNum bitsNum
																n = div lc setsNum
																cache2= splitEvery n cache
																numAdress = read stringAddress ::Int 
																(t,idx) = convertAddress numAdress bitsNum "setAssoc"
																idxdec = convertBinToDec idx 
																res = cache2 !! (idxdec)
																stag = show t
																lent = length stag
																rem = bitsNum + lent
																rem1 = 6 - rem
																newtag = fillZeros stag rem1 
																(dt,hp) =helperGetDataSetAssoc newtag res 0

helperGetDataSetAssoc tag [] _ = ("No",0)
helperGetDataSetAssoc tag (It (T t) (D d) x n :xs) hp |(read tag :: Int) == t && x==True = (d,(hp))
												      | otherwise = helperGetDataSetAssoc tag xs (hp+1)
													  
												



getSetsNum bitsNum = 2^bitsNum


--replaceInCache :: Integral b => Int -> Int -> [a] -> [Item a] -> [Char] -> b -> (a, [Item a])

replaceInCache tag idx memory oldCache "directMap" bitsNum = (dt,hp)
																where
																	sIdx=show idx
																	sTag=show tag
																	idxLen=length sIdx
																	stagLen=length sTag
																	remIdx=bitsNum-idxLen
																	remTag=(6-stagLen)-bitsNum
																	newIdx=fillZeros sIdx remIdx
																	newTag=fillZeros sTag remTag
																	addresString=newTag++newIdx
																	address=read addresString ::Int
																	decAddress=convertBinToDec address
																	decIdx=convertBinToDec idx
																	itemData = memory !! decAddress
																	(dt,hp)=(itemData,replaceIthItem y oldCache (decIdx))
																	y = It (T (read j:: Int)) (D k) u m
																	j=newTag
																	k=itemData
																	m=0
																	u=True


replaceInCache tag idx memory oldCache "fullyAssoc" bitsNum |pos == ((length oldCache))= cache2
																	where 
																		stag = show tag
																		stringLength= length stag 
																		rem = 6 - stringLength 
																		newTag = fillZeros stag rem 
																		straddress = newTag 
																		binAddress = read straddress :: Int
																		decAddress = convertBinToDec binAddress
																		itemData = memory !! decAddress
																		pos = checkValidity oldCache 0
																		newcache3 = modifyCacheN oldCache
																		pos2 = checkMax oldCache 
																		y = It (T (read j :: Int)) (D k) u m
																		cache2=(itemData,replaceIthItem y newcache3 (pos2-1))
																		j=newTag
																		k=itemData
																		m=0
																		u=True
																		
replaceInCache tag idx memory oldCache "fullyAssoc" bitsNum = cache1 
																	where 
																		stag = show tag
																		stringLength= length stag 
																		rem = 6 - stringLength 
																		newTag = fillZeros stag rem 
																		straddress = newTag 
																		binAddress = read straddress :: Int
																		decAddress = convertBinToDec binAddress
																		itemData = memory !! decAddress
																		pos = checkValidity oldCache 0
																		newcache3 = modifyCacheN oldCache
																		cache1= (itemData,replaceIthItem y newcache3 (pos))
																		y = It (T (read j:: Int)) (D k) u m
																		j=newTag
																		k=itemData
																		m=0
																		u=True




replaceInCache tag idx memory oldCache "setAssoc" bitsNum =  (itemData,cache)
														   where 
															setsNum = getSetsNum bitsNum
															decIdx = convertBinToDec idx 
															sidx = show idx
															stag = show tag
															oldIdxlen= length sidx
															rem = bitsNum - oldIdxlen
															newIdx= fillZeros sidx rem
															oldTaglen = length stag
															remTag = (6 - bitsNum) - oldTaglen
															newTag = fillZeros stag remTag
															stringAddress = newTag ++ newIdx
															binAddress = read stringAddress :: Int
															decAddress = convertBinToDec binAddress
															itemData = memory !! decAddress
															splitCache = splitEvery setsNum oldCache
															smallCache = splitCache !! decIdx
															cache= collect newsplit []
															newSmallCache =  helperReplaceSetAssoc newTag itemData smallCache
															newsplit  = replaceIthItem newSmallCache splitCache decAddress
                                                            



helperReplaceSetAssoc newTag itemData smallCache| pos == ((length smallCache)) = newSmallCache2 
													where
														pos = checkMax smallCache
														modifiedSmallCache = modifyCacheSet smallCache
														newSmallCache2 = replaceIthItem y modifiedSmallCache (pos-1)
														y= It (T b) (D k) u m
														b = read newTag :: Int  
														k = itemData
														u = True
														m =  0


helperReplaceSetAssoc newtag item smallCache =newSmallCache3
											 where
													newSmallCache3 = replaceIthItem y modifiedSmallCache pos
													pos =checkValidity smallCache 0
													modifiedSmallCache = modifyCacheSet smallCache
													y=It (T b) (D k) u m
													b=read newtag :: Int  
													k=item
													u=True
													m =0
													




collect [] acc = acc
collect (x:xs) acc = x ++ collect xs acc


modifyCacheSet [] = []
modifyCacheSet  (It (T t) (D d) x n:xs) |x==True = [(It (T t) (D d) x (n+1))] ++ modifyCacheSet xs
										| otherwise = [(It (T t) (D d) x n)]++ modifyCacheSet xs





--set Associative:




--getData :: (Eq t, Integral b) => String -> [Item t] -> [t] -> [Char] -> b -> (t, [Item t])
getData stringAddress cache memory cacheType bitsNum | x == NoOutput = replaceInCache tag index memory cache cacheType bitsNum
													 | otherwise = (getX x, cache)
														where
															x = getDataFromCache stringAddress cache cacheType bitsNum
															address = read stringAddress:: Int
															(tag, index) = convertAddress address bitsNum cacheType
															getX (Out (d, _)) = d



--runProgram :: (RealFloat a1, Eq a2) => [[Char]] -> [Item a2] -> [a2] -> [Char] -> a1 -> ([a2], [Item a2])

runProgram [] cache _ _ _ = ([], cache)
runProgram (addr: xs) cache memory cacheType numSets = ((d:prevData), finalCache)
														where
																bitsNum = round(logBase2 numSets)
																(d, updatedCache) = getData addr cache memory cacheType bitsNum
																(prevData, finalCache) = runProgram xs updatedCache memory cacheType numSets


checkMax [] = 0
checkMax ((It (T a) (D d) m n):xs) | n > checkMax xs = n
									| otherwise = checkMax xs


checkValid [] =False

checkValid ((It (T a) (D d) m n):xs) | m==False = checkValid xs
									 | otherwise = True

checkValidity [] i = i
checkValidity ((It (T a) (D d) m n):xs) i | m==False = i
										  | otherwise = checkValidity xs (i+1) 


modifyCacheN [] = []  
modifyCacheN ((It (T a) (D d) m n):xs) | m==False = ((It (T a) (D d) m n) :(modifyCacheN xs))
									   | otherwise =  ((It (T a) (D d) m c):modifyCacheN xs) 
											where			
												c = n+1




maxlist [] = [] 
maxlist [x] = x

maxlist (x:xs) | length x > length (maxlist xs)  = x
               |   otherwise = maxlist xs
			   
			   
			   
divf _ [] = 0
divf c (x:xs) | mod c x ==0= 1 + divf c xs 
			  | otherwise = divf c xs 