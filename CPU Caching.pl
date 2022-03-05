:- style_check(-discontiguous).  %allow discontiguous predicates
%GENERAL
%-------
%1
convertBinToDec(Bin,Dec):-
	helperBinToDec(Bin,1,0,Dec).
	
	
helperBinToDec(0,_,Counter,Counter).
helperBinToDec(Bin,Acc,Counter,Dec):-
	Bin\=0,
	Dig is Bin mod 2,
	Dig = 0,
	NewNum is Bin//10,
	NewAcc is Acc*2,
	helperBinToDec(NewNum,NewAcc,Counter,Dec).
helperBinToDec(Bin,Acc,Counter,Dec):-
	Bin\=0,
	Dig is Bin mod 2,
	Dig = 1,
	NewNum is Bin//10,
	NewAcc is Acc*2,
	NewCounter is Counter + Acc,
	helperBinToDec(NewNum,NewAcc,NewCounter,Dec).
%------------------------------------------------------------------------------
%2
replaceIthItem(Item,List,I,Result):-
	helperReplaceItem(Item,List,0,I,[],Result).


helperReplaceItem(_,[],_,_,Result,Result).
helperReplaceItem(Item,[_|T],I,I,Acc,Result):-
	append(Acc,[Item],R),
	I1 is I + 1,
	helperReplaceItem(Item,T,I1,I,R,Result).
helperReplaceItem(Item,[H|T],X,I,Acc,Result):-
	append(Acc,[H],R),
	X \= I,
	I1 is X + 1,
	helperReplaceItem(Item,T,I1,I,R,Result).
%------------------------------------------------------------------------------
%3
splitEvery(N,List,Res):-
	helperSplitEvery(N,List,1,[],[],Res).


helperSplitEvery(_,[],_,Acc,Res,Res):-
	length(Acc,L),
	L=0.
helperSplitEvery(_,[],_,Acc,Res,R):-
	length(Acc,L),
	L\=0,
	append(Res,[Acc],R).
helperSplitEvery(N,[H|T],X,Acc,Result,Res):-
	append(Acc,[H],R1),
	append(Result,[R1],R),
	X1 is X mod N,
	X1 = 0,
	helperSplitEvery(N,T,1,[],R,Res).
helperSplitEvery(N,[H|T],X,Acc,Result,Res):-
	append(Acc,[H],R),
	X1 is X mod N,
	X1 \= 0,
	X2 is X + 1,
	helperSplitEvery(N,T,X2,R,Result,Res).
%------------------------------------------------------------------------------
%4
logBase2(Num,Res):-
	helperLogBase2(Num,0,Res).
	
	
helperLogBase2(1,Acc,Acc).
helperLogBase2(Num,Acc,Res):-
	Num\=1,
	NewAcc is Acc+1,
	NewNum is Num//2,
	helperLogBase2(NewNum,NewAcc,Res).
%------------------------------------------------------------------------------
%5
getNumBits(_,fullyAssoc,_,0).
getNumBits(_,directMap,Cache,BitsNum):-
	length(Cache,R),
	logBase2(R,BitsNum).
getNumBits(NumOfSets,setAssoc,_,BitsNum):-
	logBase2(NumOfSets,BitsNum).
%------------------------------------------------------------------------------
%6
fillZeros(S,0,S).
fillZeros(S,N,R):-
	N\=0,
	string_concat("0",S,R1),
	N1 is N - 1,
	fillZeros(R1,N1,R).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
%DirectMapping
%-------------
%1
getAddressAndTag(StringAddress,BitsNum,Address,Tag):-
	atom_number(StringAddress,R),
	X is 10**BitsNum,
	AddressBin is R mod X,
	convertBinToDec(AddressBin,Address),
	atom_number(AddressString,AddressBin),
	string_length(AddressString,O),
	Z is BitsNum - O,
	fillZeros(AddressString,Z,AddressX),
	string_concat(Tag,AddressX,StringAddress).
	
	
getDataFromCache(StringAddress,Cache,Data,HopsNum,directMap,BitsNum):-
	getAddressAndTag(StringAddress,BitsNum,Address,Tag),
	helperGetDataFromCache(Address,Tag,0,Cache,Data,directMap,BitsNum),
	HopsNum is 0.


helperGetDataFromCache(Address,Tag,Address,[item(tag(Tag),data(Data),1,_)|_],Data,directMap,_).
helperGetDataFromCache(Address,TagX,CurrAddress,[item(tag(_),data(_),_,_)|T],DataX,directMap,BitsNum):-
	CurrAddress\=Address,
	CurrAddress<Address,
	NewCurrAddress is CurrAddress + 1,
	helperGetDataFromCache(Address,TagX,NewCurrAddress,T,DataX,directMap,BitsNum).
%------------------------------------------------------------------------------
%2
convertAddress(Bin,BitsNum,Tag,Idx,directMap):-
	X is 10**BitsNum,
	Idx is Bin mod X,
	Tag is Bin//X.
%------------------------------------------------------------------------------
%3

getDataFromMem(Mem,MemAddress,ItemData):-
	helperGetDataFromMem(Mem,MemAddress,0,ItemData).
helperGetDataFromMem([_|T],MemAddress,CurrAddress,ItemData):-
	CurrAddress\=MemAddress,
	CurrAddress<MemAddress,
	NewCurrAddress is CurrAddress + 1,
	helperGetDataFromMem(T,MemAddress,NewCurrAddress,ItemData).
helperGetDataFromMem([H|_],MemAddress,MemAddress,H).
	
	
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,directMap,BitsNum):-
	atom_number(StringTag,Tag),
	atom_number(StringIdx,Idx),
	string_length(StringIdx,P),
	U is BitsNum - P,
	fillZeros(StringIdx,U,FilledIdx),
	string_concat(StringTag,FilledIdx,X),
	atom_number(X,Z),
	convertBinToDec(Z,MemAddress),
	getDataFromMem(Mem,MemAddress,ItemData),
	convertBinToDec(Idx,IdxDec),
	helperReplaceInCache(StringTag,IdxDec,0,OldCache,NewCache,[],ItemData,directMap,BitsNum).
	
	
helperReplaceInCache(_,_,_,[],NewCache,NewCache,_,directMap,_).
helperReplaceInCache(Tag,IdxAcc,IdxAcc,[item(tag(_),data(_),_,_)|T],NewCache,CacheAcc,ItemData,directMap,BitsNum):-
	string_length(Tag,R2),
	Z is 6 - R2 - BitsNum,
	fillZeros(Tag,Z,Q),
	append(CacheAcc,[item(tag(Q),data(ItemData),1,0)],NewCacheAcc),
	NewIdxAcc is IdxAcc + 1,
	helperReplaceInCache(Tag,IdxAcc,NewIdxAcc,T,NewCache,NewCacheAcc,ItemData,directMap,BitsNum).
helperReplaceInCache(Tag,IdxDec,IdxAcc,[H|T],NewCache,CacheAcc,ItemData,directMap,BitsNum):-
	IdxDec\=IdxAcc,
	NewIdxAcc is IdxAcc + 1,
	append(CacheAcc,[H],NewCacheAcc),
	helperReplaceInCache(Tag,IdxDec,NewIdxAcc,T,NewCache,NewCacheAcc,ItemData,directMap,BitsNum).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
%Fully Associative
%-----------------
%1	
getDataFromCache(StringAddress,Cache,Data,HopsNum,fullyAssoc,BitsNum):-
	helperGetDataFromCache(StringAddress,Cache,Data,HopsNum,0,fullyAssoc,BitsNum).


helperGetDataFromCache(StringAddress,[item(tag(StringAddress),data(Data),1,_)|_],Data,HopsNumAcc,HopsNumAcc,fullyAssoc,_).
helperGetDataFromCache(StringAddress,[item(tag(Tag),data(_),_,_)|T],Data,HopsNum,HopsNumAcc,fullyAssoc,BitsNum):-
	StringAddress\=Tag,
	NewHopsNumAcc is HopsNumAcc + 1,
	helperGetDataFromCache(StringAddress,T,Data,HopsNum,NewHopsNumAcc,fullyAssoc,BitsNum).
%------------------------------------------------------------------------------
%2

convertAddress(Bin,_,Bin,_,fullyAssoc).
%------------------------------------------------------------------------------
%3

isThereInvalid([item(tag(_),data(_),0,_)|_]).
isThereInvalid([item(tag(_),data(_),1,_)|T]):-
	isThereInvalid(T).


replaceInCache(Tag,_,Mem,OldCache,NewCache,ItemData,fullyAssoc,BitsNum):-
	isThereInvalid(OldCache),
	atom_number(StringTag,Tag),
	convertBinToDec(Tag,MemAddress),
	getDataFromMem(Mem,MemAddress,ItemData),
	thereIsInvalid(StringTag,OldCache,0,NewCache,[],ItemData,fullyAssoc,BitsNum).
replaceInCache(Tag,_,Mem,OldCache,NewCache,ItemData,fullyAssoc,BitsNum):-
	\+isThereInvalid(OldCache),
	atom_number(StringTag,Tag),
	convertBinToDec(Tag,MemAddress),
	getDataFromMem(Mem,MemAddress,ItemData),
	getMaxOrder(OldCache,0,MaxOrder),
	thereIsNotInvalid(StringTag,OldCache,NewCache,[],MaxOrder,ItemData,fullyAssoc,BitsNum).
	
	
thereIsInvalid(_,[],_,NewCache,NewCache,_,fullyAssoc,_).
thereIsInvalid(Tag,[item(tag(_),data(_),0,_)|T],Inserted,NewCache,CacheAcc,ItemData,fullyAssoc,BitsNum):-
	Inserted=0,
	string_length(Tag,R2),
	Z is 6 - R2,
	fillZeros(Tag,Z,Q),
	append(CacheAcc,[item(tag(Q),data(ItemData),1,0)],NewCacheAcc),
	thereIsInvalid(Tag,T,1,NewCache,NewCacheAcc,ItemData,fullyAssoc,BitsNum).
thereIsInvalid(Tag,[item(tag(A),data(B),ValidBit,C)|T],Inserted,NewCache,CacheAcc,ItemData,fullyAssoc,BitsNum):-
	Inserted=1,
	incrementOtherOrders([item(tag(A),data(B),ValidBit,C)|T],[],IncrementedCache),
	append(CacheAcc,IncrementedCache,NewCacheAcc),
	thereIsInvalid(Tag,[],Inserted,NewCache,NewCacheAcc,ItemData,fullyAssoc,BitsNum).
thereIsInvalid(Tag,[item(tag(A),data(B),ValidBit,Order)|T],Inserted,NewCache,CacheAcc,ItemData,fullyAssoc,BitsNum):-
	Inserted=0,
	ValidBit=1,
	NewOrder is Order + 1,
	append(CacheAcc,[item(tag(A),data(B),ValidBit,NewOrder)],NewCacheAcc),
	thereIsInvalid(Tag,T,Inserted,NewCache,NewCacheAcc,ItemData,fullyAssoc,BitsNum).


incrementOtherOrders([],IncrementedCache,IncrementedCache).	
incrementOtherOrders([item(tag(A),data(B),1,Order)|T],IncrementedCacheAcc,IncrementedCache):-
	NewOrder is Order + 1,
	append(IncrementedCacheAcc,[item(tag(A),data(B),1,NewOrder)],NewIncrementedCacheAcc),
	incrementOtherOrders(T,NewIncrementedCacheAcc,IncrementedCache).
incrementOtherOrders([item(tag(A),data(B),0,Order)|T],IncrementedCacheAcc,IncrementedCache):-
	append(IncrementedCacheAcc,[item(tag(A),data(B),0,Order)],NewIncrementedCacheAcc),
	incrementOtherOrders(T,NewIncrementedCacheAcc,IncrementedCache).
	
	
maximum(A,B,C):-
	A>=B,
	C=A.
maximum(A,B,C):-
	B>A,
	C=B.
	
	
getMaxOrder([],MaxOrder,MaxOrder).
getMaxOrder([item(tag(_),data(_),_,Order)|T],MaxOrderAcc,MaxOrder):-
	maximum(Order,MaxOrderAcc,NewMaxOrderAcc),
	getMaxOrder(T,NewMaxOrderAcc,MaxOrder).


thereIsNotInvalid(_,[],NewCache,NewCache,_,_,fullyAssoc,_).
thereIsNotInvalid(Tag,[item(tag(A),data(B),ValidBit,Order)|T],NewCache,CacheAcc,MaxOrder,ItemData,fullyAssoc,BitsNum):-
	Order\=MaxOrder,
	NewOrder is Order + 1,
	append(CacheAcc,[item(tag(A),data(B),ValidBit,NewOrder)],NewCacheAcc),
	thereIsNotInvalid(Tag,T,NewCache,NewCacheAcc,MaxOrder,ItemData,fullyAssoc,BitsNum).
thereIsNotInvalid(Tag,[item(tag(_),data(_),_,Order)|T],NewCache,CacheAcc,MaxOrder,ItemData,fullyAssoc,BitsNum):-
	Order=MaxOrder,
	string_length(Tag,R2),
	Z is 6 - R2,
	fillZeros(Tag,Z,Q),
	append(CacheAcc,[item(tag(Q),data(ItemData),1,0)],NewCacheAcc),
	thereIsNotInvalid(Tag,T,NewCache,NewCacheAcc,MaxOrder,ItemData,fullyAssoc,BitsNum).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
%Set Associative
%-----------------
%1
getAddressAndTagSetAssoc(StringAddress,BitsNum,Address,Tag):-
	atom_number(StringAddress,R),
	X is 10**BitsNum,
	AddressBin is R mod X,
	convertBinToDec(AddressBin,Address),
	atom_number(AddressString,AddressBin),
	string_length(AddressString,O),
	Z is BitsNum - O,
	fillZeros(AddressString,Z,AddressX),
	string_concat(Tag,AddressX,StringAddress).
	
	
getDataFromCache(StringAddress,Cache,Data,HopsNum,setAssoc,SetsNum):-
	logBase2(SetsNum,BitsNum),
	length(Cache,R),
	ItemsPerSet is R//SetsNum,
	getAddressAndTagSetAssoc(StringAddress,BitsNum,Address,Tag),
	helperGetDataFromCache(Address,Tag,0,HopsNum,0,ItemsPerSet,0,Cache,Data,setAssoc,SetsNum).


helperGetDataFromCache(Address,Tag,Address,HopsNum,HopsNum,_,_,[item(tag(Tag),data(Data),1,_)|_],Data,setAssoc,_).
helperGetDataFromCache(Address,Tag,Address,HopsNum,HopsNumAcc,ItemsPerSet,NewItemsAcc,[item(tag(TagOld),data(_),_,_)|T],Data,setAssoc,SetsNum):-
	TagOld\=Tag,
	NewHopsNumAcc is HopsNumAcc + 1,
	helperGetDataFromCache(Address,Tag,Address,HopsNum,NewHopsNumAcc,ItemsPerSet,NewItemsAcc,T,Data,setAssoc,SetsNum).
helperGetDataFromCache(Address,Tag,CurrAddress,HopsNum,HopsNumAcc,ItemsPerSet,ItemsAcc,[_|T],Data,setAssoc,SetsNum):-
	CurrAddress\=Address,
	NewItemsAcc is ItemsAcc + 1,
	NewItemsAcc = ItemsPerSet,
	NewCurrAddress is CurrAddress + 1,
	helperGetDataFromCache(Address,Tag,NewCurrAddress,HopsNum,HopsNumAcc,ItemsPerSet,0,T,Data,setAssoc,SetsNum).
helperGetDataFromCache(Address,Tag,CurrAddress,HopsNum,HopsNumAcc,ItemsPerSet,ItemsAcc,[_|T],Data,setAssoc,SetsNum):-
	CurrAddress\=Address,
	NewItemsAcc is ItemsAcc + 1,
	NewItemsAcc \= ItemsPerSet,
	helperGetDataFromCache(Address,Tag,CurrAddress,HopsNum,HopsNumAcc,ItemsPerSet,NewItemsAcc,T,Data,setAssoc,SetsNum).
%------------------------------------------------------------------------------
%2
convertAddress(Bin,SetsNum,Tag,Idx,setAssoc):-
logBase2(SetsNum,BitsNum),
	X is 10**BitsNum,
	Idx is Bin mod X,
	Tag is Bin//X.
%------------------------------------------------------------------------------
%3

getBeforeAndAfterSet(_,_,_,_,[],BeforeSet,BeforeSet,Set,Set,AfterSet,AfterSet).
getBeforeAndAfterSet(Address,CurrAddress,ItemsPerSet,ItemsAcc,[H|T],BeforeSet,BeforeSetAcc,Set,SetAcc,AfterSet,AfterSetAcc):-
	CurrAddress<Address,
	NewItemsAcc is ItemsAcc + 1,
	NewItemsAcc = ItemsPerSet,
	NewCurrAddress is CurrAddress + 1,
	append(BeforeSetAcc,[H],NewBeforeSetAcc),
	getBeforeAndAfterSet(Address,NewCurrAddress,ItemsPerSet,0,T,BeforeSet,NewBeforeSetAcc,Set,SetAcc,AfterSet,AfterSetAcc).
getBeforeAndAfterSet(Address,CurrAddress,ItemsPerSet,ItemsAcc,[H|T],BeforeSet,BeforeSetAcc,Set,SetAcc,AfterSet,AfterSetAcc):-
	CurrAddress<Address,
	NewItemsAcc is ItemsAcc + 1,
	NewItemsAcc \= ItemsPerSet,
	append(BeforeSetAcc,[H],NewBeforeSetAcc),
	getBeforeAndAfterSet(Address,CurrAddress,ItemsPerSet,NewItemsAcc,T,BeforeSet,NewBeforeSetAcc,Set,SetAcc,AfterSet,AfterSetAcc).
getBeforeAndAfterSet(Address,Address,ItemsPerSet,ItemsAcc,[H|T],BeforeSet,BeforeSetAcc,Set,SetAcc,AfterSet,AfterSetAcc):-
	NewItemsAcc is ItemsAcc + 1,
	NewItemsAcc = ItemsPerSet,
	NewCurrAddress is Address + 1,
	append(SetAcc,[H],NewSetAcc),
	getBeforeAndAfterSet(Address,NewCurrAddress,ItemsPerSet,0,T,BeforeSet,BeforeSetAcc,Set,NewSetAcc,AfterSet,AfterSetAcc).
getBeforeAndAfterSet(Address,Address,ItemsPerSet,ItemsAcc,[H|T],BeforeSet,BeforeSetAcc,Set,SetAcc,AfterSet,AfterSetAcc):-
	NewItemsAcc is ItemsAcc + 1,
	NewItemsAcc \= ItemsPerSet,
	append(SetAcc,[H],NewSetAcc),
	getBeforeAndAfterSet(Address,Address,ItemsPerSet,NewItemsAcc,T,BeforeSet,BeforeSetAcc,Set,NewSetAcc,AfterSet,AfterSetAcc).
getBeforeAndAfterSet(Address,CurrAddress,ItemsPerSet,ItemsAcc,[H|T],BeforeSet,BeforeSetAcc,Set,SetAcc,AfterSet,AfterSetAcc):-
	CurrAddress>Address,
	NewItemsAcc is ItemsAcc + 1,
	NewItemsAcc = ItemsPerSet,
	NewCurrAddress is CurrAddress + 1,
	append(AfterSetAcc,[H],NewAfterSetAcc),
	getBeforeAndAfterSet(Address,NewCurrAddress,ItemsPerSet,0,T,BeforeSet,BeforeSetAcc,Set,SetAcc,AfterSet,NewAfterSetAcc).
getBeforeAndAfterSet(Address,CurrAddress,ItemsPerSet,ItemsAcc,[H|T],BeforeSet,BeforeSetAcc,Set,SetAcc,AfterSet,AfterSetAcc):-
	CurrAddress>Address,
	NewItemsAcc is ItemsAcc + 1,
	NewItemsAcc \= ItemsPerSet,
	append(AfterSetAcc,[H],NewAfterSetAcc),
	getBeforeAndAfterSet(Address,CurrAddress,ItemsPerSet,NewItemsAcc,T,BeforeSet,BeforeSetAcc,Set,SetAcc,AfterSet,NewAfterSetAcc).


replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,SetsNum):-
	atom_number(StringIdx,Idx),
	string_length(StringIdx,P),
	logBase2(SetsNum,BitsNum),
	U is BitsNum - P,
	fillZeros(StringIdx,U,FilledIdx),
	atom_number(FilledIdx,FilledIdxDec),
	convertBinToDec(FilledIdxDec,Address),
	length(OldCache,R),
	ItemsPerSet is R//SetsNum,
	getBeforeAndAfterSet(Address,0,ItemsPerSet,0,OldCache,BeforeSet,[],Set,[],AfterSet,[]),
	replaceInCache2(Tag,Idx,Mem,Set,NewCacheAcc,ItemData,setAssoc,BitsNum),
	append(BeforeSet,NewCacheAcc,CacheAccAcc),
	append(CacheAccAcc,AfterSet,NewCache).
	

replaceInCache2(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,BitsNum):-
	isThereInvalid(OldCache),
	atom_number(StringTag,Tag),
	atom_number(StringIdx,Idx),
	string_length(StringIdx,P),
	U is BitsNum - P,
	fillZeros(StringIdx,U,FilledIdx),
	string_concat(StringTag,FilledIdx,X),
	atom_number(X,Z),
	convertBinToDec(Z,MemAddress),
	getDataFromMem(Mem,MemAddress,ItemData),
	thereIsInvalid2(StringTag,OldCache,0,NewCache,[],ItemData,fullyAssoc,BitsNum).
replaceInCache2(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,BitsNum):-
	\+isThereInvalid(OldCache),
	atom_number(StringTag,Tag),
	atom_number(StringIdx,Idx),
	string_length(StringIdx,P),
	U is BitsNum - P,
	fillZeros(StringIdx,U,FilledIdx),
	string_concat(StringTag,FilledIdx,X),
	atom_number(X,Z),
	convertBinToDec(Z,MemAddress),
	getDataFromMem(Mem,MemAddress,ItemData),
	getMaxOrder(OldCache,0,MaxOrder),
	thereIsNotInvalid2(StringTag,OldCache,NewCache,[],MaxOrder,ItemData,fullyAssoc,BitsNum).
	
	
thereIsNotInvalid2(_,[],NewCache,NewCache,_,_,fullyAssoc,_).
thereIsNotInvalid2(Tag,[item(tag(A),data(B),ValidBit,Order)|T],NewCache,CacheAcc,MaxOrder,ItemData,fullyAssoc,BitsNum):-
	Order\=MaxOrder,
	NewOrder is Order + 1,
	append(CacheAcc,[item(tag(A),data(B),ValidBit,NewOrder)],NewCacheAcc),
	thereIsNotInvalid2(Tag,T,NewCache,NewCacheAcc,MaxOrder,ItemData,fullyAssoc,BitsNum).
thereIsNotInvalid2(Tag,[item(tag(_),data(_),_,Order)|T],NewCache,CacheAcc,MaxOrder,ItemData,fullyAssoc,BitsNum):-
	Order=MaxOrder,
	string_length(Tag,R2),
	Z is 6 - R2 - BitsNum,
	fillZeros(Tag,Z,Q),
	append(CacheAcc,[item(tag(Q),data(ItemData),1,0)],NewCacheAcc),
	thereIsNotInvalid2(Tag,T,NewCache,NewCacheAcc,MaxOrder,ItemData,fullyAssoc,BitsNum).
	
	
thereIsInvalid2(_,[],_,NewCache,NewCache,_,fullyAssoc,_).
thereIsInvalid2(Tag,[item(tag(_),data(_),0,_)|T],Inserted,NewCache,CacheAcc,ItemData,fullyAssoc,BitsNum):-
	Inserted=0,
	string_length(Tag,R2),
	Z is 6 - R2 - BitsNum,
	fillZeros(Tag,Z,Q),
	append(CacheAcc,[item(tag(Q),data(ItemData),1,0)],NewCacheAcc),
	thereIsInvalid2(Tag,T,1,NewCache,NewCacheAcc,ItemData,fullyAssoc,BitsNum).
thereIsInvalid2(Tag,[item(tag(A),data(B),ValidBit,C)|T],Inserted,NewCache,CacheAcc,ItemData,fullyAssoc,BitsNum):-
	Inserted=1,
	incrementOtherOrders([item(tag(A),data(B),ValidBit,C)|T],[],IncrementedCache),
	append(CacheAcc,IncrementedCache,NewCacheAcc),
	thereIsInvalid2(Tag,[],Inserted,NewCache,NewCacheAcc,ItemData,fullyAssoc,BitsNum).
thereIsInvalid2(Tag,[item(tag(A),data(B),ValidBit,Order)|T],Inserted,NewCache,CacheAcc,ItemData,fullyAssoc,BitsNum):-
	Inserted=0,
	ValidBit=1,
	NewOrder is Order + 1,
	append(CacheAcc,[item(tag(A),data(B),ValidBit,NewOrder)],NewCacheAcc),
	thereIsInvalid2(Tag,T,Inserted,NewCache,NewCacheAcc,ItemData,fullyAssoc,BitsNum).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
%getData & runProgram
%-------------------
getData(StringAddress,OldCache,_,NewCache,Data,HopsNum,Type,BitsNum,hit):-
	getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
	NewCache = OldCache.
getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,miss):-
	\+getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
	atom_number(StringAddress,Address),
	convertAddress(Address,BitsNum,Tag,Idx,Type),
	replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,Type,BitsNum).


runProgram([],OldCache,_,OldCache,[],[],_,_).
runProgram([Address|AdressList],OldCache,Mem,FinalCache,[Data|OutputDataList],[Status|StatusList],Type,NumOfSets):-
	getNumBits(NumOfSets,Type,OldCache,BitsNum),
	(Type = setAssoc, Num = NumOfSets; Type \= setAssoc, Num = BitsNum),
	getData(Address,OldCache,Mem,NewCache,Data,_,Type,Num,Status),
	runProgram(AdressList,NewCache,Mem,FinalCache,OutputDataList,StatusList,Type,NumOfSets).