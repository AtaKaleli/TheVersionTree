--Ata Kaleli 2385474
--I read and accept the submission rules and the extra rules. This is my own work that is done by myself only


data TernaryTree = Empty | Node String TernaryTree TernaryTree TernaryTree | NodeExist | NotReachable | NodeNotFound  deriving (Show,Eq,Ord)



--Part One
charToInt::Char->Int -- simple charToInt function

charToInt x = if x=='0' then 0
         else if x=='1' then 1
		 else if x=='2' then 2
		 else if x=='3' then 3
		 else if x=='4' then 4
		 else if x=='5' then 5
		 else if x=='6' then 6
		 else if x=='7' then 7
		 else if x=='8' then 8
		 else if x=='9' then 9
		 else -1
--------------------------------------------------------------------	

--Part Two
insertNode::TernaryTree->[Char]->TernaryTree


--checkChar helps me to assign the numbers in the myapp.. string, it makes an integer list for me. For ex: checkChar "myapp.1.2.3 will be [1,2,3]"
--I used this helper in my insertNode function and findNode function
--the problem with this function is that it detects the NodeExist and NotReachable cases well, and does not gives error, but it inserts these errors in to the tree. I could not manage to solve this problem
checkChar []=[]
checkChar (x:xs) = if charToInt x /= -1 then [charToInt x] ++ checkChar xs else (checkChar xs)

helperInsert Empty x (z:zs)= NotReachable -- this is the case where we have an empty tree but still an integer list. For ex: Empty [1,2]
helperInsert (Node y leftSide midSide rightSide) x [] = if x==y then NodeExist else NotReachable -- when the node exist I assigned it as NodeExist
helperInsert Empty x []= Node x Empty Empty Empty --this is the case where I add a new node to ternary tree
helperInsert  (Node y leftSide midSide rightSide) x (z:zs)= -- this is the recursive part of insertion. I go left or mıd or right side depends on the integer.
													if z==1 then (Node y (helperInsert  leftSide x zs ) midSide rightSide)
													else if z==2 then (Node y  leftSide (helperInsert  midSide x zs )  rightSide)
												    else if z==3 then (Node y  leftSide midSide (helperInsert  rightSide x zs ))
												    else NotReachable
													
insertNode	Empty x = Node x Empty Empty Empty -- base case of insertNode
insertNode (Node y leftSide midSide rightSide) x = helperInsert  (Node y leftSide midSide rightSide) x (checkChar x)-- function call of insertNode.

------------------------------------------------------------------------------------------------
--Part Three
totalNodes::TernaryTree->Int

totalNodes Empty = 0
totalNodes(Node y leftSide midSide rightSide)= 1+ totalNodes leftSide + totalNodes midSide + totalNodes rightSide -- I simply count the total nodes by traversing the whole tree

--------------------------------------------------------------------------------------------------------------------------
--Part Four
height::TernaryTree->Int

								
height Empty=0
height (Node y leftSide midSide rightSide)= 1+ max (max (height leftSide) (height midSide)) (height rightSide)-- I counted the height of every side, then I take the max of the three of them. At the end, I added +1 as I have the initial root

											
-------------------------------------------------------------------------------------------------------------------------
--Part Five
levelcount::TernaryTree->Int->Int

levelcount Empty x = 0-- This is the base case of levelcount. I could not manage to do the rest

---------------------------------------------------------------------------------------------------------------------------
--Part Six
findNode::TernaryTree->[Char]->TernaryTree

--helperFind is a helper function.
helperFind Empty x (z:zs)=NotReachable-- this is the case when there is no tree but we have still integer list.
helperFind (Node y leftSide midSide rightSide) x [] = if x==y then (Node y leftSide midSide rightSide) else NotReachable -- if x==y in this case, I show the three
helperFind Empty x []= NodeNotFound-- if the tree is empty and I dont have any integer in my list, this means the searhed value is not in the list
helperFind (Node y leftSide midSide rightSide) x (z:zs)= if z == 1 then (helperFind  leftSide x zs)-- those are the recursive cases for helperFind 
													else if z == 2 then (helperFind  midSide x zs)
													else if z == 3 then (helperFind  rightSide x zs)
													else NotReachable-- if user searched for myapp.4 , myapp.5.6 etc. , it is NotReachable
findNode  Empty x = NodeNotFound-- this is the base case for findNode
findNode (Node y leftSide midSide rightSide) x = helperFind  (Node y leftSide midSide rightSide) x (checkChar x) --this is the function call for findNode

--------------------------------------------------------------------------------------------------------------------------

--input--
leftSide = Node "MyApp.1" (Node "MyApp.1.1" Empty Empty Empty) (Node "MyApp.1.2" Empty Empty Empty) Empty
midSide = Node "MyApp.2" Empty Empty Empty
rightSide = Node "MyApp.3" (Node "MyApp.3.1" (Node "MyApp.3.1.1" Empty Empty Empty) Empty (Node "MyApp.3.1.3" Empty Empty Empty) ) (Node "MyApp.3.2" Empty Empty Empty) (Node "MyApp.3.3" Empty Empty Empty)
figureOne = Node "MyApp" leftSide midSide rightSide

	 
		 















 































