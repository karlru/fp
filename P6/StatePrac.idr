import Control.Monad.State

data Tree a = Empty
            | Node (Tree a) a (Tree a)

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred" 
                      (Node Empty "Sheila" Empty)) "Alice" 
                (Node Empty "Bob" (Node Empty "Eve" Empty))


-- flattening tree
flatten : Tree a -> List a
flatten Empty = []
flatten (Node left val right) = flatten left ++ val :: flatten right

-- labelling tree without using state monad
treeLabelWith1 : Stream labelType -> Tree a -> (Stream labelType, Tree (labelType, a))
treeLabelWith1 lbls Empty                 = (lbls, Empty)
treeLabelWith1 lbls (Node left val right) = 
      let (lblThis :: lblsLeft, left_labelled) = treeLabelWith1 lbls left
          (lblsRight, right_labelled)          = treeLabelWith1 lblsLeft right
                in
          (lblsRight, Node left_labelled (lblThis, val) right_labelled)

treeLabel1 : Tree a -> Tree (Integer, a)
treeLabel1 tree = snd (treeLabelWith1 [1..] tree)

-- labelling tree with state monad
treeLabelWith2 : Tree a -> State (Stream labelType) (Tree (labelType, a))
treeLabelWith2 Empty = pure Empty
treeLabelWith2 (Node left val right) = do left_labelled <- treeLabelWith2 left
                                          (this :: rest) <- get
                                          put rest
                                          right_labelled <- treeLabelWith2 right
                                          pure (Node left_labelled (this, val) right_labelled)
-- stream annab errorit
treeLabel2 : Tree a -> Tree (Integer, a)
--treeLabel2 tree = evalState (treeLabelWith2 tree) [1..]

-- ex 12_1
update : (stateType -> stateType) -> State stateType ()
update f = do st <- get
              put (f st)

increase : Nat -> State Nat ()
increase x = update (+x)

countEmpty : Tree a -> State Nat ()
countEmpty Empty                 = update (+1)
countEmpty (Node left val right) = do countEmpty left
                                      countEmpty right
                                      
