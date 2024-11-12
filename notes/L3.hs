{- Minimum Tree Problem -}
data BTree = Leaf Int | Branch BTree BTree -- This is a dataType
-- Which takes either a Leaf of type Int or Branch have two params Btree Btree

mintree :: BTree -> BTree
mintree b = t
    where
        (t, m) = mtree b m
        mtree :: BTree -> Int -> (BTree, Int)
        mtree (Leaf v) m = (Leaf m, v )
        mtree (Branch l r) m =
            let (t1, m1) = mtree l m
                (tr, mr) = mtree r m
            in (Branch t1 tr, min m1 mr)


{- Type Classes -}