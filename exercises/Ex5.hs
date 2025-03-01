data AVLTree a = Leaf | Branch Int (AVLTree a) a (AVLTree a)
{- 
    -- Example of an AVL tree in pictorial form:
    -- 
    --        3
    --       / \
    --      2   4
    --     /     \
    --    1       5
    --
    -- This tree is balanced because the heights of the subtrees of every node differ by at most 1.

 -}

 