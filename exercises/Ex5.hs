import Text.XHtml (height)

data AVLTree a = Leaf | Branch Int (AVLTree a) a (AVLTree a) deriving (Show, Eq)

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

height :: AVLTree a -> Int
height Leaf = 0
height (Branch h _ _ _) = h
