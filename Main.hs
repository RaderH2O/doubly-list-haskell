data DoublyList a = LeftEnd a (DoublyList a)
                  | Middle a (DoublyList a) (DoublyList a)
                  | RightEnd a (DoublyList a)
                  | Single a

instance (Show a) => Show (DoublyList a) where
  show (LeftEnd a b)    = "LeftEnd " ++ (show a) ++ ", " ++ (show b)
  show (Middle a _ c)   = "Middle " ++ (show a) ++ ", " ++ (show c)
  show (RightEnd a b)   = "RightEnd " ++ (show a)

getValue :: DoublyList a -> a
getValue (LeftEnd a _)  = a
getValue (Middle a _ _) = a
getValue (RightEnd a _) = a
getValue (Single a)     = a

nextElement :: DoublyList a -> DoublyList a
nextElement (LeftEnd _ a)               = a
nextElement (Middle _ _ (RightEnd a _)) = Single a
nextElement (Middle _ _ a)              = a
nextElement a                           = a

previousElement :: DoublyList a -> DoublyList a
previousElement (Middle _ a _)  = a
previousElement (RightEnd _ a)  = a
previousElement a               = a
-- NOTE: ^ this line handles "LeftEnd" and "Single" and any invalid argument

doublyToString :: (Show a) => DoublyList a -> String
doublyToString (LeftEnd a b) = (show a) ++ ' ' : (doublyToString b)
doublyToString (Middle a _ b) = (show a) ++ ' ' : (doublyToString b)
doublyToString (RightEnd a _) = show a
doublyToString (Single a) = show a

doublyToList :: DoublyList a -> [a]
doublyToList (LeftEnd a b)      = a : (doublyToList b)
doublyToList (Middle a _ b)     = a : (doublyToList b)
doublyToList (RightEnd a _)     = [a]
doublyToList (Single a)         = [a]

listToDoublyMiddle :: DoublyList a -> [a] -> DoublyList a
listToDoublyMiddle prevDoubly [x]       = RightEnd x prevDoubly
listToDoublyMiddle prevDoubly (x:xs)    = e1
  where e1 = Middle x prevDoubly e2
        e2 = listToDoublyMiddle e1 xs

listToDoubly :: [a] -> DoublyList a
listToDoubly (x:xs) = e1
  where e1 = LeftEnd x e2
        e2 = listToDoublyMiddle e1 xs

(.+.) :: DoublyList a -> [a] -> DoublyList a
(.+.) a1 a2 = listToDoubly $ (doublyToList a1) ++ a2

popDoubly :: DoublyList a -> DoublyList a
popDoubly a = listToDoubly $ take (length lista - 1) lista
  where lista = doublyToList a

values :: DoublyList Int
values = e1
  where e1 = LeftEnd 1 e2
        e2 = Middle 2 e1 e3
        e3 = Middle 3 e2 e4
        e4 = Middle 4 e3 e5
        e5 = RightEnd 5 e4
