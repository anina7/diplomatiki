----------------------------------------
-- Auxiliary
----------------------------------------

module Auxiliary where


newtype Set a = Set [a]

instance Eq a => Eq (Set a) where
    (Set l1) == (Set l2)  =  all (\x1 -> x1 `elem` l2) l1 &&
                             all (\x2 -> x2 `elem` l1) l2

instance (Eq a, Show a) => Show (Set a) where
    showsPrec p (Set []) = ("{}" ++)
    showsPrec p (Set l) =
        let showStatesAux [] = id
            showStatesAux [q] = showsPrec 0 q
            showStatesAux (q : qs) = showsPrec 0 q . ("," ++) .
                                     showStatesAux qs
        in  ("{" ++) . showStatesAux l . ("}" ++)

update :: Eq a => a -> b -> (a -> b) -> a -> b
update a b f x = if x == a then b else f x

splitLast :: [a] -> ([a], a)
splitLast [] = error "splitLast with empty string"
splitLast [a] = ([], a)
splitLast (a : s) =
    let (s', l) = splitLast s
    in  (a : s', l)

uniq :: Eq a => [a] -> [a]
uniq l =
    let aux [] l' = []
        aux (x : l) l' = if x `elem` l' then aux l l' else x : aux l (x : l')
    in  aux l []

setEmpty :: Eq a => Set a -> Bool
setEmpty (Set l) = null l

setElem :: Eq a => a -> Set a -> Bool
setElem a (Set l) = elem a l

setMap :: (Eq a, Eq b) => (a -> b) -> Set a -> Set b
setMap f (Set l) = Set (map f l)

setFilter :: Eq a => (a -> Bool) -> Set a -> Set a
setFilter f (Set l) = Set (filter f l)

setUnion :: Eq a => Set a -> Set a -> Set a
setUnion (Set l1) (Set l2) = Set (uniq (l1 ++ l2))

setBigUnion :: Eq a => Set (Set a) -> Set a
setBigUnion (Set l) = foldl setUnion (Set []) l

setIntersect :: Eq a => Set a -> Set a -> Set a
setIntersect (Set l1) (Set l2) = Set (filter (\x -> x `elem` l1) l2)

setDifference :: Eq a => Set a -> Set a -> Set a
setDifference (Set l1) (Set l2) = Set (filter (\x -> not (x `elem` l2)) l1)

setCardinality :: Eq a => Set a -> Int
setCardinality (Set l) = length l

powerset :: Eq a => Set a -> Set (Set a)
powerset (Set l) =
    let aux [] = [[]]
        aux (a : l) = concat [[a : s, s] | s <- aux l]
    in  Set (map Set (aux l))

setProduct :: (Eq a, Eq b) => Set a -> Set b -> Set (a, b)
setProduct (Set xs) (Set ys) =
    let aux [] ys = []
        aux (x : xs) ys = map (\y -> (x, y)) ys ++ aux xs ys
    in  Set (aux xs ys)
