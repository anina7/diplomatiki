----------------------------------------
-- Finite automata
----------------------------------------

module Automata where

import Auxiliary


-- Definition: three types
----------------------------------------

data FDA symbol state =
    FDA (Set symbol)
        (Set state)
        ((state, symbol) -> state)
        state
        (Set state)

data FNA symbol state =
    FNA (Set symbol)
        (Set state)
        ((state, symbol) -> Set state)
        state
        (Set state)

data FNAZ symbol state =
    FNAZ (Set symbol)
         (Set state)
         ((state, Maybe symbol) -> Set state)
         state
         (Set state)


-- Operation
----------------------------------------

runFDA :: (Eq symbol, Eq state) =>
          FDA symbol state -> [symbol] -> Bool
runFDA (FDA alphabet states delta start final) string =
    let delta_star (q, []) = q
        delta_star (q, s) =
            let (alpha, a) = splitLast s
            in  delta (delta_star (q, alpha), a)
    in  delta_star (start, string) `setElem` final

delta_bar :: (Eq symbol, Eq state) =>
             ((state, symbol) -> Set state) ->
             (Set state, symbol) -> Set state
delta_bar delta (Set [], s) = Set []
delta_bar delta (Set (q : r), s) =
    delta (q, s) `setUnion` delta_bar delta (Set r, s)

runFNA :: (Eq symbol, Eq state) =>
          FNA symbol state -> [symbol] -> Bool
runFNA (FNA alphabet states delta start final) string =
    let delta_star (q, []) = Set [q]
        delta_star (q, s) =
            let (alpha, a) = splitLast s
            in  delta_bar delta (delta_star (q, alpha), a)
    in  not (setEmpty (delta_star (start, string) `setIntersect` final))

epsilonClosure :: (Eq symbol, Eq state) =>
                  ((state, Maybe symbol) -> Set state) ->
                  Set state -> Set state
epsilonClosure delta r =
    let epsilonClosure 0 =
            r
        epsilonClosure n =
            setBigUnion (setMap (\q -> delta (q, Nothing))
                                (epsilonClosure (n-1)))
        calcUnion closure n =
            let closure' = closure `setUnion` epsilonClosure n
            in  if closure == closure' then
                    closure
                else
                    calcUnion closure' (n+1)
    in  calcUnion (Set []) 0

delta_barZ :: (Eq symbol, Eq state) =>
              ((state, Maybe symbol) -> Set state) ->
              (Set state, symbol) -> Set state
delta_barZ delta (Set [], s) = Set []
delta_barZ delta (Set (q : r), s) =
    delta (q, Just s) `setUnion` delta_barZ delta (Set r, s)

runFNAZ :: (Eq symbol, Eq state) =>
           FNAZ symbol state -> [symbol] -> Bool
runFNAZ (FNAZ alphabet states delta start final) string =
    let delta_star (q, []) = epsilonClosure delta (Set [q])
        delta_star (q, s) =
            let (alpha, a) = splitLast s
            in  epsilonClosure delta
                    (delta_barZ delta (delta_star (q, alpha), a))
    in  not (setEmpty (delta_star (start, string) `setIntersect` final))


-- Easy conversions
----------------------------------------

convFDAtoFNA :: (Eq symbol, Eq state) =>
                FDA symbol state -> FNA symbol state
convFDAtoFNA (FDA alphabet states delta start final) =
    let delta' (q, a) = Set [delta (q, a)]
    in  FNA alphabet states delta' start final

convFNAtoFNAZ :: (Eq symbol, Eq state) =>
                 FNA symbol state -> FNAZ symbol state
convFNAtoFNAZ (FNA alphabet states delta start final) =
    let delta' (q, Just a) = delta (q, a)
        delta' (q, Nothing) = Set []
    in  FNAZ alphabet states delta' start final

convFDAtoFNAZ :: (Eq symbol, Eq state) =>
                 FDA symbol state -> FNAZ symbol state
convFDAtoFNAZ = convFNAtoFNAZ . convFDAtoFNA


-- Hard conversions
----------------------------------------

-- Silly algorithm

sillyFNAZtoFDA :: (Eq symbol, Eq state) =>
                  FNAZ symbol state -> FDA symbol (Set state)
sillyFNAZtoFDA (FNAZ alphabet states delta start final) =
    let states' = powerset states
        delta' (q', a) = epsilonClosure delta (delta_barZ delta (q', a))
        start' = epsilonClosure delta (Set [start])
        final' = setFilter (\q' -> not (setEmpty (q' `setIntersect` final)))
                           states'
    in  FDA alphabet states' delta' start' final'

sillyFNAtoFDA :: (Eq symbol, Eq state) =>
                 FNA symbol state -> FDA symbol (Set state)
sillyFNAtoFDA = sillyFNAZtoFDA . convFNAtoFNAZ

sillyFNAZtoFNA :: (Eq symbol, Eq state) =>
                  FNAZ symbol state -> FNA symbol (Set state)
sillyFNAZtoFNA = convFDAtoFNA . sillyFNAZtoFDA

-- Algorithm: FNAZ -> FDA

convFNAZtoFDA :: (Eq symbol, Eq state) =>
                  FNAZ symbol state -> FDA symbol (Set state)
convFNAZtoFDA (FNAZ alphabet states delta start final) =
    let start' = epsilonClosure delta (Set [start])
        delta0 (q, a) = Set []
        loop1 bq' delta' marked =
            case bq' `setDifference` marked of
                (Set []) -> (bq', delta')
                (Set (q' : _)) ->
                    let loop2 bq' delta' (Set []) = (bq', delta')
                        loop2 bq' delta' (Set (a : symbols)) =
                            let q'' = epsilonClosure delta
                                         (delta_barZ delta (q', a))
                                delta'' = update (q', a) q'' delta'
                                bq'' = bq' `setUnion` Set [q'']
                            in  loop2 bq'' delta'' (Set symbols)
                        (bq'', delta'') = loop2 bq' delta' alphabet
                    in  loop1 bq'' delta'' (Set [q'] `setUnion` marked)
        (states', delta') = loop1 (Set [start']) delta0 (Set [])
        final' = setFilter (\q' -> not (setEmpty (q' `setIntersect` final)))
                           states'
    in  FDA alphabet states' delta' start' final'

convFNAtoFDA :: (Eq symbol, Eq state) =>
                 FNA symbol state -> FDA symbol (Set state)
convFNAtoFDA = convFNAZtoFDA . convFNAtoFNAZ

convFNAZtoFNA :: (Eq symbol, Eq state) =>
                  FNAZ symbol state -> FNA symbol (Set state)
convFNAZtoFNA = convFDAtoFNA . convFNAZtoFDA


-- Optimizations
----------------------------------------

-- Algorithm 2.2

unreachableStates :: (Eq symbol, Eq state) =>
                     FDA symbol state -> Set state
unreachableStates (FDA alphabet states delta start final) =
    let loop1 r =
            let loop2 (Set []) r = r
                loop2 (Set ((q, a) : qas)) r =
                    let q' = delta (q, a)
                    in  if not (q' `setElem` r) then
                            loop2 (Set qas) (r `setUnion` Set [q'])
                        else
                            loop2 (Set qas) r
                r' = loop2 (setProduct r alphabet) r
            in  if r == r' then r else loop1 r'
    in  states `setDifference` loop1 (Set [start])

removeUnreachableStates :: (Eq symbol, Eq state) =>
                           FDA symbol state -> FDA symbol state
removeUnreachableStates fda@(FDA alphabet states delta start final) =
    let unreachable = unreachableStates fda
    in  FDA alphabet
            (states `setDifference` unreachable)
            delta
            start
            (final `setDifference` unreachable)

-- Algorithm 2.3

equivalentStates :: (Eq symbol, Eq state) =>
                    FDA symbol state -> (Set (state, state))
equivalentStates (FDA alphabet states delta start final) =
    let (t, s) =
            let loop1 [] (tl, sl) = (tl, sl)
                loop1 (qi : qs) (tl, sl) =
                   let loop2 [] (tl, sl) = (tl, sl)
                       loop2 (qj : qs) (tl, sl) =
                           if (qi `setElem` final) == (qj `setElem` final) then
                               loop2 qs (tl, (qi, qj) : sl)
                           else
                               loop2 qs ((qi, qj) : tl, sl)
                   in  loop1 qs (loop2 qs (tl, sl))
                Set statesl = states
                (tl, sl) = loop1 statesl ([], [])
            in  (Set tl, Set sl)
        loop1 (t, s) =
            let loop2 (Set []) (t, s) = (t, s)
                loop2 (Set (((qi, qj), a) : qpas)) (t, s) =
                    let qx = delta (qi, a)
                        qy = delta (qj, a)
                    in  if (qx, qy) `setElem` t || (qy, qx) `setElem` t then
                            loop2 (Set qpas)
                                  (t `setUnion` Set [(qi, qj)],
                                   s `setDifference` Set [(qi, qj)])
                        else
                            loop2 (Set qpas) (t, s)
                (t', s') = loop2 (setProduct s alphabet) (t, s)
            in  if (t, s) == (t', s') then (t, s) else loop1 (t', s')
        (tfinal, sfinal) = loop1 (t, s)
    in  sfinal

mergeEquivalentStates :: (Eq symbol, Eq state) =>
                         FDA symbol state -> FDA symbol state
mergeEquivalentStates fda =
    let merge (FDA alphabet states delta start final) (qi, qj) =
           let states' = states `setDifference` Set [qj]
               delta' (q, a) = let q' = delta (q, a)
                               in  if q' == qj then qi else q'
               start' = if qj == start then qi else start
               final' = final `setDifference` Set [qj]
           in  FDA alphabet states' delta' start' final'
        Set qp = equivalentStates fda
    in  foldl merge fda qp


-- Pretty printing
----------------------------------------

instance (Eq symbol, Eq state, Show symbol, Show state) =>
         Show (FDA symbol state) where
    showsPrec p (FDA (Set alphabetl) (Set statesl) delta start (Set finall)) =
        let printList [] = id
            printList [str] = (str ++) . ("\n" ++)
            printList (str : l) = (str ++) . ("\t" ++) . printList l
            loop [] = id
            loop (q : qs) =
                printList ((show q :
                            map (\a -> show (delta (q, a))) alphabetl) ++
                           [if q `elem` finall then "yes" else ""] ++
                           [if q == start then "*" else ""]) .
                loop qs
        in  printList (("" : map show alphabetl) ++ ["-|"]) .
            loop statesl

instance (Eq symbol, Eq state, Show symbol, Show state) =>
         Show (FNA symbol state) where
    showsPrec p (FNA (Set alphabetl) (Set statesl) delta start (Set finall)) =
        let printList [] = id
            printList [str] = (str ++) . ("\n" ++)
            printList (str : l) = (str ++) . ("\t" ++) . printList l
            showStates (Set []) = ""
            showStates (Set l) =
                let showStatesAux [] = ""
                    showStatesAux [q] = show q
                    showStatesAux (q : qs) = show q ++ "," ++ showStatesAux qs
                in  "{" ++ showStatesAux l ++ "}"
            loop [] = id
            loop (q : qs) =
                printList ((show q :
                            map (\a -> showStates (delta (q, a))) alphabetl) ++
                           [if q `elem` finall then "yes" else ""] ++
                           [if q == start then "*" else ""]) .
                loop qs
        in  printList (("" : map show alphabetl) ++ ["-|"]) .
            loop statesl

instance (Eq symbol, Eq state, Show symbol, Show state) =>
         Show (FNAZ symbol state) where
    showsPrec p (FNAZ (Set alphabetl) (Set statesl) delta start (Set finall)) =
        let printList [] = id
            printList [str] = (str ++) . ("\n" ++)
            printList (str : l) = (str ++) . ("\t" ++) . printList l
            showStates (Set []) = ""
            showStates (Set l) =
                let showStatesAux [] = ""
                    showStatesAux [q] = show q
                    showStatesAux (q : qs) = show q ++ "," ++ showStatesAux qs
                in  "{" ++ showStatesAux l ++ "}"
            loop [] = id
            loop (q : qs) =
                printList ((show q :
                            map (\a -> showStates (delta (q, a)))
                                (map Just alphabetl ++ [Nothing])) ++
                           [if q `elem` finall then "yes" else ""] ++
                           [if q == start then "*" else ""]) .
                loop qs
        in  printList (("" :
                        map show alphabetl ++ ["\\epsilon"]) ++
                       ["-|"]) .
            loop statesl


----------------------------------------
-- Regular expressions
----------------------------------------


-- Definition
----------------------------------------

data Regexp symbol =
    REempty
  | REsymbol symbol
  | REconcat (Regexp symbol) (Regexp symbol)
  | REor (Regexp symbol) (Regexp symbol)
  | REstar (Regexp symbol)
  deriving Eq


-- Conversion to FNAZ
----------------------------------------

convREtoFNAZ :: Eq symbol => Set symbol -> Regexp symbol -> FNAZ symbol Int
convREtoFNAZ alphabet r =
    let aux next REempty =
            ([], next, next, next+1)
        aux next (REsymbol a) =
            ([((next, Just a), Set [next+1])],
             next, next+1, next+2)
        aux next (REconcat r s) =
            let (deltas1, start1, end1, next1) = aux next r
                (deltas2, start2, end2, next2) = aux next1 s
            in  (deltas1 ++ deltas2 ++
                 [((end1, Nothing), Set [start2])],
                 start1, end2, next2)
        aux next (REor r s) =
            let (deltas1, start1, end1, next1) = aux (next+2) r
                (deltas2, start2, end2, next2) = aux next1 s
            in  (deltas1 ++ deltas2 ++
                 [((next, Nothing), Set [start1, start2]),
                  ((end1, Nothing), Set [next+1]),
                  ((end2, Nothing), Set [next+1])],
                 next, next+1, next2)
        aux next (REstar r) =
            let (deltas, start, end, next') = aux (next+1) r
            in  (deltas ++
                 [((end, Nothing), Set [start]),
                  ((start, Nothing), Set [next])],
                 start, next, next')
        calcDelta [] p = Set []
        calcDelta ((p', q') : deltas) p =
            if p == p' then
                q' `setUnion` calcDelta deltas p
            else
                calcDelta deltas p
        (deltas, start, end, next) = aux 0 r
    in  FNAZ alphabet
             (Set (enumFromTo 0 (next-1)))
             (calcDelta deltas)
             start
             (Set [end])


-- Pretty printing
----------------------------------------

instance (Show symbol) => Show (Regexp symbol) where
    showsPrec p REempty =
        ("\\epsilon" ++)
    showsPrec p (REsymbol a) =
        showsPrec p a
    showsPrec p (REconcat r s) =
        showParen (p > 3) (showsPrec 3 r . (" " ++) . showsPrec 3 s)
    showsPrec p (REor r s) =
        showParen (p > 2) (showsPrec 2 r . (" | " ++) . showsPrec 2 s)
    showsPrec p (REstar r) =
        showParen (p > 4) (showsPrec 4 r . ("*" ++))


----------------------------------------
-- Push down automata
----------------------------------------


-- Definition
----------------------------------------

data PDA symbol state stack =
    PDA (Set symbol)
        (Set state)
        (Set stack)
        ((state, stack, Maybe symbol) -> Set ([stack], state))
        state
        stack
        (Set state)


-- Operation
----------------------------------------

runPDA :: (Eq symbol, Eq state, Eq stack) =>
            PDA symbol state stack -> [symbol] -> Bool
runPDA (PDA alphabet states stack delta q0 h0 final) string =
    let goes (q, gh, alpha) =
            let (g, h) = splitLast gh
                Set me = delta (q, h, Nothing)
            in  [ (q', g ++ g', alpha) | (g', q') <- me ] ++
                case alpha of
                    [] -> []
                    (a : beta) ->
                        let Set ma = delta (q, h, Just a)
                        in  [ (q', g ++ g', beta) | (g', q') <- ma ]
        isFinal (q, g, []) = q `setElem` final
        isFinal _  = False
        loop [] csl = False
        loop (c : cl) csl =
            if isFinal c then
                True
            else if c `elem` csl then
                False
            else
                loop (cl ++ goes c) (c : csl)
    in  loop [(q0, [h0], string)] []


-- Pretty printing
----------------------------------------

instance (Eq symbol, Eq state, Eq stack,
          Show symbol, Show state, Show stack) =>
         Show (PDA symbol state stack) where
    showsPrec p (PDA (Set alphabetl) (Set statesl) (Set stackl)
                     delta q0 h0 (Set finall)) =
        let printList [] = id
            printList [str] = (str ++) . ("\n" ++)
            printList (str : l) = (str ++) . ("\t" ++) . printList l
            showMove (q, h, a, g, q') =
                let showSymbol (Just a) = ["read(" ++ show a ++ ")"]
                    showSymbol Nothing  = ["keep"]
                    showStack h [] = ["pop"]
                    showStack h g@(h' : g') =
                        if h == h' then
                            if null g' then [] else ["push(" ++ show g' ++ ")"]
                        else
                            ["pop", "push(" ++ show g ++ ")"]
                    showMoveAux [] = ""
                    showMoveAux [str] = str
                    showMoveAux (str : l) = str ++ "/" ++ showMoveAux l
                    ml = showSymbol a ++
                         showStack h g ++
                         if q /= q' then ["move(" ++ show q' ++ ")"] else []
                in  showMoveAux ml
            showStates (Set []) = ""
            showStates (Set l) =
                let showStatesAux [] = ""
                    showStatesAux [m] = showMove m
                    showStatesAux (m : ms) = showMove m ++ "," ++
                                             showStatesAux ms
                in  "{" ++ showStatesAux l ++ "}"
            loop [] = id
            loop (q : qs) =
                printList ((show q :
                            map (\h -> showStates (setBigUnion (Set
                                 (map (\a ->
                                      (setMap (\(g, q') -> (q, h, a, g, q'))
                                              (delta (q, h, a))))
                                      (map Just alphabetl ++ [Nothing])))))
                                 stackl) ++
                           [if q `elem` finall then "yes" else ""] ++
                           [if q == q0 then "*" else ""]) .
                loop qs
        in  printList (("" : map (\h -> show h ++
                                        (if h == h0 then "*" else ""))
                                 stackl) ++ ["-|"]) .
            loop statesl


----------------------------------------
-- Examples from our book
----------------------------------------

-- Example 2.1

ex1 :: FDA Char Char
ex1 =
    let a = Set ['0', '1']
        q = Set ['a', 'b']
        delta ('a', '0') = 'a'
        delta ('a', '1') = 'b'
        delta ('b', '0') = 'b'
        delta ('b', '1') = 'a'
        q0 = 'a'
        f = Set ['a']
    in  FDA a q delta q0 f

-- Example 2.2

ex2 :: FNA Char Char
ex2 =
    let a = Set ['0', '1']
        q = Set ['a', 'b']
        delta ('a', '0') = Set []
        delta ('a', '1') = Set ['a', 'b']
        delta ('b', '0') = Set ['b']
        delta ('b', '1') = Set ['b']
        q0 = 'a'
        f = Set ['b']
    in  FNA a q delta q0 f

-- Example 2.3

ex3 :: FDA Char (Set Char)
ex3 =
    convFNAtoFDA ex2

-- Example 2.4

ex4re :: Regexp Char
ex4re = REconcat (REsymbol 'a')
                 (REconcat (REstar (REor (REsymbol 'a')
                                         (REsymbol 'b')))
                           (REsymbol 'b'))

ex4a :: FNAZ Char Int
ex4a = convREtoFNAZ (Set ['a', 'b']) ex4re

ex4b :: FDA Char (Set Int)
ex4b = convFNAZtoFDA ex4a

ex4c :: FDA Char (Set Int)
ex4c = sillyFNAZtoFDA ex4a

ex4d :: Set (Set Int)
ex4d = unreachableStates ex4c

ex4e :: FDA Char (Set Int)
ex4e = removeUnreachableStates ex4c

ex4f :: Set (Set Int, Set Int)
ex4f = equivalentStates ex4b

ex4g :: FDA Char (Set Int)
ex4g = mergeEquivalentStates ex4b

-- Example 2.9

ex5 :: PDA Char Char Char
ex5 =
    let a = Set ['(', ')']
        q = Set ['S', 'T']
        h = Set ['X', 'I']
        delta('S', 'X', Just '(') = Set [("XI", 'S')]
        delta('S', 'X', Nothing)  = Set [("X", 'T')]
        delta('S', 'I', Just '(') = Set [("II", 'S')]
        delta('S', 'I', Just ')') = Set [("", 'S')]
        delta _ = Set []
        q0 = 'S'
        h0 = 'X'
        f = Set ['T']
    in  PDA a q h delta q0 h0 f

-- Example 2.10

ex6 :: PDA Char Char Char
ex6 =
    let a = Set ['0', '1']
        q = Set ['S', 'A', 'B', 'T']
        h = Set ['X', '0', '1']
        delta('S', 'X', Just '0') = Set [("X0", 'A')]
        delta('S', 'X', Just '1') = Set [("X1", 'A')]
        delta('A', '0', Just '0') = Set [("00", 'A'), ("", 'B')]
        delta('A', '0', Just '1') = Set [("01", 'A')]
        delta('A', '1', Just '0') = Set [("10", 'A')]
        delta('A', '1', Just '1') = Set [("11", 'A'), ("", 'B')]
        delta('B', '0', Just '0') = Set [("", 'B')]
        delta('B', '1', Just '1') = Set [("", 'B')]
        delta('B', 'X', Nothing) = Set [("X", 'T')]
        delta _ = Set []
        q0 = 'S'
        h0 = 'X'
        f = Set ['S', 'T']
    in  PDA a q h delta q0 h0 f

-- Example 2.11

ex7 :: PDA Char Char Char
ex7 =
    let a = Set ['(', ')']
        q = Set ['A', 'B']
        h = Set ['X', 'I']
        delta('A', 'X', Just '(') = Set [("XI", 'B')]
        delta('B', 'X', Nothing)  = Set [("X", 'A')]
        delta('B', 'I', Just '(') = Set [("II", 'B')]
        delta('B', 'I', Just ')') = Set [("", 'B')]
        delta _ = Set []
        q0 = 'A'
        h0 = 'X'
        f = Set ['A']
    in  PDA a q h delta q0 h0 f


-- Other examples
----------------------------------------

-- Aho, page 120
exx3 :: FNAZ Char String
exx3 =
    let a = Set ['a', 'b']
        q = Set ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
        delta ("0", Nothing) = Set ["1", "7"]
        delta ("1", Nothing) = Set ["2", "4"]
        delta ("2", Just 'a') = Set ["3"]
        delta ("3", Nothing) = Set ["6"]
        delta ("4", Just 'b') = Set ["5"]
        delta ("5", Nothing) = Set ["6"]
        delta ("6", Nothing) = Set ["1", "7"]
        delta ("7", Just 'a') = Set ["8"]
        delta ("8", Just 'b') = Set ["9"]
        delta ("9", Just 'b') = Set ["10"]
        delta _ = Set []
        q0 = "0"
        f = Set ["10"]
    in  FNAZ a q delta q0 f


-- Main program
----------------------------------------

main :: IO ()
main =
    let test number x y =
            if x == y then
               putStr ("Test " ++ show number ++ ": OK\n")
            else
               putStr ("Test " ++ show number ++ ": FAIL!!!\n")
    in  do {
            test  1 (runFDA ex1 "01001011") True;
            test  2 (runFDA ex1 "010010111") False;
            test  3 (runFDA ex1 "0100101100001") False;
            test  4 (runFNA ex2 "01001011010001") False;
            test  5 (runFNA ex2 "0") False;
            test  6 (runFNA ex2 "011") False;
            test  7 (runFNA ex2 "1") True;
            test  8 (runFNA ex2 "100010010") True;
            test  9 (runFNAZ (convFNAtoFNAZ ex2) "00010010") False;
            test 10 (runFNAZ (convFNAtoFNAZ ex2) "100010010") True;
            test 11 (runFNAZ (convFDAtoFNAZ ex1) "100010010") False;
            test 12 (runFNAZ (convFDAtoFNAZ ex1) "1000100101") True;
            test 13 (runFDA (sillyFNAZtoFDA (convFNAtoFNAZ ex2)) "011") False;
            test 14 (runFDA (sillyFNAZtoFDA (convFNAtoFNAZ ex2)) "1011") True;
            test 15 (runFDA (sillyFNAtoFDA ex2) "01001011010001") False;
            test 16 (runFDA (sillyFNAtoFDA ex2) "0") False;
            test 17 (runFDA (sillyFNAtoFDA ex2) "011") False;
            test 18 (runFDA (sillyFNAtoFDA ex2) "1") True;
            test 19 (runFDA (sillyFNAtoFDA ex2) "100010010") True;
            test 20 (runFDA (convFNAtoFDA ex2) "011") False;
            test 21 (runFDA (convFNAtoFDA ex2) "1011") True;
            test 30 (runFNAZ exx3 "abbababaabbababb") True;
            test 31 (runFNAZ exx3 "abbababaabbababba") False;
            test 32 (runFDA (convFNAZtoFDA exx3) "abbababaabbababb") True;
            test 33 (runFDA (convFNAZtoFDA exx3) "abbababaabbababba") False;
          }
