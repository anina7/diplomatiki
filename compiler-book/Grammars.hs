----------------------------------------
-- Context-free grammars
----------------------------------------

module Grammars where

import Auxiliary


-- Definition
----------------------------------------

data CFG term nonterm =
    CFG (Set term)
        (Set nonterm)
        (Set (nonterm, [Either term nonterm]))
        nonterm

data LL1 term nonterm =
    LL1 (CFG term nonterm)
        (nonterm -> Maybe term -> Maybe [Either term nonterm])

type Item term nonterm = (nonterm, [Maybe (Either term nonterm)])
type State term nonterm = (Int, Set (Item term nonterm))

data Action term nonterm =
    A_shift
  | A_reduce (nonterm, [Either term nonterm])
  | A_accept
  deriving Eq

data SLR1 term nonterm =
    SLR1 (CFG term nonterm) nonterm
         (State term nonterm -> Maybe term ->
              Maybe (Action term nonterm))
         (State term nonterm -> Either term nonterm ->
              Maybe (State term nonterm))


-- Pretty printing
----------------------------------------

showRule :: (Eq term, Eq nonterm, Show term, Show nonterm) =>
            (nonterm, [Either term nonterm]) -> ShowS
showRule (left, right) =
    let showRight [] = id
        showRight (Left term : alpha) =
            showsPrec 0 term . (" " ++) . showRight alpha
        showRight (Right nonterm : alpha) =
            showsPrec 0 nonterm . (" " ++) . showRight alpha
    in  showsPrec 0 left . (" ::= " ++) . showRight right

instance (Eq term, Eq nonterm, Show term, Show nonterm) =>
         Show (CFG term nonterm) where
    showsPrec p (CFG (Set termsl) (Set nontermsl) (Set rulesl) start) =
        let showRules [] = id
            showRules (r : rl) = showRule r . ("\n" ++) . showRules rl
        in  showRules rulesl

instance (Eq term, Eq nonterm, Show term, Show nonterm) =>
         Show (LL1 term nonterm) where
    showsPrec p (LL1 (CFG (Set termsl) (Set nontermsl) (Set rulesl) start) m) =
        let mtermsl = map Just termsl ++ [Nothing]
            showLines [] = id
            showLines (n : nl) = showsPrec 0 n . ("\t" ++) .
                                 showLine n mtermsl . showLines nl
            showLine n [] = ("\n" ++)
            showLine n (mt : mtl) =
                case m n mt of
                    Just alpha -> showRule (n, alpha) . ("\t" ++) .
                                  showLine n mtl
                    Nothing    -> ("@\t" ++) . showLine n mtl
            showTerms [] = ("\n" ++)
            showTerms (Just t : mtl) =
                showsPrec 0 t . ("\t" ++) . showTerms mtl
            showTerms (Nothing : mtl) = ("EOF" ++) . showTerms mtl
        in  ("\t" ++) . showTerms mtermsl . showLines nontermsl

showItems :: (Eq term, Eq nonterm, Show term, Show nonterm) =>
             Set (Item term nonterm) -> ShowS
showItems (Set itemsl) =
    let showRight [] = id
        showRight (Nothing : alpha) =
            ("@ " ++) . showRight alpha
        showRight (Just (Left term) : alpha) =
            showsPrec 0 term . (" " ++) . showRight alpha
        showRight (Just (Right nonterm) : alpha) =
            showsPrec 0 nonterm . (" " ++) . showRight alpha
        loop [] = id
        loop ((nonterm, mntl) : itemsl) =
            showsPrec 0 nonterm . (" ::= " ++) . showRight mntl . ("\n" ++) .
            loop itemsl
    in  loop itemsl

showStates :: (Eq term, Eq nonterm, Show term, Show nonterm) =>
              Set (State term nonterm) -> ShowS
showStates (Set l) =
    let loop [] = id
        loop ((i, ii) : l) =
           ("I" ++) . showsPrec 0 i . (" =\n" ++) . showItems ii . loop l
    in  loop l

instance (Eq term, Eq nonterm, Show term, Show nonterm) =>
         Show (Action term nonterm) where
    showsPrec p A_shift = ("s" ++)
    showsPrec p (A_reduce r) = ("r(" ++) . showRule r . (")" ++)
    showsPrec p A_accept = ("acc" ++)

instance (Eq term, Eq nonterm, Show term, Show nonterm) =>
         Show (SLR1 term nonterm) where
    showsPrec p (SLR1 g@(CFG (Set termsl) (Set nontermsl) (Set rulesl) start)
                      start' action next) =
        let rulesl' = (start', [Right start]) : rulesl
            mtermsl = map Just termsl ++ [Nothing]
            k@(Set kl) = calcStates g start'
            showLines [] = id
            showLines (s@(i, _) : sl) =
                showsPrec 0 i . ("\t" ++) .
                showLineT s mtermsl . showLineN s nontermsl .
                showLines sl
            showLineT s [] = id
            showLineT s (mt : mtl) =
                case action s mt of
                    Just A_shift ->
                        ("s" ++) .
                        (case mt of
                             Just t ->
                                 case next s (Left t) of
                                     Just (j, _) -> showsPrec 0 j
                                     Nothing     -> error "missing state!"
                             Nothing ->
                                 error "it is illegal to shift EOF!") .
                        ("\t" ++) . showLineT s mtl
                    Just (A_reduce r) ->
                        let loop [] n =
                                error ("rule " ++ show r ++ "not found!")
                            loop (r' : rl) n =
                                if r == r' then n else loop rl (n+1)
                        in  ("r" ++) . showsPrec 0 (loop rulesl' 0) .
                            ("\t" ++) . showLineT s mtl
                    Just A_accept ->
                        ("acc\t" ++) . showLineT s mtl
                    Nothing ->
                        ("@\t" ++) . showLineT s mtl
            showLineN s [] = ("\n" ++)
            showLineN s (n : nl) =
                case next s (Right n) of
                    Just (j, _) -> ("s" ++) . showsPrec 0 j . ("\t" ++) .
                                   showLineN s nl
                    Nothing     -> ("@\t" ++) . showLineN s nl
            showTerms [] = ("\t" ++)
            showTerms (Just t : mtl) =
                showsPrec 0 t . ("\t" ++) . showTerms mtl
            showTerms (Nothing : mtl) = ("EOF" ++) . showTerms mtl
            showNonTerms [] = ("\n" ++)
            showNonTerms (n : nl) =
                showsPrec 0 n . ("\t" ++) . showNonTerms nl
        in  ("\t" ++) . showTerms mtermsl . showNonTerms nontermsl .
            showLines kl



----------------------------------------
-- Algorithms
----------------------------------------


-- FIRST
----------------------------------------

calcFirst :: (Eq term, Eq nonterm) =>
             CFG term nonterm -> (nonterm -> Set (Maybe term),
                                  [Either term nonterm] -> Set (Maybe term))
calcFirst (CFG (Set termsl) (Set nontermsl) (Set rulesl) start) =
    let suffixL [] = []
        suffixL ((nonterm, alpha) : rl) = suffix alpha ++ suffixL rl
        suffix [] = [[]]
        suffix (x : alpha) = alpha : suffix alpha
        tff0 t = Set []
        sff0 [] = Set [Nothing]
        sff0 (Left t : alpha) = Set [Just t]
        sff0 (Right n : alpha) = Set []
        loop1 (tff, sff) =
            let loop2 [] (tff, sff, chflg) = (tff, sff, chflg)
                loop2 ((nonterm, alpha) : rl) (tff, sff, chflg) =
                    let fn   = tff nonterm
                        fa   = sff alpha
                        fn'  = fn `setUnion` fa
                        tff' = update nonterm fn' tff
                    in  if fn' /= fn then
                            loop2 rl (tff', sff, True)
                        else
                            loop2 rl (tff, sff, chflg)
                loop3 [] (tff, sff, chflg) = (tff, sff, chflg)
                loop3 (alpha@(Right n : beta) : sfl) (tff, sff, chflg) =
                    let fa  = sff alpha
                        fn  = tff n
                        fa' = fa `setUnion` setFilter (\mt -> mt /= Nothing) fn
                    in  if Nothing `setElem` fn then
                            let fa'' = fa' `setUnion` sff beta
                                sff' = update alpha fa'' sff
                            in  if fa'' /= fa then
                                    loop3 sfl (tff, sff', True)
                                else
                                    loop3 sfl (tff, sff, chflg)
                        else
                            if fa' /= fa then
                                loop3 sfl (tff, update alpha fa' sff, True)
                            else
                                loop3 sfl (tff, sff, chflg)
                loop3 (alpha : sfl) (tff, sff, chflg) =
                    loop3 sfl (tff, sff, chflg)
                (tff', sff', chflg) =
                    loop3 (suffixL rulesl) (loop2 rulesl (tff, sff, False))
            in  if chflg then
                    loop1 (tff', sff')
                else
                    (tff, sff)
        (tff, sff) = loop1 (tff0, sff0)
    in  (tff, sff)


-- FOLLOW
----------------------------------------

calcFollow :: (Eq term, Eq nonterm) =>
              CFG term nonterm -> nonterm -> Set (Maybe term)
calcFollow g@(CFG (Set termsl) (Set nontermsl) (Set rulesl) start) =
    let (tff, sff) = calcFirst g
        nff0 n = if n == start then Set [Nothing] else Set []
        loop1 nff =
            let loop2 [] (nff, chflg) = (nff, chflg)
                loop2 ((na, alpha) : rl) (nff, chflg) =
                    let loop3 [] (nff, chflg) = (nff, chflg)
                        loop3 (Right nb : beta) (nff, chflg) =
                            let nfb = nff nb
                                tfb = sff beta
                                nfb' = nfb `setUnion`
                                          setFilter (\mt -> mt /= Nothing) tfb
                            in  if Nothing `setElem` tfb then
                                    let nfb'' = nfb' `setUnion` nff na
                                        nff' = update nb nfb'' nff
                                    in  if nfb'' /= nfb then
                                            loop3 beta (nff', True)
                                        else
                                            loop3 beta (nff, chflg)
                                else
                                    if nfb' /= nfb then
                                        loop3 beta (update nb nfb' nff, True)
                                    else
                                        loop3 beta (nff, chflg)
                        loop3 (a : beta) (nff, chflg) =
                            loop3 beta (nff, chflg)
                    in  loop2 rl (loop3 alpha (nff, chflg))
                (nff', chflg) = loop2 rulesl (nff, False)
            in  if chflg then
                    loop1 nff'
                else
                    nff
    in  loop1 nff0


-- LL(1) table
----------------------------------------

calcLL1 :: (Eq term, Eq nonterm, Show term, Show nonterm) =>
           CFG term nonterm -> LL1 term nonterm
calcLL1 g@(CFG (Set termsl) (Set nontermsl) (Set rulesl) start) =
    let m0 n mt = Nothing
        (tff, sff) = calcFirst g
        nff = calcFollow g
        loop1 [] m = m
        loop1 (r@(na, alpha) : rl) m =
            let checkUpdate mt r mna =
                    case mna mt of
                        Just r' -> error ("The grammar is not LL(1)\n" ++
                                          "m(" ++ show na ++ ", " ++
                                          (case mt of
                                               Just a -> show a
                                               Nothing -> "EOF") ++
                                          ") should contain:\n" ++
                                          showRule (na, r') "" ++ "\n" ++
                                          showRule (na, r) "")
                        Nothing -> update mt (Just r) mna
                Set sfal = sff alpha
                Set nfal = nff na
                loop2 [] mna = mna
                loop2 (Just ta : mtl) mna =
                    let mna' = checkUpdate (Just ta) alpha mna
                    in  loop2 mtl mna'
                loop2 (mt : mtl) mna = loop2 mtl mna
                mna = loop2 sfal (m na)
            in  if Nothing `elem` sfal then
                    let mna' = loop2 nfal mna
                        mna'' = checkUpdate Nothing alpha mna'
                    in  if Nothing `elem` nfal then
                            loop1 rl (update na mna'' m)
                        else
                            loop1 rl (update na mna' m)
                else
                    loop1 rl (update na mna m)
        m = loop1 rulesl m0
    in  LL1 g m


runLL1 :: (Eq term, Eq nonterm, Show term, Show nonterm) =>
           LL1 term nonterm -> [term] -> IO Bool
runLL1 (LL1 (CFG (Set termsl) (Set nontermsl) (Set rulesl) start) m) tl =
    let move stl mtl n =
            putStr (show n ++ "\t") >>
            printStack stl >>
            putStr "\t" >>
            printAlpha mtl >>
            putStr "\t" >>
            next stl mtl (n+1)
        next [] [Nothing] n =
            putStr "success\n" >>
            return True
        next [] _  n =
            putStr "failure\n" >>
            return False
        next (Left t : stl) (mt : mtl) n =
            if mt == Just t then
                putStr ("consume " ++ show t ++ "\n") >>
                move stl mtl n
            else
                putStr "failure\n" >>
                return False
        next (Right nt : stl) mtl@(mt : mtl') n =
            case m nt mt of
                Just alpha ->
                    putStr ("rule " ++ showRule (nt, alpha) "" ++ "\n") >>
                    move (push alpha stl) mtl n
                Nothing ->
                    putStr "failure\n" >>
                    return False
        next _ _ n =
            putStr "failure\n" >>
            return False
        push [] stl = stl
        push (a : alpha) stl = a : push alpha stl
        printStack [] =
            return ()
        printStack (Left t : stl) =
            printStack stl >>
            putStr (show t ++ " ")
        printStack (Right n : stl) =
            printStack stl >>
            putStr (show n ++ " ")
        printAlpha [] =
            return ()
        printAlpha (Just t : mtl) =
            putStr (show t ++ " ") >>
            printAlpha mtl
        printAlpha (Nothing : mtl) =
            putStr "EOF\t" >>
            printAlpha mtl
    in  move [Right start] (map Just tl ++ [Nothing]) 0


-- Miscellaneous functions on items
----------------------------------------

nextSymbol :: (Eq term, Eq nonterm) =>
              Item term nonterm ->
              Maybe (Either term nonterm)
nextSymbol (nt, alpha) =
    let loop (Just nt : mntl) = loop mntl
        loop (Nothing : Just nt : mntl) = Just nt
        loop _ = Nothing
    in  loop alpha

forward :: (Eq term, Eq nonterm) => Item term nonterm -> Item term nonterm
forward (nt, alpha) =
    let loop (Nothing : mnt : mntl) = mnt : Nothing : mntl
        loop (Just nt : mntl) = Just nt : loop mntl
        loop mntl = mntl
    in  (nt, loop alpha)


-- CLOSURE
----------------------------------------

calcClosure :: (Eq term, Eq nonterm) =>
               CFG term nonterm ->
               Set (Item term nonterm) ->
               Set (Item term nonterm)
calcClosure (CFG (Set termsl) (Set nontermsl) (Set rulesl) start)
            (Set itemsl) =
    let loop1 jl =
            let loop2 [] (jl, chflg) = (jl, chflg)
                loop2 (item : itemsl) (jl, chflg) =
                    let loop3 [] nb (jl, chflg) = (jl, chflg)
                        loop3 ((nonterm, alpha) : rl) nb (jl, chflg) =
                            if nonterm == nb then
                                let item' = (nonterm, Nothing : map Just alpha)
                                in  if not (item' `elem` jl) then
                                        loop3 rl nb (jl ++ [item'], True)
                                    else
                                        loop3 rl nb (jl, chflg)
                            else
                                loop3 rl nb (jl, chflg)
                    in  case nextSymbol item of
                            Just (Right nb) ->
                                loop2 itemsl (loop3 rulesl nb (jl, chflg))
                            otherwise ->
                                loop2 itemsl (jl, chflg)
                (jl', chflg) = loop2 jl (jl, False)
            in  if chflg then
                    loop1 jl'
                else
                    jl
    in  Set (loop1 itemsl)


-- GOTO
----------------------------------------

calcGoto :: (Eq term, Eq nonterm) =>
            CFG term nonterm ->
            Set (Item term nonterm) ->
            Either term nonterm ->
            Set (Item term nonterm)
calcGoto g items nt =
    let check item =
            case nextSymbol item of
                Just nt' -> nt == nt'
                Nothing  -> False
        j = setMap forward (setFilter check items)
    in  calcClosure g j


-- Set of states
----------------------------------------

calcStates :: (Eq term, Eq nonterm) =>
              CFG term nonterm -> nonterm ->
              Set (State term nonterm)
calcStates g@(CFG (Set termsl) (Set nontermsl) (Set rulesl) start) start' =
    let i0 = calcClosure g (Set [(start', [Nothing, Just (Right start)])])
        k0 = [(0, i0)]
        symbs = map Left termsl ++ map Right nontermsl
        loop i k =
            let loop2 [] (i, k, chflg) = (i, k, chflg)
                loop2 ((j, ij) : k') (i, k, chflg) =
                    let loop3 [] (i, k, chflg) = (i, k, chflg)
                        loop3 (nt : ntl) (i, k, chflg) =
                            let inew = calcGoto g ij nt
                            in  if not (setEmpty inew) &&
                                   not (inew `elem` map snd k) then
                                    loop3 ntl (i+1, k ++ [(i+1, inew)], True)
                                else
                                    loop3 ntl (i, k, chflg)
                    in  loop2 k' (loop3 symbs (i, k, chflg))
                (i', k', chflg) = loop2 k (i, k, False)
            in  if chflg then
                    loop i' k'
                else
                    k
    in  Set (loop 0 k0)


-- SLR(1) table
----------------------------------------

augment :: (Eq term, Eq nonterm) =>
           CFG term nonterm -> nonterm -> CFG term nonterm
augment (CFG terms (Set nontermsl) (Set rulesl) start) start' =
    let nontermsl' = start' : nontermsl
        rulesl' = (start', [Right start]) : rulesl
    in  CFG terms (Set nontermsl') (Set rulesl') start'

calcSLR1 :: (Eq term, Eq nonterm, Show term, Show nonterm) =>
            CFG term nonterm -> nonterm -> SLR1 term nonterm
calcSLR1 g@(CFG (Set termsl) (Set nontermsl) (Set rulesl) start) start' =
    let g' = augment g start'
        k@(Set kl) = calcStates g start'
        nff = calcFollow g'
        symbs = map Left termsl ++ map Right nontermsl
        action0 s t = Nothing
        next0 s nt = Nothing
        checkUpdate str f s@(i, _) x y =
            case f s x of
                Just z ->
                    error ("The grammar is not SLR(1)\n" ++
                           str ++ "(I" ++ show i ++ ", " ++ show x ++
                                  ") should contain:\n" ++
                                  show z ++ "\n" ++ show y)
                Nothing -> \s' -> \x' ->
                    if s' == s && x' == x then Just y else f s' x'
        findState items =
            let loop [] = error ("State was not found:\n" ++
                                 showItems items "")
                loop (s@(i, items') : sl) =
                    if items == items' then s else loop sl
            in  case k of
                    Set l -> loop l
        makeAlpha [] = []
        makeAlpha (Just nt : mntl) = nt : makeAlpha mntl
        makeAlpha (Nothing : mntl) = makeAlpha mntl
        loop1 [] (action, next) = (action, next)
        loop1 (s@(i, items@(Set itemsl)) : kl') (action, next) =
            let loop2 [] action = action
                loop2 (item@(na, mntl) : itemsl) action =
                    case nextSymbol item of
                        Just (Left ta) ->
                            let action' = checkUpdate "ACTION" action
                                              s (Just ta) A_shift
                            in loop2 itemsl action'
                        Nothing ->
                            let loop3 [] action = action
                                loop3 (mt : mtl) action =
                                    let r = (na, makeAlpha mntl)
                                    in  if na == start' then
                                            loop3 mtl (checkUpdate "ACTION"
                                                       action
                                                       s mt A_accept)
                                        else
                                            loop3 mtl (checkUpdate "ACTION"
                                                       action
                                                       s mt (A_reduce r))
                                action' = case nff na of
                                              Set l -> loop3 l action
                            in  loop2 itemsl action'
                        otherwise ->
                            loop2 itemsl action
                action' = loop2 itemsl action
                loop4 [] next = next
                loop4 (s'@(j, Set itemsl) : kl') next =
                    let loop5 [] next = next
                        loop5 (nt : ntl) next =
                            let items' = calcGoto g' items nt
                                next' = checkUpdate "NEXT" next s nt s'
                            in  if not (setEmpty items') &&
                                   s' == findState items' then
                                    loop5 ntl next'
                                else
                                    loop5 ntl next
                        next' = loop5 symbs next
                    in  loop4 kl' next'
                next' = loop4 kl next
            in  loop1 kl' (action', next')
        (action, next) = loop1 kl (action0, next0)
    in  SLR1 g start' action next

runSLR1 :: (Eq term, Eq nonterm, Show term, Show nonterm) =>
           SLR1 term nonterm -> [term] -> IO Bool
runSLR1 (SLR1 g@(CFG (Set termsl) (Set nontermsl) (Set rulesl) start)
              start' action next) tl =
    let k@(Set kl) = calcStates g start'
        s0 = head kl
        move stl mtl n =
            putStr (show n ++ "\t") >>
            printStack stl >>
            putStr "\t" >>
            printAlpha mtl >>
            putStr "\t" >>
            moveNext stl mtl (n+1)
        moveNext stl@(Left s : stl') mtl@(mt : mtl') n =
            case action s mt of
                Just A_shift ->
                    case mt of
                        Just a ->
                            case next s (Left a) of
                                Just s'@(i, _) ->
                                    putStr ("s" ++ show i ++ "\n") >>
                                    move (Left s' : Right (Left a) : stl)
                                         mtl' n
                                Nothing ->
                                    error ("this should never occur: " ++
                                           "empty next after a shift!")
                        Nothing ->
                            error "this should never occur: next s EOF !"
                Just (A_reduce r@(na, alpha)) ->
                    let count = length alpha
                        nstl@(Left s' : nstl') = drop (2*count) stl
                    in  case next s' (Right na) of
                            Just s'' ->
                                putStr ("r with " ++ showRule r "" ++ "\n") >>
                                move (Left s'' : Right (Right na) : nstl) mtl n
                            Nothing ->
                                error ("this should never occur: " ++
                                       "empty next after a reduce!")
                Just A_accept ->
                    putStr "accept\n" >>
                    return True
                Nothing ->
                    putStr "failure\n" >>
                    return False
        moveNext _ _ n =
            putStr "failure\n" >>
            return False
        printStack [] =
            return ()
        printStack (Left (i, _) : stl) =
            printStack stl >>
            putStr (show i ++ " ")
        printStack (Right (Left t) : stl) =
            printStack stl >>
            putStr (show t ++ " ")
        printStack (Right (Right n) : stl) =
            printStack stl >>
            putStr (show n ++ " ")
        printAlpha [] =
            return ()
        printAlpha (Just t : mtl) =
            putStr (show t ++ " ") >>
            printAlpha mtl
        printAlpha (Nothing : mtl) =
            putStr "EOF\t" >>
            printAlpha mtl
    in  move [Left s0] (map Just tl ++ [Nothing]) 0


----------------------------------------
-- Examples from our book
----------------------------------------

data Symbol = SymT String | SymN String
  deriving Eq

instance Show Symbol where
    showsPrec p (SymT s) = (s ++)
--  showsPrec p (SymT s) = showsPrec p s
    showsPrec p (SymN s) = (s ++)

fixRules [] = []
fixRules ((na, alpha) : rl) =
    let fixAlpha [] = []
        fixAlpha (Left t : alpha) = Left (SymT t) : fixAlpha alpha
        fixAlpha (Right n : alpha) = Right (SymN n) : fixAlpha alpha
    in  (SymN na, fixAlpha alpha) : fixRules rl

printMany msgf f [] =
    putStr "\n"
printMany msgf f (n : nl) =
    putStr (msgf n ++ " = ") >>
    print (f n) >>
    printMany msgf f nl

-- Example 4.1

g1 :: CFG Symbol Symbol
g1 =
    let terms = map SymT ["id", "+", "*", "(", ")"]
        nonterms = map SymN ["E", "E'", "T", "T'", "F"]
        rules = fixRules [
            ("E", [Right "T", Right "E'"]),
            ("E'", []),
            ("E'", [Left "+", Right "T", Right "E'"]),
            ("T", [Right "F", Right "T'"]),
            ("T'", []),
            ("T'", [Left "*", Right "F", Right "T'"]),
            ("F", [Left "(", Right "E", Left ")"]),
            ("F", [Left "id"])
         ]
        start = SymN "E"
    in  CFG (Set terms) (Set nonterms) (Set rules) start

ex_4_1 =
    let (tff, sff) = calcFirst g1
        nff = calcFollow g1
        nonterms = map SymN ["E", "E'", "T", "T'", "F"]
    in  print g1 >>
        printMany (\n -> "FIRST(" ++ show n ++ ")") tff nonterms >>
        printMany (\n -> "FOLLOW(" ++ show n ++ ")") nff nonterms

-- Example 4.5

ex_4_5 =
    print (calcLL1 g1)

-- Example 4.6

ex_4_6 =
    let ll1 = calcLL1 g1
        terms = map SymT ["id", "+", "id", "*", "id"]
    in  runLL1 ll1 terms

-- Example 4.7

g3 :: CFG Symbol Symbol
g3 =
    let terms = map SymT ["r", ",", "a", "b"]
        nonterms = map SymN ["S", "B", "D"]
        rules = fixRules [
            ("S", [Left "r", Right "B"]),
            ("B", [Right "D"]),
            ("B", [Right "B", Left ",", Right "D"]),
            ("D", [Left "a"]),
            ("D", [Left "b"])
         ]
        start = SymN "S"
    in  CFG (Set terms) (Set nonterms) (Set rules) start

ex_4_7 =
    print g3

-- Example 4.9

items_4_9 = Set [ (SymN "S", [
                      Just (Left (SymT "r")),
                      Nothing,
                      Just (Right (SymN "B"))
                   ]) ]

ex_4_9 =
    let j1 = calcClosure g3 items_4_9
    in  putStr (showItems j1 "")

-- Example 4.10

items_4_10a = Set [ (SymN "B", [
                        Nothing,
                        Just (Right (SymN "B")),
                        Just (Left (SymT ",")),
                        Just (Right (SymN "D"))
                     ]),
                    (SymN "B", [
                        Nothing,
                        Just (Right (SymN "D"))
                     ]) ]

items_4_10b = Set [ (SymN "S", [
                        Nothing,
                        Just (Left (SymT "r")),
                        Just (Right (SymN "B"))
                     ]) ]

ex_4_10 =
    let j1 = calcGoto g3 items_4_10a (Right (SymN "B"))
        j2 = calcGoto g3 items_4_10b (Left (SymT "r"))
    in  putStr (showItems j1 "") >>
        putStr "\n" >>
        putStr (showItems j2 "")

-- Example 4.11

g3' = augment g3 (SymN "S'")

ex_4_11 =
    let k = calcStates g3 (SymN "S'")
    in  print g3' >>
        putStr (showStates k "")

-- Example 4.12

ex_4_12 =
    let slr1 = calcSLR1 g3 (SymN "S'")
        terms = map SymT ["r", "a", ",", "b"]
    in  print slr1 >>
        putStr "\n" >>
        runSLR1 slr1 terms

-- Example 4.13

g4 :: CFG Symbol Symbol
g4 =
    let terms = map SymT ["id", "+", "*", "(", ")"]
        nonterms = map SymN ["E", "T", "F"]
        rules = fixRules [
            ("E", [Right "T"]),
            ("E", [Right "E", Left "+", Right "T"]),
            ("T", [Right "F"]),
            ("T", [Right "T", Left "*", Right "F"]),
            ("F", [Left "(", Right "E", Left ")"]),
            ("F", [Left "id"])
         ]
        start = SymN "E"
    in  CFG (Set terms) (Set nonterms) (Set rules) start

g4' = augment g4 (SymN "S")

ex_4_13 =
    let k = calcStates g4 (SymN "S")
        slr1 = calcSLR1 g4 (SymN "S")
        terms = map SymT ["id", "*", "id", "+", "id"]
    in  print g4' >>
        putStr (showStates k "\n") >>
        print slr1 >>
        putStr "\n" >>
        runSLR1 slr1 terms

main :: IO ()
main = do test "ex_4_1" ex_4_1
          test "ex_4_5" ex_4_5
          test "ex_4_6" ex_4_6
          test "ex_4_7" ex_4_7
          test "ex_4_9" ex_4_9
          test "ex_4_10" ex_4_10
          test "ex_4_11" ex_4_11
          test "ex_4_12" ex_4_12
          test "ex_4_13" ex_4_13
  where test number t = do
          putStrLn $ replicate 72 '-'
          putStrLn $ "Test " ++ number
          result <- t
          putStrLn $ "Result: " ++ show result ++ "\n"
