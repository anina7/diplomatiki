import Test.QuickCheck
import Control.Monad (forever)
import Control.Applicative (liftA2)

data RegExp a = REempty
              | RESymbol a
              | REConcat (RegExp a) (RegExp a)
              | REAlt (RegExp a) (RegExp a)
              | REStar (RegExp a)

correct :: RegExp a -> Gen [a]
correct r = sized $ correctSized r

correctSized :: RegExp a -> Int -> Gen [a]
correctSized REempty n =
  return []
correctSized (RESymbol x) n =
  return [x]
correctSized (REConcat r1 r2) n = do
  k <- choose (0, n)
  liftA2 (++) (correctSized r1 k) (correctSized r2 (n-k))
correctSized (REAlt r1 r2) n =
  oneof [correctSized r1 n, correctSized r2 n]
correctSized (REStar r) n = do
  k <- choose (0, n)
  concat <$> vectorOf k (correctSized r (n `div` k))

regexp :: (Arbitrary a, Eq a) => RegExp a -> Gen [a]
regexp r = sized $ regexpSized r

regexpSized :: (Arbitrary a, Eq a) => RegExp a -> Int -> Gen [a]
regexpSized r n = frequency [ (8, correctSized r n)
                            , (2, wrongSized r n)
                            ]

wrong :: (Arbitrary a, Eq a) => RegExp a -> Gen [a]
wrong r = sized $ wrongSized r

wrongSized :: (Arbitrary a, Eq a) => RegExp a -> Int -> Gen [a]
wrongSized REempty n = do
  arbitrary		--`suchThat` (/= [])
wrongSized (RESymbol x) n = do
  y <- arbitrary `suchThat` (/= x) --kai x anoikei sti lista twn pi8anwn dia8esimwn xaraktirwn
  return [y]
wrongSized (REConcat r1 r2) n = do	--kai na ginei elegxos oti o sindiasmos twn 2 de 8a dwsei swsto apotelesma
  k <- choose (0, n)
  frequency [ (4, liftA2 (++) (wrongSized r1 k) (correctSized r2 (n-k)))
            , (4, liftA2 (++) (correctSized r1 k) (wrongSized r2 (n-k)))
            , (2, liftA2 (++) (wrongSized r1 k) (wrongSized r2 (n-k)))
            ]
wrongSized (REAlt r1 r2) n =
  oneof [wrongSized r1 n, wrongSized r2 n]
wrongSized (REStar r) n = do	--kai na ginei elegxos oti o sindiasmos twn xaraktirwn de 8a dwsei swsto apotelesma
  k <- choose (0, n)
  let k' = max 1 k
  let nk = n `div` k'
  c <- choose (0, k'-1)
  cs <- vectorOf c (correctSized r nk)
  w <- wrongSized r nk
  ss <- vectorOf (k'-c-1) (regexpSized r nk)
  return $ concat $ cs ++ w : ss

newtype Digit = Digit { unDigit :: Char }
  deriving Eq

instance Show Digit where
  showsPrec p (Digit d) = ([d] ++)

instance Arbitrary Digit where
  arbitrary = Digit <$> elements "012"

instance Show a => Show (RegExp a) where
  showsPrec p REempty = ("ε" ++)
  showsPrec p (RESymbol x) = showsPrec p x
  showsPrec p (REConcat r1 r2) = showParen (p > 1) $
    showsPrec 2 r1 . showsPrec 1 r2
  showsPrec p (REAlt r1 r2) = showParen (p > 0) $
    showsPrec 1 r1 . ("|" ++) . showsPrec 0 r2
  showsPrec p (REStar r) = showParen (p > 2) $
    showsPrec 2 r . ("*" ++)

test r = do
  putStrLn $ "Testing: " ++ show r
  putStrLn "Some correct inputs:"
  mapM_ (putStrLn . ("  " ++)) =<< (sample' $ (fmap $ map unDigit) $ correct r)
  putStrLn "Some wrong inputs:"
  mapM_ (putStrLn . ("  " ++)) =<< (sample' $ (fmap $ map unDigit) $ wrong r)

instance Num Digit where
  fromInteger = Digit . head . show
  
instance Num a => Num (RegExp a) where
  fromInteger = RESymbol . fromInteger

infixr 5 .|.
infixr 6 .@.

(.@.) :: RegExp a -> RegExp a -> RegExp a
r1 .@. r2 = REConcat r1 r2

(.|.) :: RegExp a -> RegExp a -> RegExp a
r1 .|. r2 = REAlt r1 r2

s :: RegExp a -> RegExp a
s = REStar

main = do
  test $ 0 .@. s 1
  test $ s (0 .|. 1) .@. 2 .@. s (0 .|. 1)
