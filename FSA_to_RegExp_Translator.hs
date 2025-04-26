{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isAlphaNum)
import Data.List (intercalate, sort)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Data type for automaton
data FSA = FSA
  { fsaType      :: FSAType               -- deterministic or not
  , states       :: [String]              -- list of states
  , alphabet     :: [String]              -- list of symbols
  , initialState :: String                -- start state
  , accepting    :: [String]              -- accepting states
  , transitions  :: [(String,String,String)]  -- list of (from, symbol, to)
  }

data FSAType = Deterministic | NonDeterministic deriving (Eq)

-- Entry point: read lines, process, then print regex or error
main :: IO ()
main = do
  ls <- fmap lines getContents          -- read all input lines
  case processLines ls of
    Left err -> putStrLn err            -- print first error found
    Right f  -> putStrLn (convertToRegExp f)  -- or print resulting regex

-- helper errors with detailed messages
errState :: String -> Either String a
errState s = Left $ "A state '" ++ s ++ "' is not in the set of states."

errSymbol :: String -> Either String a
errSymbol a = Left $ "A transition symbol '" ++ a ++ "' is not in the alphabet."

-- Parse and validate all six lines
processLines :: [String] -> Either String FSA
processLines [tL,sL,aL,iL,acL,trL] = do
  -- 1. parse type line
  ftype <- case stripPrefix "type=[" tL >>= stripSuffix "]" of
    Just "deterministic"     -> Right Deterministic
    Just "non-deterministic" -> Right NonDeterministic
    _                        -> Left "Input is malformed."

  -- 2. parse states, require at least one
  stsRaw <- parseIdListLine "states" sL
  when (null stsRaw) (Left "Input is malformed.")
  let sts = nubLike stsRaw         -- remove duplicates

  -- 3. parse alphabet, require at least one
  alph <- parseIdListLine "alphabet" aL
  when (null alph) (Left "Input is malformed.")
  whenDetEps ftype alph            -- eps only in non-det
  when (ftype == Deterministic && "eps" `elem` alph) (Left "FSA is non-deterministic.")

  -- 4. parse initial state, must be in states
  initS <- parseSingleIdLine "initial" iL
  when (initS `notElem` sts) (errState initS)

  -- 5. parse accepting states, dedup and sort lexicographically
  accRaw <- parseIdListLine "accepting" acL
  when (null accRaw) (Left "Set of accepting states is empty.")
  mapM_ (\q -> when (q `notElem` sts) (errState q)) accRaw
  let accs = sort (nubLike accRaw)

  -- 6. parse transitions (strict syntax, no empty parts)
  rawTrs <- parseTransitionsStrict trL
  when (length rawTrs /= length (nubLike rawTrs)) (Left "Input is malformed.")
  trs <- mapM (validateTrans sts alph) rawTrs

  -- extra checks: determinism & reachability
  when (ftype == Deterministic && nondet trs) (Left "FSA is non-deterministic.")
  when (disjoint (FSA ftype sts alph initS accs trs)) (Left "Some states are disjoint.")

  -- if all ok, return FSA structure
  pure (FSA ftype sts alph initS accs trs)

processLines _ = Left "Input is malformed."

-- Check identifier characters
validId :: String -> Bool
validId s = not (null s) && all (\c -> isAlphaNum c || c=='_') s

-- Parse lines like "key=[x,y,...]" into list, ignore empty parts
parseIdListLine :: String -> String -> Either String [String]
parseIdListLine key l =
  case stripPrefix (key ++ "=[") l >>= stripSuffix "]" of
    Just body ->
      let lst = filter (/="") (splitOn ',' body)
      in if all validId lst then Right lst else Left "Input is malformed."
    _ -> Left "Input is malformed."

-- Parse single-value lines, check non-empty
parseSingleIdLine :: String -> String -> Either String String
parseSingleIdLine key l =
  case stripPrefix (key ++ "=[") l >>= stripSuffix "]" of
    Just ""  -> Left "Initial state is not defined."
    Just s | validId s -> Right s
           | otherwise -> Left "Input is malformed."
    _ -> Left "Input is malformed."

-- Strict parsing of transitions, reject empty items or wrong format
parseTransitionsStrict :: String -> Either String [(String,String,String)]
parseTransitionsStrict l =
  case stripPrefix "transitions=[" l >>= stripSuffix "]" of
    Nothing -> Left "Input is malformed."
    Just body ->
      let items = splitOn ',' body
      in if any null items then Left "Input is malformed."
         else traverse toTriple items
  where
    toTriple str =
      case splitOn '>' str of
        [u,a,v] | all validId [u,a,v] -> Right (u,a,v)
        _ -> Left "Input is malformed."

-- Check each transition against known states and alphabet
validateTrans :: [String] -> [String] -> (String,String,String)
              -> Either String (String,String,String)
validateTrans sts alph (u,a,v)
  | u `notElem` sts  = errState u
  | a `notElem` alph = errSymbol a
  | v `notElem` sts  = errState v
  | otherwise        = Right (u,a,v)

-- Strip prefix if matches
stripPrefix p s = if take (length p) s == p then Just (drop (length p) s) else Nothing
stripSuffix suf str = fmap reverse (stripPrefix (reverse suf) (reverse str))

-- Split on delimiter, keep empty pieces
splitOn _ "" = [""]
splitOn d s  = let (l,r) = break (==d) s
               in l : case r of [] -> [] ; (_:xs) -> splitOn d xs

-- Simple when for Either
when :: Bool -> Either String () -> Either String ()
when True  e = e
when False _ = Right ()

-- Epsilon only in non-deterministic
whenDetEps Deterministic alph | "eps" `elem` alph = Left "FSA is non-deterministic."
                              | otherwise         = Right ()
whenDetEps _ _ = Right ()

-- remove duplicates but preserve first occurrences
nubLike :: Eq a => [a] -> [a]
nubLike = foldr (\x acc -> if x `elem` acc then acc else x:acc) []

-- detect non-determinism
nondet :: [(String,String,String)] -> Bool
nondet trs = any (>1) $ Map.elems $
               Map.fromListWith (+) [((u,a),1::Int) | (u,a,_) <- trs]

-- check that every state is reachable from initial
disjoint :: FSA -> Bool
disjoint (FSA _ qs _ initS _ trs) = any (`Set.notMember` reach) qs
  where
    adj   = Map.fromListWith (++) [(u,[v]) | (u,_,v) <- trs]
    reach = dfs Set.empty [initS]
    dfs vis []     = vis
    dfs vis (x:xs)
      | x `Set.member` vis = dfs vis xs
      | otherwise          = let nxt = Map.findWithDefault [] x adj
                             in dfs (Set.insert x vis) (xs ++ nxt)

-- Build final regex with Kleene's algorithm
convertToRegExp :: FSA -> String
convertToRegExp (FSA _ qs _ initS acc trs) =
  case finals of
    []  -> "{}"                  -- empty language
    [x] -> x                     -- single final
    xs  -> intercalate "|" xs   -- multiple finals
  where
    n     = length qs
    idx   = Map.fromList $ zip qs [0..]
    r0    = buildR0 qs trs
    rn    = foldl (\m k -> buildR k m) r0 [0..n-1]
    finals = [ rn !! (idx Map.! initS) !! (idx Map.! a) | a <- acc ]

-- initial R⁰ matrix
buildR0 :: [String] -> [(String,String,String)] -> [[String]]
buildR0 qs trs = [[ cell u v | v <- qs ] | u <- qs ]
  where
    cell u v =
      let syms = [s | (x,s,y) <- trs, x==u, y==v]
          base = syms ++ ["eps" | u==v]
          (noEps,epsPart) = span (/= "eps") base
          rePart = case base of
                     [] -> "{}"
                     _  -> intercalate "|" (noEps ++ ["eps" | not (null epsPart)])
      in paren rePart      -- wrap each cell in parentheses

-- build Rᵏ from Rᵏ⁻¹
buildR :: Int -> [[String]] -> [[String]]
buildR k prev = [[ entry i j | j <- [0..n-1] ] | i <- [0..n-1] ]
  where
    n = length prev
    entry i j =
      let rik = prev!!i!!k
          rkk = prev!!k!!k
          rkj = prev!!k!!j
          rij = prev!!i!!j
          loop = rik ++ star rkk ++ rkj
      in paren (loop ++ "|" ++ rij)

-- helpers for regex
star s  = s ++ "*"               -- Kleene star
paren s = "(" ++ s ++ ")"        -- wrap in parentheses
