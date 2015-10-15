data Token = Outer String | Inner String
  deriving (Show) -- TODO: This is for debugging only, remove.

tokenize :: String -> [Token]
tokenize str = next str (Outer "") []
  where next :: String -> Token -> [Token] -> [Token]
        next             []         token tokens = tokens ++ [token]
        next ('{':'{':more) (Outer outer) tokens = next more (Inner "") (tokens ++ [Outer outer])
        next (      c:more) (Outer outer) tokens = next more (Outer (outer ++ [c])) tokens
        next ('}':'}':more) (Inner inner) tokens = next more (Outer "") (tokens ++ [Inner inner])
        next (      c:more) (Inner inner) tokens = next more (Inner (inner ++ [c])) tokens

data Fragment = Loop String
              | Conditional String
              | End
              | Variable String
              | Raw String
  deriving (Show) -- TODO This is for debugging only, remove.

fragmentize :: Token -> Fragment
fragmentize (Outer str) = Raw str
fragmentize (Inner str) = case span (/= ' ') str of
  ("foreach ", list) -> Loop list
  ("if ", condition) -> Conditional condition
  ("end", "")        -> End
  (variable, "")     -> Variable variable

parse :: String -> [Fragment]
parse = fmap fragmentize . tokenize
