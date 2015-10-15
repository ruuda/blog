data Token = Outer String | Inner String
  deriving (Show) -- TODO: This is for debugging only, remove.

parseTemplate :: String -> [Token]
parseTemplate str = parse str (Outer "") []
  where parse :: String -> Token -> [Token] -> [Token]
        parse           []         token tokens = tokens ++ [token]
        parse ('{':'{':more) (Outer outer) tokens = parse more (Inner "") (tokens ++ [Outer outer])
        parse (      c:more) (Outer outer) tokens = parse more (Outer (outer ++ [c])) tokens
        parse ('}':'}':more) (Inner inner) tokens = parse more (Outer "") (tokens ++ [Inner inner])
        parse (      c:more) (Inner inner) tokens = parse more (Inner (inner ++ [c])) tokens

data Fragment = Raw String
              | Variable String
              | ForEach String
              | Conditional String
              | End
