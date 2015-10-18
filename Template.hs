-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

module Template ( Context
                , ContextValue(StringValue, ListValue)
                , Template
                , parse
                , apply ) where

-- This file implements a tiny templating engine inspired by Moustache. A
-- context for applying templates is a subset of json that features strings,
-- lists, and maps. A template is a string where things inside {{moustaches}}
-- have special meaning:
--
--  * Conditionals: "{{if <cond>}} inner {{end}}" expands to " inner " if the
--    key <cond> is present.
--
--  * Loops: "{{foreach <list>}} inner {{end}}" expands to an expansion of
--    " inner " for every element of <list>. The context for the inner expansion
--    is the list element.
--
--  * Variables: "{{<var>}}" expands to the value of <var> if that is a string.

import           Control.Monad (join)
import qualified Data.Map as M

data Token = Outer String | Inner String

-- Splits a string into things inside and outside of moustaches.
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

type Template = [Fragment]

-- Converts a string into fragments that can be fed into the interpreter.
parse :: String -> Template
parse = fmap toFragment . tokenize
  where toFragment (Outer str) = Raw str
        toFragment (Inner str) = case break (== ' ') str of
          ("foreach", list) -> Loop $ tail list
          ("if", condition) -> Conditional $ tail condition
          ("end", "")       -> End
          (variable, _)     -> Variable variable

type Context      = M.Map String ContextValue
data ContextValue = StringValue String
                  | ListValue [Context]

-- Applies the template (fragments) with given context until an end fragment is
-- encountered, at which point the currint string and the remaining fragments
-- are returned.
applyBlock :: [Fragment] -> Context -> (String, [Fragment])
applyBlock fragments context = next fragments ""
  where expand variable = case M.lookup variable context of
          Just (StringValue str) -> str
          Just (ListValue _)     -> variable ++ " is a list"
          Nothing                -> "undefined"
        getList name = case M.lookup name context of
          Just (ListValue list) -> list
          _                     -> []
        isTrue condition = M.member condition context
        next :: [Fragment] -> String -> (String, [Fragment])
        next              [] str = (str, [])
        next (fragment:more) str = case fragment of
          End               -> (str, more)
          Raw contents      -> next more (str ++ contents)
          Variable variable -> next more (str ++ expand variable)
          Conditional cond  -> next continue $ if (isTrue cond) then str ++ inner else str
            where (inner, continue) = applyBlock more context
          Loop list         -> next continue $ str ++ inner
            where inner         = join $ fmap (\ctx -> fst $ applyBlock more ctx) $ getList list
                  (_, continue) = applyBlock more context

apply :: Template -> Context -> String
apply fragments context = fst $ applyBlock fragments context
