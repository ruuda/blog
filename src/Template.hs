-- Copyright 2015 Ruud van Asseldonk
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 3. See
-- the licence file in the root of the repository.

{-# LANGUAGE BangPatterns #-}

module Template
(
  Context,
  ContextValue (ListValue, StringValue, TemplateValue),
  Template,
  apply,
  listField,
  nestContext,
  parse,
  stringField
)
where

-- This file implements a tiny templating engine inspired by Moustache. A
-- context for applying templates resembles json. It features strings, lists,
-- maps, and templates itself. A template is a string where things inside
-- {{moustaches}} have special meaning:
--
--  * Conditionals: "{{if <cond>}} inner {{end}}" expands to " inner " if the
--    key <cond> is present. "{{if !cond}} inner {{end}}" expands to " inner "
--    if the key <cond> is not present.
--
--  * Loops: "{{foreach <list>}} inner {{end}}" expands to an expansion of
--    " inner " for every element of <list>. The context for the inner expansion
--    is the list element.
--
--  * Includes: "{{include <template>}}" expands to an expansion of the template
--    <template>. The context for this expansion is the current context.
--
--  * Variables: "{{<var>}}" expands to the value of <var> if that is a string.

import Control.Monad (join)

import qualified Data.Map.Strict as Map

data Token
  = Outer String
  | Inner String

-- Splits a string into things inside and outside of moustaches.
-- Note: for the current token string, we accumulate in reverse, and then
-- reverse the entire token at the end. This avoids quadratic time tokenization
-- due to linear-time append.
tokenize :: String -> [Token]
tokenize str = next str (Outer "")
  where
    next :: String -> Token -> [Token]
    next             [] (Outer outer) = [Outer $ reverse outer]
    next             [] (Inner inner) = [Inner $ reverse inner]
    next ('{':'{':more) (Outer outer) = (Outer $ reverse outer) : next more (Inner "")
    next (      c:more) (Outer outer) = next more (Outer $ c : outer)
    next ('}':'}':more) (Inner inner) = (Inner $ reverse inner) : next more (Outer "")
    next (      c:more) (Inner inner) = next more (Inner $ c : inner)

data Fragment
  = Conditional !String
  | End
  | Include !String
  | Loop !String
  | Raw !String
  | Variable !String

type Template = [Fragment]

-- Converts a string into fragments that can be fed into the interpreter.
parse :: String -> Template
parse = fmap toFragment . tokenize
  where
    toFragment (Outer str) = Raw str
    toFragment (Inner str) =
      case break (== ' ') str of
        ("if", condition) -> Conditional $ tail condition
        ("end", "")       -> End
        ("include", tmpl) -> Include $ tail tmpl
        ("foreach", list) -> Loop $ tail list
        (variable, _)     -> Variable variable

type Context = Map.Map String ContextValue
data ContextValue
  = ListValue ![Context]
  | StringValue !String
  | TemplateValue !Template

stringField :: String -> String -> Context
stringField key value = Map.singleton key (StringValue value)

listField :: String -> [Context] -> Context
listField key values = Map.singleton key (ListValue values)

-- Fakes nested context fields by prepending "<key>." to the keys of the child
-- context.
nestContext :: String -> Context -> Context
nestContext key child = Map.mapKeys ((key ++ ".") ++) child

-- Applies the template (fragments) with given context until an end fragment is
-- encountered, at which point the current string and the remaining fragments
-- are returned.
applyBlock :: [Fragment] -> Context -> (String, [Fragment])
applyBlock fragments context = next fragments ""
  where expand variable = case Map.lookup variable context of
          Just (StringValue str) -> str
          Just _                 -> variable ++ " is not a string"
          Nothing                -> "undefined"
        include name = case Map.lookup name context of
          Just (TemplateValue tmpl) -> apply tmpl context
          Just _                    -> name ++ " is not a template"
          Nothing                   -> "undefined"
        getList name = case Map.lookup name context of
          Just (ListValue list) -> list
          _                     -> []
        isTrue condition = if (head condition) == '!' then negated else present
          where present  = Map.member condition context
                negated  = not $ Map.member (tail condition) context
        next :: [Fragment] -> String -> (String, [Fragment])
        next              [] str = (str, [])
        next (fragment:more) str = case fragment of
          End               -> (str, more)
          Raw contents      -> next more (str ++ contents)
          Variable variable -> next more (str ++ expand variable)
          Conditional cond  -> next continue $ if (isTrue cond) then str ++ inner else str
            where (inner, continue) = applyBlock more context
          Include template  -> next more (str ++ include template)
          Loop list         -> next continue $ str ++ inner
            where inner         = join $ fmap (\ctx -> fst $ applyBlock more ctx) $ getList list
                  (_, continue) = applyBlock more context

-- Applies the template with the given context.
apply :: Template -> Context -> String
apply fragments context = fst $ applyBlock fragments context
