> {-# LANGUAGE Rank2Types #-}
> {-# LANGUAGE EmptyDataDecls #-}

This module defines a domain-specific-language for constructing web pages.

> module Hstml where

We abstract from the determing shape, leaving only what programs of 
different shape have in common. [Move these to module Origami]

> data Fix s a = In{out :: s a (Fix s a)}

Only (covariant) bifunctors are suitable for Fixing.

> class Bifunctor s where
>	bimap :: (a -> c) -> (b -> d) -> (s a b  ->  s c d)

Bifunctor provides sufficient flexibility to capture a wide variety
of recursion patterns as datatype-generic programs.

> unfold :: Bifunctor s => (b -> s a b) -> b -> Fix s a
> unfold f = In  .  bimap id (unfold f)  .  f

> build :: Bifunctor s => (forall b. (s a b  ->  b) -> b)  ->  Fix s a
> build f = f In

> fold :: Bifunctor s => (s a b  ->  b) -> Fix s a -> b
> fold f = f  .  bimap id (fold f)  .  out

> gmap :: Bifunctor s => (a -> b) -> (Fix s a  -> Fix s b)
> gmap f = In  .  bimap f (gmap f)  .  out

> hylo :: Bifunctor s => (b  ->  s a b) -> (s a c -> c) -> b -> c
> hylo f g = g  .  bimap id (hylo f g)  . f

A page Element is either string or a tagged collection of elements.
Note that it is a Phantom type and will be used to make type safe 
combinators e.g. Element Html, Element Meta etc.

> data FElement a b = 
>	Entity {
>		tag :: String, 
>		attrs :: [(String, String)], 
>		children :: [b]} 
>	|
>	Text String

> instance Bifunctor FElement where
>	bimap f g (Entity cs attrs es) = Entity cs attrs (map g es)
>	bimap f g (Text txt) = Text txt

> type Element a = Fix FElement a

Elements are `compiled' by converting them to strings.

> instance Show b => Show (FElement a b) where
>	show (Text s) = s
>	show (Entity tag_name attrs es) = 
>		case es of
>			[] -> "<"++tag_name  ++  join(map attr_out attrs) ++ "/>"
>			_ ->  "<"  ++  tag_name  ++  join(map attr_out attrs)  ++  ">"++
>					 elem_join es ++
>					"</"++tag_name++">"
>		where 
>			elem_join [e] = show e
>			elem_join (x:xs) = show x ++ elem_join xs
>			join [] = []
>			join [s] = " " ++ s
> 			join (s:ss) = " " ++ s ++ "," ++ join ss
>			attr_out (k,v) = k ++ "=\"" ++ v ++ "\""

Here we define the builders for our Elements.

> type Attributes = [(String,String)]

> html,meta :: Attributes -> Element a
> html attrs = build $ \f -> f (Entity "html" attrs [])

> has :: Element a -> Element a -> Element a 
> x `has` y = let
>			x' = out x
>			xs = children x' ++ [y]
>		   in In $ Entity (tag x') (attrs x') xs

> meta attrs = In (Entity "meta" attrs [])

> title :: String -> Attributes -> Element a
> title cs attrs = In (Entity "title" attrs [In $ Text cs])

> tsb = html [] `has` title "Title" []



The top level element of our web pages is, of course, the html 
element.



Pages have a doctype and a sub-component.

> data Page a = Page {doctype :: String, content :: Element a}

For now we will only generate HTML 4.01 pages.



> instance Show (Page a) where
>	show (Page cs el) = cs  ++  fold show el

> page :: Element a -> Page a
> page element = Page "" element
