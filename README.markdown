SmardEdit - A tobe smarteditor
==============================
As a FOPL-2010 project Simon Edwardsson and Daniel Gustafsson will
create a smart editor for a simple functional programming language
(subset of haskell) that will provide type-derived feedback to the
programmer. For example it will tell the programmer what type is
expected at certain contexts.

Since it is a smart editor, it will not save the actual text (in 
reallity there is no text) but will save the AST of the language
instead. So all changes that the programmer does are expressed as
changes of the syntax tree and not of the text string. This make it
so that the program is always parseable and it can't go in an inconsistent
state.

One of the goals is that this editor should do contious typechecking
during execution so it will try to minimise the need to re-typecheck
everything for the changes made.
