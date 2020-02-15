WARNING
========

this is an old expired version of the documentation.

Please use the new documentation instead. 

Here is the main page for the new documentation: https://github.com/zack-bitcoin/amoveo-docs 

And [here is the link for the newest version of the page you are currently looking at](https://github.com/zack-bitcoin/amoveo-docs/blob/master//design/elixir.md)

a few reasons I like erlang better than elixir.

1) elixir lets you rebind variables, which can make it easier to hide bugs.
2) elixir encourages a piping syntax, which is very conventient to think about, but often encourages bad behavior.

Imagine we wanted to do a memory intensive multi-step process to every item in a list.
If we used piping, then the memory requirement would grow with the number of elements in the list.

In general, it is more secure to process each item in the list entirely before moving on to the next item. This way you don't waste memory storing half-done computations.

These kinds of bugs are hard to measure or find once they are in your code.
3) elixir macros are too powerful. Blockchains need to be secure, we don't want people contributing code that will mess with the compiler. It can be too hard to reason about tools like that.