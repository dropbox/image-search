% Using the Dropbox API from Haskell
% Rian Hunter
% 1/2/2012

Introduction
------------

I love [Haskell](http://www.haskell.org/). My first encounter
with Haskell started out about eight
years ago. Like many people in those days, when I was in high school
I spent a lot of time playing around with code on my computer. Reading
and understanding open source projects was a main source of knowledge
and inspiration for me when I was learning how to program. When I came
upon the bzip2 homepage and consequently
[Julian Seward](http://en.wikipedia.org/wiki/Julian_Seward)'s homepage
I found a short note about Haskell and how
it was a super fun and interesting language to write a compiler for.
Haskell? What's that?

After reading more about Haskell, functional programming, lazy evaluation,
and type inference and seeing the elegance of the various code samples,
I was hooked. I spent the next couple of weeks going through "[Yet
Another Haskell Tutorial](http://www.cs.utah.edu/~hal/htut/)"
and I remember it being incredibly
difficult yet incredibly rewarding. After I wrote my first fold over a
recursive algebraic datatype, I felt like I was finally starting
to speak Haskell. I felt like I had massively improved as a programmer.

While I've been at Dropbox, [Python](http://www.python.org/)
has been my main language of computational expression.
Even though it was a bit rocky at first, I've grown
to really love Python and the culture of fast iteration and
[duck typing](http://en.wikipedia.org/wiki/Duck_typing).
Like a good Pythonista, I'm of the opinion that types are training wheels,
but that's really only until you use a language with a real type system.
In C# or Java, types can get in your way and even force you to
write overly verbose code or follow silly "design patterns." In Haskell, types help
you soar to higher computational ground. They encourage you to
model your data in coherent, concise, and elegant ways that feel
right. They aren't annoying.

More people should use Haskell. The steep learning curve forces
you to understand what you are doing at a deeper level and you
will be a better programmer because of it. To help that happen, this
post will be in a
[semi-literate programming](http://en.wikipedia.org/wiki/Literate_programming)
style and I'll be describing a
[Dropbox API](https://www.dropbox.com/developers)
app written in Haskell. This won't be like a normal tutorial
so you'll probably have to do a bit of supplemental
reading and practice afterward. The goal is to give you a
flavor of what a real program in Haskell looks like.

This post assumes no previous knowledge with Haskell but
it does assume moderate programming ability in another
language, e.g. C, C++, Ruby, Python, Java, or Lisp.
Since this post does not assume previous
Haskell experience the beginning will be more of a
Haskell tutorial and core concepts will be sectioned
off to facilitate the learning process.
This post is a published version of a
[Literate Haskell](http://www.haskell.org/haskellwiki/Literate_programming#Bird_Style) file.
Code lines prefixed with the "`>`" character are actually part of the final program.
This makes it so that it's possible to simply copy & paste the text
here into your favorite editor and run it, just make sure you save the
file with a ".lhs" extension. If you ever get tired
of reading you can get
[the real source](http://github.com/dropbox/image-search)
at the GitHub repo.

The Haskell implementation we'll be using is
[The Haskell Platform](http://hackage.haskell.org/platform/),
it's a full stack of Haskell tools prepackaged to work
out of the box on all three major desktop operating systems.
It's based on [GHC](http://haskell.org/ghc/), The Glasgow Haskell Compiler.
GHC is an advanced optimizing compiler
focused on producing efficient code.

Image Search for Dropbox
------------------------

Years ago [Robert Love](http://en.wikipedia.org/wiki/Robert_Love) of Linux kernel hacker fame wrote a
[FUSE](http://fuse.sourceforge.net/)
file system that made it so that user-created folders were populated with
[Beagle](http://en.wikipedia.org/wiki/Beagle_(software))
search results using the folder name as the search query. It was called
[beaglefs](http://svn.gnome.org/svn/beagle/trunk/beaglefs/README).
The point was
to demonstrate the power of user-space file systems, notably the
power of having so much more library code available to you than in
kernel-space.

We can do a similar thing with the Dropbox API. We're going
to write a hypothetical Dropbox API app that populates user-created
folders with
[Creative Commons](http://creativecommons.org/)
licensed images found by performing a web image search
using the folder name as the search term. Using Dropbox, all the
user has to do to perform an image search is simply create a folder.

Let's get started!

LANGUAGE Pragma
---------------

> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE DeriveDataTypeable #-}

Despite being more than two decades old, Haskell is still evolving.
You can tell your Haskell compiler to allow newer language
features using this syntax, this is called the LANGUAGE Pragma.
Please don't worry about what these exact language extensions do just yet,
you can read more in the [GHC docs](http://haskell.org/ghc/docs/latest/html/users_guide/pragmas.html#language-pragma "Language Pragma").

Import Declarations
-------------------

> import Yesod

This is an import declaration. Declarations like these inform the Haskell module
system that I am going to use definitions from this other module. A module in Haskell
is a collection of values (functions are values in Haskell too!), datatypes,
type synonyms, type classes, etc.

Here I am telling the module system to bring in all definitions from the module
`Yesod` in the namespace of this module. If I wanted to I could also access
those definitions prefixed with "Yesod." similar to Java.

[Yesod](http://www.yesodweb.com/) is a fully-featured modern web-framework for Haskell. We'll be using
it to create the web interface of our API app.

> import Text.XML.HXT.Core hiding (when)

This is a another import declaration. This is just like the Yesod import
except we use the "hiding" syntax to tell the Haskell module system to not
import the "when" definition into this module's namespace.
The module we are importing is the main module from
[HXT](http://www.fh-wedel.de/~si/HXmlToolbox/index.html), the
XML processing library that we use to parse out the search results from an
image search result page. More details on this much later.

> import Control.Applicative ((<$>), (<*>))
> import Control.Concurrent (forkIO, threadDelay, newChan,
>                            writeChan, readChan, Chan, isEmptyChan)
> import Control.Monad (forM_, when)
> import Control.Monad.Base (MonadBase)
> import Control.Monad.Trans.Control (MonadBaseControl)
> import Data.Maybe (fromJust, mapMaybe, isNothing)
> import Data.Text.Encoding (decodeUtf8With)
> import Data.Text.Encoding.Error (ignore)
> import Data.Typeable (Typeable)
> import Network.HTTP.Enumerator (simpleHttp)
> import System.FilePath.Posix (takeFileName, combine)

These import declarations are slightly different. Instead of bringing in
all names from the modules I am only bringing in specific names. This is very
similar to the "from module import name" statement in Python.

> import qualified Control.Exception.Lifted as C
> import qualified Data.ByteString as B
> import qualified Data.ByteString.Lazy as L
> import qualified Data.List as DL
> import qualified Data.Map as Map
> import qualified Data.Text as T
> import qualified Data.URLEncoded as URL
> import qualified Dropbox as DB
> import qualified Network.URI as URI

Remember how I said that I could also access names by prefixing them with
"Yesod." earlier? Adding `qualified` to the import declaration makes it so
you must refer to names in other modules using the module prefix. The
"as C" part in the first line makes it so that I can do `C.try` instead of
`Control.Exception.Lifted.try`.

Algebraic Datatypes
-------------------

> data ImageSearch = ImageSearch (Chan (DB.AccessToken, String)) DB.Config

This is an *algebraic datatype* declaration. Nevermind what algebraic means
for the moment, this is the basic way to define new types in Haskell.
Even though it's very short this little code actually does
a couple of things:

1. It defines a type called `ImageSearch`. This is the immediate text after "data".
2. It defines a function called `ImageSearch` that takes two arguments,
   the first of type `Chan (DB.AccessToken, String)`, the second of type `DB.Config`,
   and it returns a value of type `ImageSearch`, **ignore
   these complex types for now they aren't important**. This function
   is called a *constructor* in Haskell.

Constructors play a big role in Haskell. Wherever a name
can be bound to a value in Haskell, you can also use contructor pattern matching,
or *deconstruction*,
to extract out the data contained within that value. Here's
a function to get out the channel component of our `ImageSeach`
type:

< imageSearchChan (ImageSearch chan config) = chan

`imageSearchChan` takes in an `ImageSearch` argument and
return the channel wrapped inside of it. You'll see deconstruction
a lot more later.

So far we've defined a type called `ImageSearch` and we've
also defined a function called `ImageSearch`. This is okay because in
Haskell **type names and value names live in different namespaces**.

Maybe Type
----------

The `Maybe` type is one algebraic datatype that you'll
see a lot in Haskell code. It's often used to denote an error
value from a function or an optional argument to a function.

< data Maybe a = Nothing | Just a

Unlike `ImageSearch`, the `Maybe` type has not one but two
constructors: `Nothing` and
`Just`. You can use either constructor to create a value in the `Maybe` type.
A value of `Nothing` usually denotes an error in the `Maybe` type. A `Just`
value indicates success.

Another difference from our `ImageSearch` type is that `Maybe` isn't a concrete
type on its own; **it has to wrap some other type**. In Haskell, a "higher-order"
type like this is called a *type constructor*; it takes a concrete type and
returns a new concrete type. For example, `Maybe Int` or `Maybe String` are two
concrete types created by the `Maybe` type constructor. This is similar
to generics, like `List<T>`, in Java or C#. We use the *type variable*
"`a`" in the declaration to show that the `Maybe` type constructor can be
applied to any concrete type.

It's important to note
that **type constructors, like `Maybe`, are different from data constructors,
like `Nothing` or `Just`**.

Type Signatures & Currying
--------------------------

> toStrict :: L.ByteString -> B.ByteString
> toStrict = B.concat . L.toChunks

This is a function definition in Haskell. I'll explain the definition
in the next section but first I wanted to talk about the use of "`::`" since it keeps
coming up. This is how we explicitly tell the Haskell compiler what type
we expect a value to be. Even though a Haskell compiler is good enough to infer
the types of all values in most cases,
it's considered good practice to at least explicitly specify
the types of top-level definitions.

The arrow notation in the type signature denotes a function.
`toStrict` is a function that takes an `L.ByteString` value
and returns a `B.ByteString` value.

One cool thing about Haskell is that functions can only take one argument.
I know it doesn't sound cool but it's actually really cool. How do you
specify a function that takes two arguments you ask? Well that's a
function that takes a single argument and returns another function that
takes another argument. This reformulation of multiple-argument functions
is called *currying*. Currying is cool because it allows us
to easily do *partial application* with functions, you'll see examples of
this later.

Here's the type signature for a function that takes two arguments of
any two types and returns a value in the second type:

< twoArgFunc :: a -> b -> b

We use the type variables "`a`" and "`b`" to indicate that `twoArgFunc`
is *polymorphic*, i.e. we can apply `twoArgFunc` to
any two values of any two respective types.

With currying in mind, it shouldn't be too hard to figure out that the
arrow is right-associative, i.e. "`a -> b -> b`" actually means
"`a -> (b -> b)`". That would make sense, `twoArgFunc` is a function that
takes an argument and returns another function that takes an argument and
returns the final value. Say that over and over again until you understand it.

What if we grouped the arrow the other way?

< weirdFunc :: (a -> b) -> b

In this case `weirdFunc` is a function that takes another function as a
its sole argument and returns a value. This is much different from
`twoArgFunc` which instead returns a second function after it accepts its
first argument. Passing functions to functions like this is a
common idiom in Haskell and it's one of the strengths of a language
where functions are ordinary values just like integers and strings.

Functions
---------------------

The definition of `toStrict` makes use of the function composition operator, "`.`", but
Haskell functions don't have to be defined this way. Here's a function defined using
a variable name:

< square x = x * x

What about two arguments?

< plus x y = x + y

We can define a function that adds five to its argument by making using of currying:

< addFive = plus 5

We curried in the `5` argument to the `plus` function and just as we said, it returns a
new function that takes an argument and adds a five to it:

< addFive 2 == 7

Another way to define functions is to use a *lambda abstraction*.
You can write a function as an expression by using the backslash and arrow symbols.
Lambda abstractions are also known as *anonymous functions*. Here's another way
to define `plus`:

< plus = \x y -> x + y

All functions in Haskell are pure. This means that functions in Haskell cannot
do anything that causes side-effects like changing a global variable or performing IO.
More on this later.

Operators
---------

Okay now that you're cool with functions let's get back to `toStrict`. Here's
another way to define it using a lambda:

< toStrict = \x -> B.concat (L.toChunks x)

Instead, we define `toStrict` using the function composition operator, "`.`".
The function composition operator takes two functions
and returns a new function that passes its argument to the right
function and the result of that is passed to the left function and the result of that is
returned. Here's one possible definition:

< f . g = \x -> f (g x)

Yes this is a real way to define an operator in Haskell!
The function composition operator comes standard in Haskell but even if it didn't
you'd still be able to define it yourself. In Haskell, operators and functions
are actually two different syntaxes for the same thing.
The form above is the infix form but you can also use an operator in the prefix form, like
a normal function. Here's another way to define "`.`":

< (.) f g = \x -> f (g x)

In this definition of the function composition
operator we use prefix notation. It is only necessary to surround an operator
with parentheses to transform it into its prefix form.
Switching between infix and prefix notation
works for any operator, e.g. you
can add two numbers with `(+) 4 5` in Haskell.

The ability to switch
between prefix and infix notation isn't limited to operators, you can do it with
functions too by surrounding the function name with backticks, "`":

< div 1 2 == 1 `div` 2

This is useful for code like: ``"Dropbox" `isInfixOf` "The Dropbox API is sick!"``

One last thing about operators; Haskell has special syntax to curry
in arguments to operators in infix form. For example, `(+5)` is a function
that takes in a number and adds five to that number. These are called
*sections*. To further illustrate,
all of the following functions do the same thing:

* `(+5)`
* `(5+)`
* `\x -> x + 5`
* `(+) 5`
* `plus 5`
* `addFive`

With operator currying it's important to recognize that **the side you curry
the argument in matters**. For example, `(.g)` and `(g.)` behave differently.

The "`$`" Operator
------------------

Next to the "`.`" operator there is another function-oriented
operator that you'll see often in Haskell code. This is the
function application operator and it's defined like this:

< f $ x = f x

Weird right? Why does Haskell have this?

In Haskell, normal function application has a higher precedence than any
other operator and it's left-associative:

< f g h j x == (((f g) h) j) x

Conversely, "`$`" has the lowest precedence of all operators and it's
right-associative:

< f $ g $ h $ j $ x == f (g (h (j x)))

Using "`$`" can make Haskell code more readable as an alternative to
using parentheses. It has other uses too, more on that later.

Lists
-----

> listToPair :: [a] -> (a, a)
> listToPair [x, y] = (x, y)
> listToPair _ = error "called listToPair on list that isn't two elements"

`listToPair` is a little function I use to convert a two-element *list* to
a two-element *tuple*, usually called a *pair*.

A list in Haskell is a higher order type that represents an ordered collection of same-typed
values. A list type is denoted using brackets, e.g. "`[a]`" is the polymorphic list of any inner type and
"`[Int]`" is a concrete type that denotes a list of `Int` values.
Unlike vectors or arrays in other languages, you can't index into a Haskell list in constant
time. It is more akin to the traditional [Linked List](http://en.wikipedia.org/wiki/Linked_list) data
structure.

You can construct a list in a number of ways:

* The empty list: `[]`
* List literal: `[1, 2, 3]`
* Adding to the front of a list: `1 : [2]`

The "`:`" operator in Haskell constructs a new list that starts with the left argument
and continues with the right argument:

< 1 : [2, 3, 4] == [1, 2, 3, 4]

One last thing about the list type in Haskell, it's not that special. We can define our own list type
very simply:

< data MyList a = Nil | Cons a (MyList a)
<
< -- using "#" as my personal ":" operator
< x # xs = Cons x xs
<
< myHead :: MyList a -> a
< myHead (Cons x xs) = x
< myHead Nil = error "empty list"

Yep, a list is just a recursive algebraic datatype.

`foldr` and `map`
-----------------

In the Lisp tradition, lists are a fundamental data structure in Haskell.
They provide an elegant model for solving problems that deal with multiple
values at once. While lists are still fresh in your mind
let's go over two functions that are essential to know when manipulating lists.

The first function is `foldr`. `foldr` means "fold from the right" and
it's used to build a aggregate value by visiting all the elements in a list.
It's arguments are an aggregating function, a starting value, and
a list of values. The aggregating function
accepts two argu--Screw it, let's just define it using recursion:

< foldr :: (a -> b -> b) -> b -> [a] -> b
< foldr f z []     = z
< foldr f z (x:xs) = f x $ foldr f z xs

Notice how we used the "`:`" operator to deconstruct
the input list into its head and tail components.
Like I said, you can use `foldr` to aggregate things in a list, e.g.
adding all the numbers in a list:

< foldr (+) 0 [1..5] == 15

"`[1..5]`" is [syntactic sugar](http://en.wikipedia.org/wiki/Syntactic_sugar) for all integers from
1 to 5, inclusive.

Another less useful thing you can do with `foldr` is copy a list:

< foldr (:) [] ["foo", "bar", "baz"] == ["foo", "bar", "baz"]

`foldr`'s brother is `foldl`; it means "fold from the left."

< foldl :: (b -> a -> b) -> b -> [a] -> b
< foldl f z []     = z
< foldl f z (x:xs) = foldl f (f z x) xs

`foldl` collects values in the list from the left while `foldr` starts
from the right. Notice how the type signature of the aggregating
function is reversed, this should help as a sort of mnemonic when
using `foldr` and `foldl`. Though it may not seem like it,
the direction of the fold matters a lot. As an exercise try
copying a list by using `foldl` instead of `foldr`.

`map` is another common list operation.
`map` is awesome! It takes a function
and a list and returns a new list with the user-supplied function
applied to each value in the original list.
[Recursion is kind of clunky](http://haskell.org/haskellwiki/Things_to_avoid#Avoid_explicit_recursion)
so let's define it using `foldr`:

< map :: (a -> b) -> [a] -> [b]
< map f l = foldr ((:) . f) [] l

Even though `foldr` is more primitive than `map` I find myself
using `map` much more often. Here's how you would map a list of strings
to a list of their lengths:

< map length ["a", "list", "of", "strings"] == [1, 4, 2, 7]

Remember "`$`", the function application operator? We can also use `map`
to apply the same argument to a list of functions:

< map ($5) [square, (+5), div 100] == [25, 10, 20]

We curried in the `5` value into right side the "`$`" operator. That creates a function
that takes a function and then returns the application of that function to
the value `5`. Using `map` we then apply that to every function in the list.
This is why having an "`$`" operator in a functional language is a good idea
but it's also why `map` is awesome!

A Quick Note About Tuples
-------------------------

[I talked about lists](#lists) but I kind of ignored tuples. Like lists,
tuples are a way to group values together in Haskell. Unlike lists,
with tuples you can store values of different types in a single tuple.

< intAndString :: (Int, String)
< intAndString = (07734, "world")

A more subtle difference from lists is that tuples of different lengths
are of different types. For instance, writing a function that returns
the first element of a three-element tuple is easy:

< fst3 :: (a, b, c) -> a
< fst3 (x, _, _) = x

Unfortunately, there is no general way to define a "first" function
for tuples of any length without writing a function for each tuple type.
Haskell does at least provide a `fst` function for two-element tuples.

Final note, see how we ignored the second and third elements of
the tuple deconstruction by using "`_`"? This is a common way to avoid
assigning names in Haskell.

Template Haskell
----------------

> $(mkYesod "ImageSearch" [parseRoutes|
>                         / HomeR GET
>                         /dbredirect DbRedirectR GET
>                         |])

Yesod makes heavy use of [Template Haskell](http://www.haskell.org/haskellwiki/Template_Haskell).
Template Haskell allows you to do compile-time
[metaprogramming](http://en.wikipedia.org/wiki/Metaprogramming)
in Haskell, essentially writing code that writes code at compile-time. It's similar to
[C++ templates](http://en.wikipedia.org/wiki/Template_(C%2B%2B))
but it's a lot more like
[Lisp macros](http://en.wikipedia.org/wiki/Macro_(computer_science)#Lisp_.2F_S-expression_macros).
Template Haskell is a pretty exotic feature that is rarely used in Haskell but Yesod makes
use of it to minimize the amount of code you have to write to get a website up and running.

The `mkYesod` function here generates all the boilerplate code necessary for connecting
the HTTP routes to user-defined handler functions. In our app we have two HTTP routes:

1. `/ -> HomeR`
2. `/dbredirect -> DbRedirectR`

The first route connects to a resource called `HomeR`. The second route, located at `/dbredirect`,
connects to a resource called `DbRedirectR`. We'll define these resources later.

Type Classes
------------

This part of the app brings us to one of the most
powerful parts of Haskell's type system, *type classes*.

Type classes specify a collection of functions that can be applied to multiple types.
This is similar to interfaces in C# and Java or duck typing
in dynamically-typed languages like Python and Ruby. An instance
declaration actually defines the functions of a type
class for specific type. Formally, type classes are an extension to the
[Hindley-Milner](http://en.wikipedia.org/wiki/Hindley-Milner)
type system that Haskell implements to allow for
[ad-hoc polymorphism](http://en.wikipedia.org/wiki/Ad-hoc_polymorphism),
i.e. an advanced form of function overloading.

**Note that the word instance used in this context
is very different from the meaning in object-oriented languages. In an
object-oriented language instances are more akin to Haskell values**.

Section [6.3](http://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1270006.3)
of the
[Haskell 2010 Report](http://www.haskell.org/haskellwiki/Language_and_library_specification#The_Haskell_2010_report)
has a graphic of the standard Haskell type classes. Many core functions in the standard
library are actually part of some type class, e.g. `(==)`, the equals operator, is
part of the `Eq` type class. For fun, let's make a new type class called `Binary`. It defines
functions that convert data between the instance type and a byte format:

< import qualified Data.ByteString as B
<
< class Binary a where
<   toBinary :: a -> B.ByteString
<   fromBinary :: B.ByteString -> a

You can imagine I might use this class when serializing Haskell data over a
byte-oriented transmission medium, for example a file or a BSD socket. Here's an example
instance for the `String` type:

< import Data.Text as T
< import Data.Text.Encoding as E
< import Data.ByteString as B
<
< instance Binary String where
<   toBinary = E.encodeUtf8 . T.pack
<   fromBinary = T.unpack . E.decodeUtf8

For this instance `toBinary` is defined as a composition of
`E.encodeUtf8` and `T.pack`. **Notice the use of
`T.pack`, you'll see it a lot. `T.pack` converts a
`String` value into a `Text` value**. `E.encodeUtf8` converts
the resulting `Text` value into a `ByteString` value.
`fromBinary` does the inverse conversion, it converts a `ByteString`
value into a `String` value.

Let's define another instance:

< import Control.Arrow ((***))
< import Data.Int (Int32)
< import Data.Bits (shiftR, shiftL, (.&.))
<
< -- stores in network order, big endian
< instance Binary Int32 where
<   toBinary x = B.pack $ map (fromIntegral . (0xff.&.) . shiftR x . (*8))
<                $ reverse [0..3]
<   fromBinary x = sum $ map (uncurry shiftL . (fromIntegral *** (*8)))
<                  $ zip (B.unpack x) (reverse [0..3])

`fromBinary` in this instance may look kind of gnarly but you should know what it does;
it converts a `ByteString` value to an `Int32` value. Understanding it
is left as an exercise for the reader.

Now getting Haskell data into a byte format is as easy as calling `toBinary`. An important
distinction between type classes and the interfaces of C# and Java is that it's very easy
to add new functionality to existing types. In this example, *the creators of the both
the `String` and `Int32` types
didn't need any foreknowledge of the `Binary` type class*.
With interfaces, it would have been necessary to specify
the implementation of `toBinary` and `fromBinary`
at the time those types were defined.

It's also possible to define functions that depend on their arguments or return
values being part of a certain type class:

< packWithLengthHeader :: Binary a => a -> B.ByteString
< packWithLengthHeader x = B.append (toBinary $ B.length bin) bin
<   where bin = toBinary x

Here `packWithLengthHeader` requires that its input type "`a`" be a part of the
`Binary` type class, this is specified using the "`Binary a =>`" *context*
in the type signature. A
subtle point in the definition of this function is that it requires the `Int` type
to be a part of the `Binary` type class as well (the return value of `B.length`).

> instance Yesod ImageSearch where
>   approot _ = T.pack "http://localhost:3000"

Yesod requires you to declare a couple of instances for
your app type. Most of the definitions in the `Yesod` type
class are optional and have reasonable defaults but it does
require you to define `approot`, the root URL location
of your app. This is necessary for Yesod to be able generate URLs.

> instance RenderMessage ImageSearch FormMessage where
>   renderMessage _ _ = defaultFormMessage

Here's another instance declaration. The `RenderMessage` type class in this case
is actually based on two types, `ImageSearch` and `FormMessage`. It defines a function
called `renderMessage` which takes two arguments and returns `defaultFormMessage`.

Exceptions & Automatic Type Class Derivation
--------------------------------------------

> data EitherException = EitherException String
>     deriving (Show, Typeable)
> instance C.Exception EitherException

There are [multiple ways](http://www.randomhacks.net/articles/2007/03/10/haskell-8-ways-to-report-errors)
to specify errors in Haskell. In purely functional contexts
it's not uncommon to see the use of either the `Maybe` or `Either` types. In *monads* based on the
`IO` monad,
I usually like to use Haskell exceptions. What's a monad you say? It's
[complicated](http://www.haskell.org/haskellwiki/Monad_tutorials_timeline). Just kidding :)
I'll get to them later.

For now, we're defining a new exception type. It's the same algebraic datatype
declaration you saw earlier for our `ImageSearch` type except now there's this "deriving"
thing. A Haskell compiler can automatically derive instance declarations for some of
the standard type classes. For `EitherException` we automatically derive instances
for the type classes `Show` and `Typeable`. As a note,
the ability to automatically derive instances for the `Typeable` type class
was enabled by the LANGUAGE pragma `DeriveDataTypeable` above.

The last line declares `EitherException` to be an instance of `C.Exception`. It might be
weird that we didn't define any functions for this instance. This is because type classes
sometimes provide default implementations for the functions in the class. The `C.Exception`
type class actually provides default implementations for types that are part of the
`Typeable` type class.

Monads
------

> exceptOnFailure :: MonadBase IO m => m (Either String v) -> m v
> exceptOnFailure = (>>=either (C.throwIO . EitherException) return)

`exceptOnFailure` is a function that takes a monad that wraps an `Either` type
and returns that same monad except now wrapping the right side of the `Either` type. This makes sense
to me very clearly but I know, o patient reader, that this must look like gibberish to you.

First let's talk about monads. Monads are types within the `Monad` type class. The `Monad`
type class specifies two functions (or an operator and a function):

< class Monad m where
<   (>>=) :: m a -> (a -> m b) -> m b
<   return :: a -> m a

The [actual `Monad` type class definition](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Monad.html#t:Monad)
is slightly more complicated but for simplicity's sake this will do.

The first operator "`>>=`" is called *bind*. What does it do? Well it depends on your
monad instance! What we can say for sure is that it takes takes a monad of
type "`a`", a function that maps from "`a`" to a monad of
type "`b`", and returns a monad of type "`b`".

The second function, `return`, takes a value and wraps it in the monad. What it means to be
wrapped in the monad, again, depends on the instance. Please note, **the `return` function
isn't like the `return` statement in other languages, i.e. it doesn't short-circuit
execution of a monad bind sequence.**

If that sounds abstract that's because it is! [Monads are a sort of computational framework](http://www.haskell.org/haskellwiki/What_a_Monad_is_not),
many different types of computation are
monadic, i.e. they fit the form imposed by bind. Why are monads important? Perhaps the most
important reason they exist in Haskell is that they provide a purely functional way
to perform (or specify how to perform) IO. Monads are much more than just a way to do IO, however,
their general applicability extends to many things.

I won't dwell on monads too much in this post but for the purposes of your immediate understanding,
it suffices to explain the `IO` monad and do notation. Just as I'm not dwelling on
monads, you shouldn't either. It takes a long time to really understand and master what's going on.
The more you program in Haskell, the more it'll make sense.

So why can't we do IO in Haskell without the `IO` monad? Haskell is a purely functional language, that
means that functions in Haskell mimic their mathematical counterparts. A pure function is a
mathematical entity that consistently maps values from one domain to another. You expect `cos(0)`
to always evaluate to `1`, if it ever evaluated to something else something would be very wrong.

In a purely functional language how would you define `getChar`?

< getChar :: Char
< getChar = ...

It takes no arguments so how can it deterministically return what the user is submitting? The answer is
it can't. You can't do this in a purely functional language.

So what are our options? The answer is to
generate a set of actions to take as IO
is occurring, in a purely functional manner. This is what the `IO` monad
is and this is why values in the `IO` monad are called
*IO Actions*. It's a form of metaprogramming. Here's an IO action that prints "yes" if a user
types "y" and "no" otherwise:

< -- type signatures
< print :: Show a => a -> IO ()
< getChar :: IO Char
<
< main :: IO ()
< main = getChar >>= (\x -> print $ if x == 'y' then "yes" else "no")

Why does this work? Notice how the "output" of `getChar` isn't tied to its own value, instead
the bind operation gets the output value for us. We're using monads here to build and
model sequential and stateful computation, in a purely functional way!

You can imagine that writing real programs in the `IO` monad could get ugly
if you used "`>>=`" and lambdas everywhere so that's why Haskell has some syntactic sugar
for writing out complex monadic values. This is called do notation. Here's the same IO action
from above written in do notation.

< main = do
<   x <- getChar
<   print $ if x == 'y'
<           then "yes"
<           else "no"

In do notation each monad is bound using bind in order and values
are pulled out using the left-pointing arrow "`<-`".

We're coming to the close of yet another Haskell monad explanation
but before we finish I really want to emphasize that monads and
the `IO` monad in particular aren't that special. Here's my very
own implementation of the `IO` monad:

< data MyIO a = PrimIO a | forall b. CompIO (MyIO b) (b -> (MyIO a))
<
< myGetChar :: MyIO Char
< myGetChar = PrimIO 'a'
<
< myPrint :: Show a => a -> MyIO ()
< myPrint s = PrimIO ()
<
< instance Monad MyIO where
<   m >>= f = CompIO m f
<   return x = PrimIO x
<
< runMyIO :: MyIO m -> m
< runMyIO (PrimIO x) = x
< runMyIO (CompIO m f) = runMyIO (f (runMyIO m))

Of course in the real `IO` monad, `getChar` isn't hard-coded to return the same thing each time and
`print` actually prints something on your terminal. IO actions are run by your
Haskell runtime which is usually written in a language where you can actually call a non-pure
`getChar` function, like C.

Either Type
-----------

Now, back to `exceptOnFailure`. Let's look at it again:

< exceptOnFailure :: MonadBase IO m => m (Either String v) -> m v
< exceptOnFailure = (>>=either (C.throwIO . EitherException) return)

Is it still confusing? :)

Remember how I said earlier that the `Either` datatype was used to denote errors in Haskell?
The definition of `Either` looks like this:

< data Either a b = Left a | Right b
<
< either :: (a -> c) -> (b -> c) -> Either a b -> c
< either f _ (Left x) = f x
< either _ g (Right x) = g x

You can use the `either` function to return different values depending on the
`Either` datatype passed in. By convention the `Left` constructor is used to denote
an error value.

For `exceptOnFailure`, first we create a function that takes an `Either` value and if
it's a failure we throw an exception using `C.throwIO` otherwise we call `return` to
rewrap the success value. Then we curry in that function to the right side
of the "`>>=`" operator.

Some Utility Functions
----------------------

> myAuthStart :: DB.Config -> Maybe DB.URL -> IO (DB.RequestToken, DB.URL)
> myAuthStart config murl = exceptOnFailure $ DB.withManager $ \mgr ->
>   DB.authStart mgr config murl
>
> myAuthFinish :: DB.Config -> DB.RequestToken -> IO (DB.AccessToken, String)
> myAuthFinish config rt = exceptOnFailure $ DB.withManager $ \mgr ->
>   DB.authFinish mgr config rt
>
> myMetadata :: DB.Session -> DB.Path -> Maybe Integer
>               -> IO (DB.Meta, Maybe DB.FolderContents)
> myMetadata s p m = exceptOnFailure $ DB.withManager $ \mgr ->
>   DB.getMetadataWithChildren mgr s p m

Here I've defined a couple of convenience functions for using the Dropbox SDK.
This is just so I don't have to use `DB.withManager` every time I call these
functions. Also all of the vanilla Dropbox SDK functions return an `Either`
value in the `IO` monad so we make use of `exceptOnFailure` to automatically
throw an exception for us if something goes wrong.

> tryAll :: MonadBaseControl IO m => m a -> m (Either C.SomeException a)
> tryAll = C.try

`C.try` is the normal way to catch exceptions in Haskell. Unfortunately it
uses ad-hoc polymorphism within the `Exception` type class to determine
which exception it catches. Since `tryAll` has an explicit type signature,
it's bound to the instance of `C.try` that catches `C.SomeException`.

> dbRequestTokenSessionKey :: T.Text
> dbRequestTokenSessionKey = T.pack "db_request_token"

This is a constant I use for the name of my session key that stores the OAuth
request token when authenticating the user to my API app but more on
that later.

The Dropbox API Authentication Process
--------------------------------------

The Dropbox API uses OAuth to grant apps access to Dropbox user accounts. To access
any of the HTTP endpoints of the Dropbox API an app must provide
an *access token* as part of the HTTP request. Access tokens
are revokable long-lived per-user per-app tokens that are granted to an
app at the request of a user.

Acquiring an access token is a three step process:

1. The app
   [must obtain a unauthenticated *request token*](https://www.dropbox.com/developers/reference/api#request-token)
   via the
   Dropbox API.
2. The app redirects the user to the
   [Dropbox website](https://www.dropbox.com/developers/reference/api#authorize).
   Once the user is at the Dropbox website the user can either approve
   or deny the request to authenticate the request token for the app.
3. The Dropbox website redirects the user back to the app's website.
   The app can then [exchange the
   authenticated request token for an access token](https://www.dropbox.com/developers/reference/api#access-token).

A request token actually consists of two components, a key and a secret.
Only the key component should be exposed in plaintext. To ensure proper
security, the secret should only ever be known to the Dropbox servers
and the API app attempting authentication.
To exchange an authenticated request token for an access token,
the app must also provide the original secret of the request token. This
prevents third-parties from hijacking authenticated request tokens.

An access token is long-lived but at any point in time can become
invalid. When an access token becomes invalid it is the responsibility
of the API app to go through the authentication process again. This
allows Dropbox and its users to revoke access tokens at will.

Initiating the Authentication Process
-------------------------------------

> getHomeR :: Handler RepHtml
> getHomeR = do

Users can enable our app for their Dropbox account using the web.
Yesod is the web framework we are using to implement web pages in Haskell.
In Yesod all HTTP routes are values in the `Handler` monad. The convention is
that the handler name is the combination of the
HTTP method (GET in this case) and the name of the resource.
`getHomeR` is the handler for
the GET method on the "HomeR" resource which is located at root HTTP path, "/".
Handlers are connected to HTTP routes served by the web server via the use
of [Template Haskell](#template-haskell) above.

The `Handler` monad is essentially a decorated `IO` monad so don't worry
about what it is that much. You should use it just like you
would use the `IO` monad.

>   ImageSearch _ dbConfig <- getYesod

`getYesod` retrieves the app's value. In our app this value has the `ImageSearch`
type that we defined at the very beginning. Here we're deconstructing the `ImageSearch`
value and extracting only the config component (while ignoring the channel component).
The config value stores some information about our
app, like app key and locale, that is used by the Dropbox SDK.

>   myRender <- getUrlRender

`getUrlRender` gets the URL render function for your app.
It turns a resource value for your
app into a `Text` URL.

<a name="myAuthStartCall"></a>

>   (requestToken, authUrl) <- liftIO
>                              $ myAuthStart dbConfig
>                              $ Just $ T.unpack $ myRender DbRedirectR

Here we call the `DB.authStart` function (by way of `myAuthStart`). This function performs
the first step of the Dropbox API authentication process.
We pass in the URL, `DbRedirectR`, that we want the user
to be redirected to after they authenticate and we get
back our new unauthenticated request token
and the Dropbox URL where the user can authenticate it. Note that
`T.unpack` converts a `Text` value into a `String` value.

The `myAuthStart` function is in the `IO` monad so we make use of
`liftIO` to execute `myAuthStart` in the `Handler` monad.
Since `Handler` is a wrapper around the `IO` monad,
the `liftIO` function "lifts" the IO action into the higher monad.

>   let DB.RequestToken ky scrt = requestToken

Do notation allows you to bind names using "let" in a do block. This is for when
you need to assign a name to a value that isn't coming from a monad. Here
we're deconstructing the request token from `myAuthStart` to get the
key and the secret.

<a name="setSession"></a>

>   setSession dbRequestTokenSessionKey $ T.pack $ ky ++ "|" ++ scrt

A *session* in Yesod is a set of key-value pairs that is preserved per browsing
session with our web site. It's implemented using encryption on top of HTTP cookies.
We store the key and the secret of the request token in the session using `setSession`.
We'll need the secret to finish the authentication process after the user
authenticates our app.

`setSession` expects two `Text` values so we use `T.pack` to turn
the second argument from a `String` type into a `Text` type.

>   redirectText RedirectTemporary $ T.pack authUrl

Finally we redirect the user to the URL given to us from `myAuthStart`, `authUrl`. Dropbox
will ask the user if they want to allow our app to have access to their account. After
they respond, Dropbox will authenticate the request token and
then redirect the user to the URL passed
to the [call to `myAuthStart`](#myAuthStartCall) above.

Applicative Functors
--------------------

> getDropboxQueryArgs :: Handler (T.Text, T.Text)
> getDropboxQueryArgs = runInputGet $ (,)
>                       <$> ireq textField (T.pack "oauth_token")
>                       <*> ireq textField (T.pack "uid")

When Dropbox redirects the user back to our site it passes along some query args
in the GET request: "oauth_token" and "uid". Yesod provides a convenient way,
using `runInputGet`, to extract those in the handler.

The `(,)` operator is a special prefix-only operator that creates pairs for us:

< (,) x y = (x, y)

You might be wondering what "`<$>`" and "`<*>`" are. Relax, these are regular
operators. They are used for [applicative functors](http://www.haskell.org/haskellwiki/Applicative_functor).
Applicative functors are kind of like monads except not as powerful. I'm going to do
something horrible here and define "`<$>`" and "`<*>`" in monad terms:

< (<*>) :: Monad m => m (a -> b) -> m a -> m b
< mf <*> m = do
<   f <- mf
<   x <- m
<   return $ f x
<
< (<$>) :: Monad m => (a -> b) -> m a -> m b
< f <$> m = return f <*> m

If you pretend that bind, "`>>=`", doesn't exist and you only have "`<*>`" and `return`
defined for your type, then your type
isn't a monad, it's an applicative functor. The only exception is that `return`
is instead called `pure` in the applicative functor type class:

< class Applicative f where
<   pure  :: a -> f a
<   (<*>) :: f (a -> b) -> f a -> f b

The [actual `Applicative` type class definition](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Applicative.html#t:Applicative)
is slightly more complicated but for simplicity's sake this is good enough.

Now let's put it all together, in our definition of `getDropboxQueryArgs`
we apply `(,)` in the applicative functor, then pass the resulting value
to `runInputGet`. `runInputGet` then runs the applicative functor in the `Handler` monad.

I know it sounds crazy, I know it does, but luckily you don't have to fully understand
what's going on behind the scenes to understand how it's supposed to behave. Keep
writing and reading Haskell and eventually it'll make a lot of sense. Trust me,
if I can understand this stuff you can too.

The Authentication Result Handler
---------------------------------

> getDbRedirectR :: Handler RepHtml
> getDbRedirectR = do

This is the handler for the location of the redirect in the app authentication
process. After the user has given our app access to their account they
are redirected here.

>   mtoken <- lookupSession dbRequestTokenSessionKey

Remember how before we redirected the user to the Dropbox authentication URL
we first [set a key-value pair in the Yesod session](#setSession) using `setSession`? After
the user is redirected, we use the `lookupSession` function to get the token
back out. `lookupSession` returns a `Maybe` value so that if a key-value pair
does not exist in the current session it can return `Nothing`, otherwise
it will return the value wrapped in the `Just` constructor.

>   let noCookieResponse = defaultLayout [whamlet|
> cookies seem to be disable for your browser! to use this
> app you have to enable cookies.|]
>
>   when (isNothing mtoken) $ noCookieResponse >>= sendResponse

`when` is a nice function courtesy of `Control.Monad` that runs
the second argument, a value in some monad, only if the first
argument is true. It's defined like this:

<   when :: Monad m => Bool -> m () -> m ()
<   when p m = if p then m else return ()

If `mtoken` is bound to a `Nothing` value we'll return an error message
to the user asking them to enable cookies and discontinue normal execution
by using `sendResponse`. After the `isNothing`
check we are guaranteed that `mtoken` is bound to a `Just` value.

>   let rt@(sessionTokenKey:_) = T.splitOn (T.pack "|")
>                                $ fromJust mtoken

We extract the token from `mtoken` using `fromJust` and pass that along to
`T.splitOn`. `T.splitOn` will split a `Text` value into a list of `Text`
values using the input argument ("|" in this case) as the delimiter.
Then we use the "`@`" syntax to
simultaneously bind the result to the `rt` name and deconstruct the first
element of the result into the `sessionTokenKey` name.

>   (getTokenKey, _) <- getDropboxQueryArgs
>   when (getTokenKey /= sessionTokenKey)
>     $ invalidArgs [T.pack "oauth_token"]

To get the request token key that the user authenticated at the
Dropbox website we use `getDropboxQueryArgs`. Checking this key
against the request token key that we stored in the session helps
prevent [request forgery](http://en.wikipedia.org/wiki/CSRF).
If the keys don't match we stop execution of this handler by
calling `invalidArgs`. "`/=`" is the not equals operator, like
"`!=`" in other languages.

We do this verification because
we want to prevent other sites from successfully coercing a user
into invoking this handler. We only want the Dropbox website
to invoke this handler.

>   let requestToken = uncurry DB.RequestToken
>                      $ listToPair $ map T.unpack rt

Here's `listToPair` in action! Since it returns a tuple we use this nifty
built-in function called `uncurry`:

< uncurry f (x, y) = f x y

Using `uncurry` and `listToPair`, we pass the `DB.RequestToken`
constructor the request token key and secret that we stored in the
session. Since `rt` contains two `Text` values
we use "`map T.unpack`" to convert them into two `String` values.

>   ImageSearch chan dbConfig <- getYesod
>   accessToken <- liftIO $ myAuthFinish dbConfig requestToken

Now we can finish up the authentication process and get our access token. We
pass in the authenticated request token to `DB.authFinish` (by way
of `myAuthFinish`) and if everything is successful we obtain the access token.

>   liftIO $ writeChan chan accessToken

In this app we make use of
[Concurrent Haskell](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent.html).
This allows us to create
multiple independent threads of control in the `IO` monad.
There are logically two main concurrently running threads in this app, the
web server thread where all of our handler code is run, and the thread that
is updating the Dropbox accounts of the users of our
app with the relevant search results. We'll talk about
[our use of threads](#thread-architecture)
a lot more later.

Channels are a mechanism for typed inter-thread communication in Haskell. We use
the channel component of our app value to send over
the access token so that we can begin the updating process for this
user's Dropbox account.

>   defaultLayout [whamlet|okay you are hooked up!|]

Finally, we send a success message to the user's browser indicating
they have linked their account to our app.

Arrows
------

We've gone over monads and we've gone over their weaker counterparts, applicative functors.
[Arrows](http://www.haskell.org/arrows/index.html) are another
pattern you'll likely see used in Haskell code. They
commonly serve as a general framework for representing and
combining operations that convert input
to output, kind of like filters. Arrows,
like monads and applicative functors, are actually types and arrow types
are instances of the `Arrow` type class:

< class Arrow a where
<   arr :: (b -> c) -> a b c
<   (>>>) :: a b c -> a c d -> a b d
<   first :: a b c -> a (b, d) (c, d)

The [actual `Arrow` type class definition](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Arrow.html#t:Arrow)
is slightly more complicated but for simplicity's
sake this will do just fine :)

Given my analogy to filters you might think
that regular Haskell functions
resemble arrows and you'd be right. Arrows
are generalizations of regular Haskell functions and
functions are, in fact, defined as instances of the `Arrow` type class:

< instance Arrow (->) where
<   arr f = f
<   f >>> g = g . f
<   first f (x, y) = (f x, y)

To understand this instance declaration it's important to note that the
arrow symbol, "`->`", is an operator in the Haskell type language. Just like
normal operators, operators in the type language also have prefix forms, e.g.
"`(->) a a`" is the same type as "`a -> a`."

In "`instance Arrow (->)`", we turned the "`->`" operator into its prefix form
and used that to define the `Arrow` instance for functions.
In Haskell, the "`->`" operator in the type language is actually
a type constructor, i.e. when you apply it to two types it creates a new type,
a function of those two types.

> selectImageUrls :: ArrowXml a => a XmlTree String
> selectImageUrls = deep (isElem
>                         >>>
>                         hasName "a"
>                         >>>
>                         hasAttr "href"
>                         >>>
>                         getAttrValue "href"
>                         >>>
>                         isA isImgUrl
>                        )
>   where isImgUrl = DL.isInfixOf "imgurl="

It's common to use the `Arrow` type class as a basis for combinator libraries; libraries that
allow you to combine domain-specific functions in intuitive ways to create new functions.
HXT is one such combinator library, it's a library for manipulating XML documents. In our app
we define an arrow, `selectImageUrls`, that takes an XML document, denoted by the `XmlTree` type,
and extracts all of the links that contain the string "imgurl=". These are links in the
image search result page that contain the URLs to the found image files.

Thread Architecture
-------------------

Above I wrote that there were two main threads of execution in this app: the thread
that served out HTTP requests for our web front-end and the thread that implemented the
image search functionality in our user's Dropboxes. For the latter part, there are actually many
threads. There is one thread that is listening on the Haskell channel
for new users linking our app to their accounts from the web server thread.
There is a thread per user account that polls the user's Dropbox account every 30 seconds
and waits for new folders for it to populate with images. There is also a thread per new folder
per user account that is responsible for populating a specific folder with the images found
during the image search.

In other languages like C, C++, Java, or Python this unbounded use of threads wouldn't
be very efficient since threads in those languages often map 1:1 to
kernel-level threads. Normally you can't depend on kernel-level threads scaling
into the tens of thousands. In
Haskell (or at least in modern versions of GHC) threads are relatively cheap and the
runtime does a good job of distributing many Haskell threads across a bounded number
of kernel-level threads, usually one per CPU.

Image Uploading Thread
----------------------

> handleFolder :: DB.Session -> String -> IO ()
> handleFolder session filePath = do

`handleFolder` is the thread that is responsible for populating a specific folder
in a user's Dropbox with the image search results.

>   let searchTerm = takeFileName filePath

We use the file name portion of the folder path as the search term.

>       src__ = "http://www.yourfavoritesearchengine.com/"
>       src_ = src__ ++ "search?tbm=isch&tbs=sur:f&"
>       src  = src_ ++ (URL.export $ URL.importList [("q", searchTerm)])

`src` is the generated full image search URL. We use the `URLEncoded`
library to generate a properly escaped URL query string.

>   imageSearchResponse_ <- simpleHttp src
>   let imageSearchResponse = T.unpack
>                             $ decodeUtf8With ignore
>                             $ toStrict imageSearchResponse_

Here we fetch the image search result page using `simpleHttp`.
`simpleHttp` returns a lazy `ByteString` type but our XML
library requires a `String` type so we have to convert between the two
using a combination of `T.unpack`, `decodeUtf8With`, and `toStrict`.

>   images <- runX ( readString [ withParseHTML yes
>                               , withWarnings no
>                               ] imageSearchResponse
>                    >>>
>                    selectImageUrls
>                  )

Here we make use of the HXT XML processing library to parse out all the relevant
image search URLs from the HTML document returned from the search query. Notice
the use of the `selectImageUrls` arrow defined earlier. `images` is of type `[String]`.

>   forM_ images $ \url -> tryAll $ do

`forM_` executes a monad for each element in its list argument.
The second argument is a function that takes an element
from the input list and returns the corresponding monadic value.

Using `forM_` we're performing an IO action for each image
URL we parsed out of the result page to ultimately upload
that image into the user's Dropbox. We
wrap the monad expression in a `tryAll` to prevent an exception in the processing
of any single element from stopping the entire process.

>     urlenc <- URL.importURI $ fromJust $ URI.parseURIReference url
>     let imgUrl = fromJust $ URL.lookup ("imgurl" :: String) urlenc
>         imgUrlURI = fromJust $ URI.parseURIReference imgUrl
>         imgName = takeFileName $ URI.uriPath imgUrlURI
>         dropboxImgPath = combine filePath imgName

Each of the URLs that were parsed out of the HTML contain the source
URLs of the images in an embedded query arg, "imgurl". In this code
snippet we extract the actual source URL of the image, `imgUrl`, from
the "imgurl" query arg and we
generate the path into the user's Dropbox where we want to place the image,
`dropboxImgPath`.

>     image <- simpleHttp imgUrl

`simpleHttp` performs an HTTP request to the location of the source image and
returns the response body in a lazy `ByteString`.

>     DB.withManager $ \mgr ->
>       DB.addFile mgr session dropboxImgPath
>       $ DB.bsRequestBody $ toStrict image

This is the call to the Dropbox SDK that allows us to upload a file.
We upload the file data, `image`,
to the path we generated earlier, `dropboxImgPath`. If a file already
exists at that path, `DB.addFile` won't overwrite it.

New Folder Detection Thread
---------------------------

> handleUser :: Chan DB.AccessToken
>               -> DB.Config
>               -> DB.AccessToken -> [String] -> IO ()
> handleUser chan dbConfig accessToken_ foldersExplored = do

`handleUser` is the thread that runs for each user that is linked to our API app.
It monitors the user's Dropbox for new folders that we should populate with
search results. It polls the user's Dropbox every 30 seconds and loops
forever.

While this thread is running it's possible for the `handleNewUsers` thread to send us
a new access token to use through the channel given by the `chan` argument.

>   accessToken <- let getCurrentAccessToken x = do
>                        emp <- isEmptyChan chan
>                        if emp
>                          then return x
>                          else readChan chan >>= getCurrentAccessToken
>                  in getCurrentAccessToken accessToken_

I use the "`let ... in ...`" syntax to privately define the `getCurrentAccessToken`
function. This function repeatedly polls the access token channel using `isEmptyChan`
until it's empty at which point it returns the last access token that
was pulled off the channel.

>   efolders <- tryAll $ do

We wrap all the IO actions in this run of `handleUser` just in case
a transient exception occurs.

>     let session = DB.Session dbConfig accessToken

`session` is the name bound to the `DB.Session` value that the Dropbox SDK
interface needs to upload file data into a user's Dropbox.

>     metadata <- myMetadata session "/" Nothing

We make use of `myMetadata` to get a collection of all
the children inside the root of our API app sandbox, "/".

>     let (_, Just (DB.FolderContents _ children)) = metadata
>         folders = mapMaybe (\(DB.Meta base extra) ->
>                              case extra of
>                                DB.Folder -> Just $ DB.metaPath base
>                                _ -> Nothing) children
>         newFolders = folders DL.\\ foldersExplored

This snippet of code extracts out the list of new Dropbox paths
that we should be populate with image search results.

`mapMaybe` is a combination of `map` and `filter`. Any element
that the input function returns `Nothing` for is filtered out of the
returned list. Elements that the function returns a `Just` value for
are included in the output list without the `Just` wrapper. We use it here
to return all the paths in the "/" folder that are folders and we exclude
children that are plain files.

The "`DL.\\`" operator
returns all the elements in the first list operand
that aren't included in the second list operand, it's like
a [set difference](http://en.wikipedia.org/wiki/Complement_(set_theory)) operation.

>     forM_ newFolders (forkIO . handleFolder session)

Here we use the `forM_` function again. This time we spawn off
a new thread using `forkIO` for each new folder we found
in the app's sandbox folder.

>     return folders

We need to give our parent IO action access to the new list of paths in the
sandbox folder so it can keep track of what paths are new.

>   threadDelay $ 30 * 1000 * 1000

`threadDelay` is like `sleep()` in other languages; It pauses execution
for 30 seconds.

>   let curFolders = either (const []) id efolders

If an error occurred while polling the user's account for new folders
we bind an empty list to `curFolders`, otherwise we bind the current list
of folders to `curFolders`.

>   handleUser chan dbConfig accessToken
>     $ curFolders `DL.union` foldersExplored

After sleeping for 30 seconds we loop by recursing. This is the
common way in Haskell to loop in a monad. Before we recurse here we
update the total lists of folders we've ever seen so that we don't
attempt to update them again.

New App User Thread
-------------------

> handleNewUsers :: ImageSearch
>                   -> Map.Map String (Chan DB.AccessToken)
>                   -> IO ()
> handleNewUsers app_@(ImageSearch chan dbConfig) map_ = do

`handleNewUsers` is the thread that is listening for newly linked users to our
API app via the channel and spawns off a `handleUser` thread for
each new user.

We make use of the "`@`" syntax again to simultaneously
bind the `ImageSearch` argument
to the `app_` name and deconstruct it into its `chan` and
`dbConfig` components. The `map_` argument
keeps a mapping from user ID to the channel of the thread that is handling
that user ID. We need that so we can update the access token a thread
is using if it is revoked.

>   (accessToken, userId) <- readChan chan

`readChan` gets a value off the channel shared between this
thread and the web server thread. Each value is a tuple
that contains a user ID and an access token for that user
ID.

>   newMap <- case Map.lookup userId map_ of
>     Just atChan -> do
>       writeChan atChan accessToken
>       return map_
>     Nothing     -> do
>       nChan <- newChan
>       _ <- forkIO $ handleUser nChan dbConfig accessToken []
>       return $ Map.insert userId nChan map_

We look up the user ID we were given in our map of thread channels. If
we have a thread handling the account of user ID we got, we send it
the new access token by writing to its channel. If we don't have a thread handling
this specific user account then we create a new channel,
spawn off a new `handleUser` thread, and update our
channel map.

>   handleNewUsers app_ newMap

Finally we loop with the new map.

Main Program Entry Point
------------------------

> main :: IO ()
> main = do

So it's been a long and arduous path but finally we arrive at that
`main` IO action. The `main` IO action kicks off execution for every
Haskell program just like in C/C++ and Java.

>   let defaultAppKey = undefined :: String
>       defaultAppSecret = undefined :: String
>       defaultAppType = undefined :: DB.AccessType

Here are the default credentials for the app.
We use the Haskell value `undefined`, otherwise known as `_|_`.
This is a polymorphic constant that you can use anywhere in Haskell,
it can be of any type. An exception will be thrown if an undefined value
is ever evaluated in your Haskell program.
To get this app to work you will need to supply your own values
for these constants.

In theory these should be parsed out of the command line or
a configuration file but for the purposes of this demo app we
define them inline here.

>   chan <- newChan

Create the inter-thread communication channel using `newChan`.

>   dbConfig <- DB.mkConfig
>               DB.localeEn
>               defaultAppKey
>               defaultAppSecret
>               defaultAppType
>   let app_ = ImageSearch chan dbConfig

Create our application specific `ImageSearch` value. It contains
both the channel and a `DB.Config` value.

>   _ <- forkIO $ handleNewUsers app_ Map.empty

Kick off the `handleNewUsers` thread that accepts new users to our app.

>   warpDebug 3000 app_

And finally, call `warpDebug` which kicks off our Yesod web interface.

Conclusion
----------

That's it. That's our Dropbox API app in Haskell. If you were a
newcomer to Haskell this would be a healthy time to have tons
of questions. Actually if I've done my job right you should
be very curious to know more about Haskell :) Head on over
to [HaskellWiki](http://www.haskell.org/haskellwiki/Learning_Haskell)
and start your journey. If you want a nice friendly book to help you get
more formally acquainted I can recommend both
"[Real World Haskell](http://book.realworldhaskell.org/)" and
"[Learn You a Haskell for Great Good](http://learnyouahaskell.com/)".
One piece of advice for your new Haskell journey: *don't sweat
the monads*.

As for our API app, it's actually not finished yet. One huge thing
missing is that it doesn't remember which users linked to our
app and what folders we've populated across restarts, we'd
need to store that data in some kind of persistent database
to fix that.

Another pain point is the user has to wait 30 seconds in
the worst case for their folders to be populated with images.
This is because each of our `handleUser` threads poll
the Dropbox API every 30 seconds.
While this is bad from a user experience
perspective it's also bad from an engineering perspective.
This will cause the load we induce on the Dropbox
API to increase linearly with the number of users using our
app, we'd instead like it to increase linearly with the
number of active users using our app. Currently
there's no way to get around this issue but we're
working on it!

Other minor improvements include picking a better algorithm
to decide which folders we populate with photos, better
HTML for our web interface, and streaming uploads to the
Dropbox API. I'm sure there are more.

As an exercise, consider fixing some of these problems,
remember you can
[fork this project](http://github.com/dropbox/image-search)
on GitHub. By the way our
[Haskell SDK](http://github.com/dropbox/dropbox-haskell-sdk)
is also on GitHub, you may need to fork that too.

If you have any questions, feel free to reach out. My
email is rian+dropbox+com.
[Have fun](http://www.dropbox.com/jobs)!

*Many thanks to Kannan Goundan, Brian Smith, Dan Wheeler, ChenLi Wang,
Martin Baker, Tony Grue, Ramsey Homsany, Bart Volkmer, Jie Tang,
Chris Varenhorst, and Scott Loganbill for their help in reviewing this
post. Also special thanks to Michael Snoyman for creating Yesod and
accepting my patches. Lastly, a huge thanks to all those
who have researched and pushed Haskell forward throughout the years.*