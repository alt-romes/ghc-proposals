.. author:: Matthew Pickering
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/412>`_.
.. contents::
.. sectnum::

import for splice -- imports to use within a splice, at level -1
import for quote  -- imports to be used within a quote, at level 1
import for stage -1  -- imports to be used at stage -1, ie at splice

Respond to https://github.com/ghc-proposals/ghc-proposals/pull/412#issuecomment-905371210 with a concrete example of this working with splice + quote imports.

NO PATH BASED CSP. Only lifted.
But using lift instances requires the corresponding module to be available at
both runtime and compile time. Bummer but no way around.
No ESI => if imported in module with TH, both. Otherwise, just runtime.
ESI => depending on how its imported, either runtime or compile time or both.

Interaction between CSP and ESI

Respond to Sebastian's comment, explain how it works with our system.

-- If you write ``Lift`` then you can't use ExplicitSpliceImports in that module as you need CSP.

1. import splice A, either
    * A is not ESI
        * A is needed at compile time and runtime
        * And all of its dependecies too.
    * A is ESI
        * A is needed at compile time
        * Its normal and splice imports too
        * Its quote imports needed at runtime, but not compile time
1a. Module uses TH and import A

2. To define Lift, you CANNOT use ESI because you need CSP::

    X @ 0 and X @ 1
    x X = [| X |]

3. A module defining lift, and all its dependencies, are needed both at runtime and compile time when splice-imported.

TH example::

    $(printf "%n in %s") 10 "str" ==> "10 in str"


    -- $() -> +1 from inside to out (-1 to get inside)
    -- [||] -> -1 from inside to out (+1 to get inside)
    -- top-level splice - -1 to 0 (from inside to outside)

    import splice Prelude -- show(+1) ==> -1 to 0
    import quote Prelude -- shown



    -- BAN CSP 
    -- $( -1 ) ==> 0; [| +1 |] => 0

    qshow{-n-} = [| show{-n-} |]
                   --n+1------  

    gen{-n-} (D:xs{-n-}) x{-n-} = [| \n{-n+1-} -> $(gen{-n-} xs{-n-} [| $(x{-n-}) ++ show{-nbad,n+1good-} n{-n+1-} |]) |]
                                                                          --n---                         
                                                                       ---n + 1------------------------------------
                                                    ----Level n------------------------------------------------------
                                    --Level n+1------------------------------------------------------------------------
    ---Level n-----------------------------------------------------------------------------------------------------------

    module A

    gen -- level 0


    module B

    import A (gen) -- gen at level 0
    --import splice A (gen) -- gen at level -1

    foo = $(gen)

Add information about the possibility to eventually do a ``macro`` keyword, and
how that can be easily built on top of our design since it essentially amounts
to splitting a couple of granular definitions into a separate module during elaboration.

Explicit Stage Imports Extension
================================

We propose a new extension, ``ExplicitStageImports``, that restricts programs
to be level-correct -- i.e. with ``ExplicitStageImports``, a normal import
(level 0) cannot be used in a top-level splice (level -1) or within a quote
(level 1).

It modifies the import syntax so that imports which are to be used at the
splice or quote level are marked explicitly as so, essentially importing them
at a different level that matches the level of their use site.

In short:

* The goal -- only allow level correct programs.
* The mechanism -- control level via imports.

.. When the extension is enabled, path-based cross stage persistence is disabled
.. and normal imports /cannot/ be used at compile time (at levels ``< 0``).

Motivation
----------

The primary motivation for level-correct programs is for programmers and
compilers to be able to distinguish three different ways
that imported module imports are used:

1. Imported modules whose code is executed only at compile time;
2. Imported modules whose code is executed only at runtime;
3. Imported modules whose code is executed both at compile time and runtime.

Distinguishing these 3 different cases has several advantages:

1. Currently, if a module enables ``TemplateHaskell``, then all imported modules
   are compiled to object code before name resolution takes place. This ensures that any top level splices that may be encountered are able to be fully evaluated.
   This is a pessimisation because most of the imported identifiers, which we have taken such pains to ensure we can run, will not
   actually be used in a top-level splice.
   Proposals (such as `#14905 <https://gitlab.haskell.org/ghc/ghc/-/issues/14095>`_) to increase build parallelism are far less effective
   in projects which use ``TemplateHaskell`` because name resolution depends on code generation
   for all dependencies.
   By distinguishing imported modules whose code is executed only at compile time
   (which in common cases will be a small fraction of imported modules), we are
   able to improve this pessimisation.
2. GHC offers an ``-fno-code`` flag that instructs the compiler to parse and
   typecheck Haskell modules, but not to generate code. The intent is to offer
   quick feedback to the user. Any module imports of a module using
   ``TemplateHaskell`` must be compiled to object code.
   This is despite the fact that we will not generate object code for the module
   itself. By distinguishing imported modules whose code is executed only at
   compile time, we can significantly reduce this unfortunate work, and entirely in many
   cases.
3. Projects such as haskell-language-server face similar problems as 2., where they are interested only in the result of type-checking modules, but when ``TemplateHaskell`` is enabled a large
   number of modules have to be cautiously compiled to bytecode.
4. By using splice imports we can separate the dependencies into those only needed at build-time (1) and
   those only needed at runtime (2). We can then link only against those packages needed at runtime.
5. Currently, when cross-compiling, in modules that use ``TemplateHaskell``, all
   imported modules must be compiled for both host and target.
   By distinguishing imported modules used at compile time(i.e. not used at
   runtime), we can require only those modules to be compiled for the host.
   Similarly, by distinguishing imported modules used at runtime (i.e. not used at
   compile time), we can require only those modules to be compiled for the
   target. It can be very hard or impossible to make some packages available on
   some cross-compile target platforms, so this change would significantly
   improve the applicability of ``TemplateHaskell`` in these scenarios.


Definitions
-----------

level
  Each expression exists at a level. The level is increased by 1 when
  inside a quote and decreased by 1 inside a splice.
    In short:
        * ``$(e at n-1)`` is at level ``n``
        * ``[| e at n+1 |]`` is at level ``n``

  Therefore the level of an expression can be calculated as the number of
  quotes surrounding an expression subtract the number of splices. For
  example::

    -- foo is at level 0
    foo = $(let
      -- bar is at level -1
      bar = $(let
        -- baz is at level -2
        baz = [|
        -- qux is at level -1
          qux = [|
            -- quux is at level 0
            quux = [|
              -- quuz is at level 1
              quuz = 0
            |]
          |]
        |] in baz
      ) in bar
    )

top-level splice
  A splice, where the body is at a negative level, an unadorned
  declaration splice or a quasiquoter.


home module
  A module from the package that is currently being compiled.

Background: Loading for TH and Cross Stage Persistence
------------------------------------------------------

Currently, all module and package dependencies of any module that enables
``TemplateHaskell`` must be compiled and made further available at
compile-time to allow identifiers to be used both at the top-level (runtime) or
within top-level splices (compile time).

Additionally, a free variable, defined or bound at level ``0``, may be used in
the body of a quote (i.e. at a level ``n > 0``), which can be spliced in the
future, due to so called Cross Stage Persistence (CSP). For instance, the
following program is accepted because of cross stage persistence::

    {-# LANGUAGE TemplateHaskell #-}

    -- succ :: Int -> Int

    one = [| \x -> succ x |]
    two x = [| succ x |]


Crucially, in the rhs of ``one``, ``succ`` is bound at level 0 (at the top-level), but
used in the body of a quote at level 1 (while ``x`` is bound at level 1).  In
``two``, both ``succ`` *and* ``x`` are bound at level 0 but used at level 1.

There are two forms of Cross Stage persistence, both of which are needed to
make the examples work:

* **Path-based persistence**: all top-level identifiers at level 0 are
  made available at future levels (i.e., top level ``x`` bound at level ``n`` is also
  available at level ``n+1``, ``n+2``, ...).

  Intuitively, this is fine because all top-level identifiers will still exist in
  that module even if spliced at a future stage.

  This explains why the occurrence of ``succ`` in example ``one`` and ``two`` is valid.

* **Serialisation-based/Lift persistence**: if an identifier can't be persisted
  to a future stage using path-based csp, we will attempt a serialisation-based
  approach.

  As long as a value's representation can be computed at runtime, we
  can serialise that value to persist it to future stages. This serialisation is
  defined as ``lift`` of the ``Lift`` typeclass.

  Serialisation-based CSP explains why the ``x`` in ``two`` can be moved from
  a value that exists at compile time to one that exists at runtime. The
  compiler will implicitly introduce a call to ``lift`` such as::

      two x = [| succ x |]
      ===>
      two x = [| succ $(lift x) |]

  And lift will take care of converting the compile-time ``x`` into a runtime value.
  All base types such as ``Int``, ``Bool``, ``Float``, ... instance ``Lift``, and user
  types can instance it automatically with ``DeriveLift``.

Note: ``Lift`` instances will look something like::

    data MInt = Some Int | None
    instance Lift MInt where
        lift (None) = [| None |]
        lift (Some x) = [| Some $(lift x) |]

An important observation is that the data constructors ``None`` and ``Some``
are persisted using Path-based CSP. Operationally, ``None`` and ``Some`` are
needed both at compile-time *and*  runtime since they are matched on at compile
time, and persisted to be spliced in the future into a program that can make
use of them at runtime.

Intuitively, it's just that ``Lift`` converts a compile-time value to a runtime value *by definition*!

The corollary is that, regardless of ``ExplicitStageImports``, using in a
top-level splice a lift instance from module ``X`` implies ``X`` must necessarily be made
available at both compile time and runtime.

Proposed Change
---------------

The key idea is that making programs level-correct guarantees a clear stage
separation which allows the compiler to reason about stages in order to deliver
on our motivation.

The key change necessary for level-correctness is to forbid identifiers
*implicitly* being available at both compile-time and run-time in exchange for
*explicitly* importing bindings for either one, the other or both.

When the new language extension ``ExplicitStageImports`` is enabled, we **forbid**:

* All bindings imported using the traditional ``import`` statement from occurring inside
  of top-level splices (and thus being used compile-time).
* In its entirety, path-based cross stage persistence, thus forbidding
  traditional ``imports`` from being used within quotes.

Complementary, we **introduce** two new import modifiers to the import syntax:
``splice`` and ``quote``.

* A ``splice`` import of ``A`` will import all bindings of ``A`` to be used *only* at
  compile-time, within top-level splices.
* A ``quote`` import of ``B`` will import all bindings of ``B`` to be used
  *only* within quotes, which will be possibly used at runtime when those quotes spliced.

Note the clear duality in the forbidden *implicit* stage-related behaviours in exchange for
introducing dual *explicit* stage-related mechanisms.

The great benefit of being explicit over implicit is we no longer need to
pessimistically assume all modules to be needed both at compile-time vs
run-time, since explicitness tells us exactly which are needed when.

For modules where ``ExplicitStageImports`` is disabled, we keep the previous
behaviour: no restrictions on the stages at which imported bindings are in place,
since that module and its dependencies must still be pessimistically assumed to
be needed both at compile time and run time.

Splice imports
##############

An import is marked as a "splice" import when it is prefixed with ``splice``::

  {-# LANGUAGE ExplicitStageImports #-}
  {-# LANGUAGE TemplateHaskell #-}
  module Main where

  -- (1)
  import splice B (foo)

  -- (2)
  import A (bar)

  x = $(foo 25) -- accepted
  y = $(bar 33) -- rejected


The ``splice`` modifier indicates to the compiler that module ``B`` is only used at
compile time and hence the imports can **only** be used inside top-level
splices (1). When the extension is enabled, imports without the splice modifier
are only available at runtime and therefore not available to be used in
top-level splices (2). In this example, identifiers from ``B`` can **only** be
used in top-level splices and identifiers from ``A`` can be used everywhere,
apart from in top-level splices.

To make some of the initial motivation explicit:

1. Now when compiling module ``Main``, despite the fact ``TemplateHaskell`` is enabled,
   we know that only identifiers from module ``B`` will be used in top-level splices so
   only ``B`` (and its dependencies) needs to compiled to object code before starting to compile ``Main``.
2. When cross-compiling, only ``A`` needs to be built for the target and ``B``
   only for the host as it is only used at build-time.

If you require scenario (3), where a module is needed both at compile-time and
run-time, then two imports declarations can be used::

  -- (3)
  import C
  import splice C

Example
~~~~~~~

To examplify our design we use `printf` from "Template Meta-programming for Haskell" by
Simon Peyton Jones and Tim Sheard.

Let ``printf :: String -> Q Exp`` be defined in ``Printf``, such that the
arguments received by printf applied to a formatting string is determined at
compile time based on the format specifiers within the string::

    $(printf "Error: %s on line %d") "test" 123 :: String

According to our proposal, the following program would be rejected::

    {-# LANGUAGE ExplicitStageImports #-}

    import Printf (printf)

    -- rejected!
    x = $(printf "Error: %s on line %d") "test" 123 :: String

because ``printf`` was imported "normally" at the default level 0 and thus
cannot occur within a top-level splice (at level -1). For this program to be
stage correct, ``printf`` must be imported at level -1 to be used within a
top-level splice::

    {-# LANGUAGE ExplicitStageImports #-}

    import splice Printf (printf)

    -- accepted!
    x = $(printf "Error: %s on line %d") "test" 123 :: String

Splice-importing ``Printf`` makes it clear to both humans and compilers that
``printf`` will only be required at compile time, since it will only be used within top-level splices.

Quote imports
#############

An import is marked as a "quote" import when it is prefixed with ``quote``::

  {-# LANGUAGE ExplicitStageImports #-}
  {-# LANGUAGE TemplateHaskell #-}
  module Main where

  -- (1)
  import quote B (foo)

  -- (2)
  import A (bar)

  x = [| foo 25 |] -- accepted
  y = [| bar 33 |] -- rejected


The ``quote`` modifier indicates to the compiler that module ``B`` *may be*
used at runtime because it enables its identifiers to be used within *quotes*.

When a quote such as ``x = [| foo 25 |]`` is spliced, i.e. ``z = $(x)``,
its contents will be needed to execute the program at runtime (``y = foo 25``,
so evaluating ``y`` at runtime requires ``foo`` to be available):

A ``quote`` import says the above explicitly: the imported module can be used
at *runtime*. The compiler can then guarantee the module is available at
runtime.

When the extension is enabled, quote imports can **only** be used inside
quotes, that is, at level 1 (1). Imports without the quote modifier are only
available at *the top-level*, and therefore not available to be used inside
quotes (2). In this example, identifiers from ``B`` can **only** be used in
quotes and identifiers from ``A`` can be used everywhere, apart from quotes
(and splices).

**Why do we want to be explicit about quotes as well?**

Previously, path-based cross stage persistence meant *any* imported identifier
could eventually be used at runtime (when spliced)! This made path-based CSP an
enemy of explicit stage imports -- when we ``splice`` import a module, the
guarantee should be that the module is *only* needed at compile-time, but CSP
means all splice-imported modules could also be needed at *runtime*.

By forbidding path-based CSP, we guarantee that all dependencies that may be
needed at runtime, when identifiers from this module are spliced, are marked
explicitly as so.

Clarifying what runtime is
~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a bit unintuitive at first: aren't all imported modules by default
available at runtime -- and only splice imported ones at compile-time?  We've
been talking about non-splice imports as runtime imports, but now it's quote
imports that are runtime imports?

No! There's still just one run-time (level 0), and one compile-time (level -1).

But there is a critical distinction between the level of a module, and
the level a module *is imported at*.

In the a module ``Main``, top-level definitions and normal imports are at level ``0`` (runtime), however:

* A ``splice`` import *offsets* the level of all bindings in that module by ``-1``.
* A ``quote`` import *offsets* the level of all bindings in that module by ``+1``.

This means that all top-level bindings of a module imported with ``splice`` are
imported at level -1, *not at level 0*! Consequently, quote imports of that
module are effectively offset by ``-1``, or level ``-1 + 1``, or level ``0``,
which means at runtime in this ``Main`` module. So splice imports and quote
imports cancel themselves out perfectly.

Example
~~~~~~~

This offsetting can be understood more clearly through an example.
Module ``A`` splices ``foo`` from module ``B`` which both quotes ``bar`` from module ``C`` and uses ``baz`` from ``D``::

    {-# LANGUAGE ExplicitStageImports #-}
    module A where
    import splice B (foo)

    -- foo can be used within a splice (level -1) because of the splice import (-1).
    x = $(foo 10)


    {-# LANGUAGE ExplicitStageImports #-}
    module B where
    import D (baz)
    import quote C (bar)

    -- bar can be used within a quote (level +1) because of the quote import (+1)
    foo x
      | baz x = [| bar * 2 |]
      | otherwise = [| bar |]

    {-# LANGUAGE ExplicitStageImports #-}
    module C where
    bar = 42

    module D where
    baz 0 = True
    baz _ = False

In this chain of modules, both ``A`` and ``C`` are needed at runtime (since
``x`` can occur at runtime, and ``bar`` is part of the runtime definition of
``x``!), unlike module ``B`` which is only needed at compile-time (``foo`` is
not needed when the program executes!).

The perhaps curious case is ``D``: is it needed at compile time or runtime? It
does not use a splice import, so one could think it is needed at runtime -- but
here is where the distinction between the *offset* level and base level is
relevant. At a glance, ``D`` would be needed at runtime, however, it is only
being imported as a dependency of ``B`` which is *offset* -1. This makes ``D``
*also* offset at *-1*! Note how ``baz`` is just needed at compile time to define
``foo``, which is properly ``splice`` imported.

The transitive closure of a ``splice`` imported module is at the same level as
the imported module. ``quote`` imports offset the modules that will be needed
back to runtime, and make the levels all align correctly.

What happens without ExplicitStageImports
-----------------------------------------

TODO

Implementation
################

The syntax for imports is changed in the follow way::

  importdecl :: { LImportDecl GhcPs }
     : 'import' maybe_src maybe_safe optsplice optqualified maybe_pkg modid optqualified maybeas maybeimpspec


The ``splice`` keyword appears before the ``qualified`` keyword but after ``SOURCE``
and ``SAFE`` pragmas.

Resolution of scopes (often called "renaming") is blind to whether or not an
identifier was imported with ``splice``. This is important because it will allow
GHC to emit errors advising the user to modify their import declarations.

The typechecker will be modified to emit errors in the following case:

   It is an error to reference a non-``splice`` imported name from a negative
   level, and it is an error to reference a ``splice`` imported name from
   a non-negative level.


Then,
1. If a module is only available at compile time then the imports are only available in top-level splices.
2. If a module is only available at runtime then the imports are not available in top-level splices.
3. If a module is available at both runtime and compile time then the imports are available everywhere.

The driver will be modified to ensure that, for modules with
``-XTemplateHaskell``, object code is generated for ``splice`` imported modules,
whereas today it ensures object code is available for all imported modules.


Intuitive Specification of ``splice``
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Identifiers arising from splice imports can only be used at negative levels, ie, unquoted in a top-level splice::

  -- Accepted, because B is a splice import and B.qux is used at level -1
  foo = $(B.qux)

  -- Rejected, because B is a splice import and B.qux is used at level 0
  foo' =  B.qux


But identifiers from normal imports are rejected::

  -- Rejected, as A is not a splice import and used at level -1
  baz = $(A.zee)

An identifier can appear inside a top-level splice, if it is at a non-negative
level. For example, the following is legal::

  foo = $(B.qid [| A.zee |] )

Because ``A.zee`` is used at level 0 it doesn't need to be imported using a splice import.

Level Specification of ``splice``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Ordinary imports introduce variables at all non-negative levels (>= 0)
* Splice imports introduce variables at all negative levels. (< 0)

Ambiguity of instances and variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Resolution of scopes (often called "renaming") is blind to whether or not an
identifier was imported with ``splice``.

In the case of variables, variables which are splice imported can only be used
inside a top-level quotation but are reported as ambiguous if they clash with any
other variable in scope, for example::

  import A ( x )
  import splice B ( x )

  foo = $( x ) x

In this case, there is no ambiguity because ``A.x`` isn't allowed to be used in
the top-level splice, but we still produce an ambiguity error to prevent any confusing
situations about what is in scope. This position is conservative and allows more
flexibility in the future if it's deemed that the restriction should be relaxed.

For instances, a similar situation applies, splice and non-splice imports must
have a consistent view of imported instances::

  module X where
  data X = MkX

  module Normal where
  import X
  instance Show X where show _ = "normal"

  module Splice where
  import X
  instance Show X where show _ = "splice"

  module Bottom where
  import X (X(..))
  import splice X (X(..))
  import Normal ()
  import splice Splice ()
  import splice Language.Haskell.TH.Lib ( stringE )

  s1 = show MkX
  s2 = $( stringE (show MkX) )

This program is also rejected because the instances defined in ``Normal`` and ``Splice`` overlap.


Other Considerations
~~~~~~~~~~~~~~~~~~~~

When ``TemplateHaskell`` is disabled, then ``ExplicitSpliceImports`` is a no-op.

When ``TemplateHaskell`` is enabled but NOT ``ExplicitSpliceImports``, then all imports
are implicitly additionally imported as splice imports, which matches the current behaviour.

If the ``Prelude`` module is implicitly imported then it is also imported as a splice module. Hence the following is
allowed::

  zero = $(id [| 0 |])

If ``NoImplicitPrelude`` is enabled then you have to import ``Prelude`` as a splice
module as well in order to use names from ``Prelude`` in negative level splices::

  {-# LANGUAGE TemplateHaskell #-}
  {-# LANGUAGE ExplicitSpliceImports #-}
  {-# LANGUAGE NoImplicitPrelude #-}

  import splice Prelude

  -- accepted
  foo = $(id [|"foo"|])

  -- rejected
  foo = id $([|"foo"|])

All exported names are at level 0. Splice imports can't be rexported, unless
they are also imported normally.
Allowing splice imports to be exported would turn a build-time only import into a runtime
export. Maintaining the distinction between things only needed at build-time and
things only needed at runtime allows project dependencies to be separated in the
same way. This is important for cross-compilation.



Drawbacks
---------

* The user has to be aware of the significance of using splice imports.



Alternatives
------------

* ``splice`` imports could also bring identifiers into scope so that they
  can be used everywhere in a module, not **only** in top-level splices as
  the proposal suggest. This approach is not taken because it means that
  build-time only dependencies can't be distinguished from runtime dependencies

* Using a pragma rather than a syntactic modifier would fit in better with
  how ``SOURCE`` imports work and make writing backwards compatible code easier::

    import {-# SPLICE #-} B

* It might be proposed that an alternative would be to work out which modules
  need to be compiled based on usage inside a module. This would compromise the
  principle that we can learn about what's needed for a module just by looking
  at the import list in the module header.

* The extension could only apply to **home** modules, because the benefits of
  splice imports are when using GHC's ``--make`` mode. As the proposal stands,
  for uniformity, any module used inside a top-level splice must be marked as
  a splice module, even if it's an external module.

* Another alternative would be to allow even finer grained control of splice
  imports so that the cases of usage at levels -1 or -2 could be distinguished.
  This could be useful in some cross-compilation situations. This is the approach
  suggested in the `Stage Hygience for Template Haskell proposal <https://github.com/ghc-proposals/ghc-proposals/pull/243>`_.

  The syntax in this proposal can be extended in a natural way to allow for this by adding an optional
  integer component which specifies precisely what level the imported names should be allowed at::

    -- Can be used at -1
    import splice 1 A
    -- Can be used at -2
    import splice 2 A

  Practically, by far the most common situation is 2 stages.

* Since ``ExplicitSpliceImports`` is a no-op when ``TemplateHaskell`` is
  disabled, we could have ``ExplicitSpliceImports`` imply ``TemplateHaskell``.
  There is at least one case where this would be harmful: users may which to
  enable ``ExplicitSpliceImports`` globally for their project, but only
  carefully enable ``TemplateHaskell`` for a small number of modules.

* There are several proposals or the syntax of splice imports. Some have objected
  that the ``import splice`` suggestion is ungramatical, unlike ``import qualified`` or
  ``import hiding``.

  One possible alternative is ``$(import Foo)`` to represent a splice import, this
  syntax clashes with the existing syntax for declaration splices and significantly
  changes the structure of the import syntax.

  Another alternative suggested was ``import for splice`` which restores the
  gramatically nature of the import.



Unresolved Questions
--------------------

* Hs-boot modules
* Type families
* Instances and orphans
* Defaulting?
* Class constraints
* Classes in general

