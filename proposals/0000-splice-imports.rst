.. author:: Matthew Pickering, Rodrigo Mesquita, Adam Gundry
.. date-accepted:: Leave blank. This will be filled in when the proposal is accepted.
.. ticket-url:: Leave blank. This will eventually be filled with the
                ticket URL which will track the progress of the
                implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/TODO>`_.
.. contents::
.. sectnum::


Explicit Stage Imports
======================

Template Haskell allows *staged metaprogramming*: writing Haskell code that is
executed during the compilation stage, rather than merely compiled so that it
can executed at the later runtime stage. In practice, most programs can be
divided into small portions that are executed at compile-time (e.g. the
definition of ``makeLenses``), and the majority of the code that is not.
However, there are currently no clear boundaries between stages, so the compiler
must pessimistically assume that anything may be needed at compile-time or
run-time.

This has a variety of negative consequences. In particular:

* Using Template Haskell causes compile-time performance to suffer due to
  unnecessary (re)compilation.  This is particularly relevant for interactive
  use of the compiler within an IDE (e.g. via Haskell Language Server).

* Cross-compilation is made more difficult by the need to compile code for a
  platform even though it will never be executed on that platform.

The fundamental problem is implicit cross-stage persistence (CSP): the ability for
definitions at one stage to be used at a later stage.  Thus we propose a new
pair of extensions that allow programmers to write level-correct programs
without ubiquitous cross-stage persistence:

* ``NoImplicitStagePersistence`` forbids normal top-level identifiers from
  occurring within top-level splices or quotes, and

* ``ExplicitStageImports`` allows imports that explicitly enable the use of the
  imported identifiers within top-level splices or quotes.

In short:

* The goal -- only allow level-correct programs (enforced by ``NoImplicitStagePersistence``).
* The mechanism -- control level via imports (enabled by ``ExplicitStageImports``).

This proposal draws on ideas discussed previously in
`proposal #243: Stage Hygiene for Template Haskell
<https://github.com/ghc-proposals/ghc-proposals/pull/243>`_ and
`proposal #412: Explicit Splice Imports
<https://github.com/ghc-proposals/ghc-proposals/pull/412>`_.


Motivation
----------

Level-correct programs are necessary when using staged programming so
that the program can be cleanly separated into compile-time and run-time
portions. The existing mechanism to ensure level correctness for imported
identifiers is called *path-based cross stage persistence*: informally, it allows you to
use imported identifiers at any future level.
We want to remove this, because leads to the need to compile all modules
in a project for both runtime and compile time.

This proposal introduces an explicit means to control the level at which identifiers
are imported at. Therefore instead of relying on implicit persistence of an imported
identifier, the programmmer has to explicitly request for the identifier to be available
at a later or earlier stage.

The result is that identifiers can be used at precisely the level they are
bound, and no other levels.
By being very precise at levels modules are needed at, there are many advantages:

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
   typecheck Haskell modules, but not to generate code, so as to offer
   quicker feedback to the user. However, any modules imported by a module using
   ``TemplateHaskell`` must be compiled to object code,
   despite the fact that we will not generate object code for the module
   itself. By distinguishing imported modules whose code is executed only at
   compile time, we can significantly reduce this unfortunate work, and entirely eliminate it in many
   cases.
3. IDEs such as Haskell Language Server face similar problems, where they are interested only in the result of type-checking modules, but when ``TemplateHaskell`` is enabled a large
   number of modules have to be cautiously compiled to bytecode.
4. By using splice imports we can separate the dependencies into those needed only at compile-time and
   those needed only at runtime. We can then link against only those modules needed at runtime.
5. Currently, when cross-compiling modules that use ``TemplateHaskell``, all
   imported modules must be compiled for both host and target.
   By distinguishing imported modules not used at runtime,
   we can avoid the need to compile them fotr the target.
   Similarly, by distinguishing imported modules not used at
   compile-time, we can avoid the need to compile them for the host.
   It can be very hard or impossible to make some packages available on
   some cross-compilation target platforms, so this change would significantly
   improve the applicability of ``TemplateHaskell`` in these scenarios.



Example
#######

A very common pattern for using Template Haskell is the following::

  {-# LANGUAGE TemplateHaskell #-}
  module M where
    import Control.Lens.TH (makeLenses)
    import N

    data T = MkT { foo :: Int }
    $(makeLenses ''T)
    ...

Here the ``makeLenses`` function is defined in a library, and used in a
declaration splice to generate some definitions (here lens bindings, but a
similar pattern is often used where libraries provide a TH-based mechanism for
deriving instances).

At the moment, GHC must compile dependent module ``N`` before it starts
type-checking module ``M``, because as far as it knows, running the splice might
end up executing code from ``N``.  Moreover, when the programmer makes any
change to the implementation of a function in module ``N``, GHC must re-run the
``makeLenses`` splice in case its results change. (TODO: is this correct?)

This proposal allows the programmer to be explicit about the fact that
``makeLenses`` is used only in a splice, whereas the other import is definitely
not used in splices::

  {-# LANGUAGE ExplicitStageImports #-}
  {-# LANGUAGE TemplateHaskell #-}
  module M where
    import splice Control.Lens.TH (makeLenses)
    import N

    data T = MkT { foo :: Int }
    $(makeLenses ''T)
    ...

Not only does this make the code easier to understand, but moreover GHC can now
tell from the imports that ``M`` depends only on the interface of ``N``, not on
its implementation.  Correspondingly, it is possible to start type-checking
``M`` as soon as ``N`` has been type-checked (before code generation has been
completed), and changes to the implementation of ``N`` that do not affect its
interface do not cause recompilation of ``M``.


Definitions
###########

**level**
  Within a module, every declaration and every (sub-)expression exists at an integer level.  The top-level declarations in the module are at level 0.  The level is increased by 1 when
  inside a quote and decreased by 1 inside a splice. In short:

  * ``$(e)`` is at level ``n`` iff ``e`` is at level ``n-1``
  * ``[| e |]`` is at level ``n`` iff ``e`` is at level ``n+1``

  Therefore the level of an expression can be calculated as the number of
  quotes surrounding the expression minus the number of splices. For
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

**stage**
  Either compile-time or run-time.

**cross-stage persistence**
  See `Background: Cross Stage Persistence`_.

**level-correct**
  A program where every use site of an identifier or class instance occurs at the same level
  as the level of the definition site.

**top-level splice**
  A splice that does not have any enclosing quotes/splices (i.e. whose body is at a negative level), a
  declaration splice or a quasiquoter.


Background: Cross Stage Persistence
###################################

Currently, for any module that enables ``TemplateHaskell``, identifiers imported
from any of its module dependencies can be used at both the top-level (runtime)
and within top-level splices (compile time). Note that identifiers defined
within the module cannot be used within splices (this is the *stage restriction*).

Additionally, a locally-bound variable defined at the top-level may be used in
the body of a quote, and then spliced in the future.

For instance, the following program is accepted::

    {-# LANGUAGE TemplateHaskell #-}

    -- succ :: Int -> Int

    one :: Q Exp
    one = [| \x -> succ x |]

    two :: Int -> Q Exp
    two y = [| succ y |]

Crucially, in the rhs of ``one``, ``succ`` is bound at level 0 (the top-level), but
used in the body of a quote at level 1 (while ``x`` is bound at level 1).  In
``two``, both ``succ`` *and* ``y`` are bound at level 0 but used at level 1.

There are two forms of Cross-Stage Persistence (CSP), both of which are needed to
make this example work:

* **Path-based persistence**: global definitions at level ``n`` are
  made available at future levels (``n+1``, ``n+2``, ...).
  Intuitively, this is possible because all global definitions will still exist in
  the defining module even if references to them are spliced at a future stage.

  This explains why the occurrence of ``succ`` in examples ``one`` and ``two`` is valid.

* **Serialisation-based persistence (Lift)**: locally-bound variables can't be persisted
  to a future stage using path-based CSP, but provided the variable's type is serialisable, we
  can serialise its value to persist it to future stages. This serialisation is
  defined as the ``lift`` method of the ``Lift`` typeclass.

  Serialisation-based CSP explains why the ``y`` in ``two`` can be moved from
  a value that exists at compile time to one that exists at runtime. The
  compiler will implicitly introduce a call to ``lift`` such as::

      two y = [| succ y |]
      ===>
      two y = [| succ $(lift y) |]

  And ``lift`` will take care of converting the compile-time ``y`` into a runtime value.
  All base types such as ``Int``, ``Bool``, ``Float``, ... instantiate ``Lift``, and user
  types can instantiate it automatically with ``DeriveLift``.


Overview of new design
######################

The key idea is that making programs level-correct requires us to distinguish
modules needed for use at compile time vs for use at runtime, by using new
*stage* imports. The compiler can leverage this information to fullfill our motivation.

At the language level, the change necessary for level-correctness is to forbid
identifiers *implicitly* being available at both compile-time and run-time in
exchange for *explicitly* importing bindings for either one, the other or both.

The ``ImplicitStagePersistence`` extension is introduce to control the existing
path-based cross stage peristence behaviour and compile-time availability of
all top-level identifiers. This can now be disabled to force programmers to
control levels specifically with staged imports.

When the new language extension ``NoImplicitStagePersistence`` is enabled
(i.e. ``ImplicitStagePersistence`` is disabled), we forbid:

* All bindings imported using the traditional ``import`` statement from occurring inside
  of top-level splices (and thus being used compile-time).
* Path-based cross stage persistence, thus forbidding traditional ``imported``
  bindings from being used within quotes.

For example, the following is accepted under the default ``ImplicitStagePersistence``,
but will be rejected under ``NoImplicitStagePersistence``::

   import B (foo)
   data C = MkC

   quoteC = [| MkC |]  -- Error: MkC defined at level 0 but used at level 1
   spliceC = $( foo )  -- Error: foo imported at level 0 but used at level -1

Under ``NoImplicitStagePersistence``, path-based cross stage persistence is disabled
and normal imports /cannot/ be used at compile time (at levels ``< 0``).

The ``ExplicitStageImports`` extension introduces two new import modifiers to
the import syntax, ``splice`` and ``quote``, which control the level at which
identifiers from the module are brought into scope:

* A ``splice`` import of ``A`` will import all bindings of ``A`` to be used *only* at
  level -1 (compile time)
* A ``quote`` import of ``B`` will import all bindings of ``B`` to be used
  *only* at level 1.

For example, the following is accepted under ``ExplicitStateImports``::

  import quote Foo (bar) -- bar is introduced at level 1
  import Foo (baz) -- baz is introduced at level 0
  import splice Foo (qux) -- qux is introduced at level -1

  foo = baz [|bar|] $(qux)


Proposed Change Specification
-----------------------------

ImplicitStagePersistence
########################

``ImplicitStagePersistence`` is a new extension that is enabled by default in all existing language editions.
When enabled, all imported
top-level identifiers are available to be used within splices, within quotes and at
the top-level, preserving the current behaviour. This means that when
``ImplicitStagePersistence`` is enabled and TH is used, the compiler will pessimistically load
all of the module dependencies at compile time (to make all identifiers
available at levels < 0) and link all those dependencies for
runtime-execution too (to make identifiers available at levels > 0). As
explained in the Motivation section, this is suboptimal because it often results in
unnecessary work at compile-time and
linking into the binary code that is unnecessary at runtime.

Under ``NoImplicitStagePersistence``, the program must be
level-correct in order to pass the type-checker. That is, identifiers may be used only
at the same level at which they were bound. Traditional ``import`` statements bind
identifiers at level 0 **only**, which means the identifiers cannot be used within
splices (at level -1) nor within quotes (at level 1).

Under ``NoImplicitStagePersistence`` it is an error to derive a stock instance
of ``Lift`` using the ``DeriveLift`` extension. See `Deriving Lift instances
and implicit lifting`_.


ExplicitStageImports
####################

``ExplicitStageImports`` enables the use of ``splice`` and ``quote`` imports, to
import bindings at level -1 and level +1, respectively. Staged imports are the
only way to use imported bindings within splices and quotes when
``NoImplicitStagePersistence`` is on.

``ExplicitStageImports`` implies ``NoImplicitStagePersistence``.  Thus users
typically need only enable ``ExplicitStageImports``.

When a module uses ``TemplateHaskell`` with ``NoImplicitStagePersistence``,
the module dependencies no longer need
to be pessimistically compiled and loaded at compile time. Instead, the modules
that are needed at compile-time versus runtime are determined by the explicit
``splice`` and ``quote`` imports relative to the module being compiled.

It is permitted to enable both ``ExplicitStageImports`` and
``ImplicitStagePersistence``, so that ``splice`` and ``quote`` imports can be
used, but ``ImplicitStagePersistence`` still allows CSP (and thus the compiler
must still be pessimistically assume all modules are needed at all stages). This
combination is supported to allow gradual migration of codebases following the
change, and for corner cases where the programmer may wish to use the syntax of
``splice`` and ``quote`` imports without obliging the whole module to be
level-correct.

Syntax for imports
~~~~~~~~~~~~~~~~~~

Under ``ExplicitStageImports``, the syntax for imports becomes::

  importdecl :: { LImportDecl GhcPs }
     : 'import' maybe_src maybe_safe optsplice optqualified maybe_pkg modid optqualified maybeas maybeimpspec

  optsplice :: { LImportStage }
     : 'splice' { SpliceStage }
     | 'quote'  { QuoteStage  }
     |          { NormalStage }


The ``splice`` or ``quote`` keyword appears before the ``qualified`` keyword but after ``SOURCE``
and ``SAFE`` pragmas.


Level specification of staged imports
#####################################

TODO: explain this more clearly

* Ordinary imports introduce variables at level 0
* Splice imports introduce variables at level -1
* Quote imports introduce variables at level 1

The ``splice`` or ``quote`` imported modules themselves may use normal, ``splice``, and ``quote`` imports:

* Normal imports of a ``splice`` import are (transitively) also imported at level -1, and thus loaded at compile-time as well.
* ``Quote`` imports of the ``splice`` import are offset to level 0, and thus will be made available at runtime.
* Other ``splice`` imports of the ``splice`` import will also be loaded at
  compile-time, since they may be used in the code generation step of the
  module being imported.

All exported names must exist at level 0. Splice or quote imports can't be rexported, unless
they are also imported normally.
Allowing splice imports to be exported would turn a build-time only import into a runtime
export, which is not level-correct.


Name resolution and ambiguous variables
#######################################

Name resolution ("renaming") does not take account of the level at which an
identifier was imported when disambiguating ambiguous names, even though this is
sometimes more conservative than necessary.  For example, the following program
is rejected::

  {-# LANGUAGE ExplicitSpliceImports #-}

  import A ( x )
  import splice B ( x )

  foo = $( x ) x

In this case, there is in principle no ambiguity because ``A.x`` isn't allowed
to be used in the top-level splice, and ``B.x`` isn't allowed to be used outside
the splice.  Thus the only disambiguation that will pass the type-checker is::

  foo = $( B.x ) A.x

We choose to reject this disambiguation to keep the design simple and prevent
any confusion about what is in scope. This position is conservative, and can be
relaxed in the future if more flexibility appears worthwhile.

Level correctness will be checked in the type-checker, where GHC has maximum
information available to produce good error messages advising the user how they
need to modify their import declarations.


Class instance resolution
#########################

Class instances carry a level, much like identifiers, and must be used at the
correct level.

TODO: expand on this. Do we really want to reject the following program? We need
to specify how instance resolution works and why the following is rejected (if
it is).

Similarly to ambiguous names, splice and non-splice imports must
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


Examples
--------

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


The ``splice`` modifier indicates to the compiler that module ``B`` is only
used at compile time and hence the imports can **only** be used inside
top-level splices (1) (because of ``NoImplicitStagePersistence``). When the
extension is enabled, imports without the splice modifier are only available at
runtime and therefore not available to be used in top-level splices (2). In
this example, identifiers from ``B`` can **only** be used in top-level splices
and identifiers from ``A`` cannot be used in top-level
splices.

To make some of the initial motivation explicit:

1. When compiling module ``Main``, even though ``TemplateHaskell`` is enabled,
   only identifiers from module ``B`` will be used in top-level splices so
   only ``B`` (and its dependencies) needs to compiled to object code before starting to compile ``Main``.
2. When cross-compiling, ``A`` needs to be built only for the target and ``B``
   only for the host.

If the same module is needed both at compile-time and
run-time, then two import declarations can be used::

  import C
  import splice C

Example
~~~~~~~

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

When the extension is enabled, quote imports can be used **only** inside
quotes, that is, at level 1 (1) (because of ``NoImplicitStagePersistence``).
Imports without the quote modifier are available only at *the top-level*, and
therefore not available to be used inside quotes (2). In this example,
identifiers from ``B`` can be used **only** in quotes, while identifiers from
``A`` cannot be used in quotes or splices.

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
##########################

This is a bit unintuitive at first: aren't all imported modules by default
available at runtime -- and only splice imported ones at compile-time?  We've
been talking about non-splice imports as runtime imports, but now it's quote
imports that are runtime imports?

No! There's still just one run-time and one compile-time.
But there is a critical distinction between the level of a module, and the
level a module *is imported at*.

In a module ``Main``, top-level definitions and normal imports are at level ``0`` (runtime), however:

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

TODO: some of the above needs to be said in the specification.


.. What about packages
.. ~~~~~~~~~~~~~~~~~~~

.. As we've seen above, in programs such as

..     module A where
..     import splice B (foo)
..     x = $(foo)

..     module B where
..     import quote C (bar)

..     foo = [| bar |]

..     module C where
..     bar = 42

.. ``ExplicitStageImports`` improves compilation by only requiring certain modules
.. to be loaded at compile-time. In this case, ``B`` will be compiled and loaded
.. at compile-time, and ``C`` won't.

.. However, at the package level, this kind of granularity is not good enough.
.. Specifically, if this package ``pkg-a`` is imported by some ``pkg-b``,

Higher levels and stages
########################

Essentially, bindings imported at level -1 are used at compile-time, and at
level 0 used at program runtime. However, what does it mean to have a binding
at level -2, or 2, or execute an expression at those higher levels?
Consider::

    module A where
    import splice B (foo)
    main = $(foo)

    module B where
    import splice C (bar)
    foo = $(bar)

    module C where
    bar = 10

``C`` is imported at level -1 by ``B``, and exists at level -2 for ``A``.
Ultimately, this means ``C`` is needed at the compile-time of ``B``, which is
happening at the compile-time of ``A``. However, under the lens of compiling
``A``, there only exists one compilation-time -- which is when *both* ``B`` and
``C`` are compiled. Generically, *levels* ``< 0`` are collapsed into a single
compilation *stage* that happens at ``A``'s compile time.

The dual situation, higher-level quotes, is symmetrical::

    module A where
    import splice B (test)
    ex = $(test)

    module B where
    import quote C (foo)
    test = [| foo |]

    module C where
    import quote D (bar)
    foo = [| bar |]

    module D where
    bar = 10

From the perspective of ``B`` (when it is considered level 0), its import of ``C`` is at level 1
and the transitive import of ``D`` is at level 2.
But when ``A`` is compiled, ``B`` is needed at compile-time (level -1), the bindings quote
imported from ``C`` may be needed at runtime (level 0) if spliced, but the
``D`` bindings quote imported from ``C`` are at level 1 and thus used at a
future runtime.

TODO: it's not entirely clear what point this section is making by the talk of
collapsing levels into stages.


.. If we consider three distinct packages for ``pkg-d`` for ``D``, ``pkg-a`` for ``A`` and ``B``, and ``pkg-c`` for ``C``:
..
.. * ``pkg-a`` depends on ``pkg-c`` at runtime
.. * ``pkg-d`` depends on ``pkg-a`` at compile-time (because of the ``splice``
..   import of ``A``) and runtime (because of ``A``'s quote import of ``B``)
.. * Therefore, ``pkg-d`` also depends on ``pkg-c`` at runtime, since it is a
..   runtime dependency of ``pkg-a``.
..
.. In this sense, the levels >= 0 also "collapse" into a single runtime stage.

.. First, we observe that whenever the package ``pkg-b`` is used at compile-time,
.. it is *also* needed at runtime of the package depending on it since ``pkg-b``
.. quotes itself -- despite only loading ``B`` at compile-time (and not ``C``).

.. If all modules in a package use ``NoImplicitStagePersistence``...
.. The compiler determines at the module-granularity which modules are needed at
.. compile-time and which are needed at runtime for all modules using
.. ``ExplicitStageImports`` and ``NoStageMagic``.

.. The great benefit of being explicit over implicit is we no longer need to
.. pessimistically assume all modules to be needed both at compile-time vs
.. run-time, since explicitness tells us exactly which are needed when.



Effect and Interactions
-----------------------

Typed Template Haskell
######################

Typed Template Haskell (TTH) is an extension of Template Haskell that allows
using type-safe staged programming for program optimization.  (Its typical use
cases are rather different from untyped TH, since in particular it does not
support declaration splices.)

Implementing ``NoImplicitStagePersistence`` for TTH is likely to require
significant additional effort, and there are other known issues with TTH (see
`Staging with Class: a Specification for Typed Template Haskell
<https://dl.acm.org/doi/abs/10.1145/3498723>`_). We propose that an initial
implementation of ``NoImplicitStagePersistence`` may support untyped TH but not
TTH (i.e. the compiler may reject programs using TTH under
``NoImplicitStagePersistence``).  In the long term, we believe that implementing
Staging with Class is desirable and consistent with the direction of travel
established by this proposal, but the full details of Staging with Class are out
of scope.


Deriving Lift instances and implicit lifting
############################################

TODO: explain the problem with ``Lift`` instances, the relationship between
``DeriveLift`` and our new extensions, and motivate our position.  If we are
planning to make a separate proposal for ``ImplicitLifting``, say so.

It isn't possible to define a non-orphan ``Lift`` instance with
``NoImplicitStagePersistence``, because the definition of ``Lift``
essentially amounts to serializing a datatype value from compile-time to
runtime -- i.e., ``Lift`` requires the datatype to be available both at
compile-time and runtime. To do this within the same module where the
datatype is defined, you need cross-stage persistence::

    X @ 0 and X @ 1
    x X = [| X |]

This isn't problematic, rather, just a result of what ``Lift`` means.
However, it may require/drive users to define ``Lift`` able datatypes in leaf
modules to benefit more from ``NoImplicitStagePersistence`` in general.

NB: All the dependencies of this module will also need to be
available both at runtime and compile time when this module is used to
generate code as a consequence of ``NoImplicitStagePersistence``.

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
available at both compile time and runtime (this may not hold for *orphan* ``Lift`` instances).


Implicit ``Prelude`` imports and ``NoImplicitPrelude``
######################################################

TODO: where a module uses an implicit ``Prelude`` import, does it merely get
``import Prelude`` or does it also get ``import quote Prelude`` and ``import
splice Prelude``?

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



Costs and Drawbacks
-------------------

* The user has to be aware of the significance of using splice imports.

  TODO: expand on this. The compile-time and cross-compilation benefits only
  obtain if users switch on the extensions.  In simple use cases (e.g.
  ``makeLenses``) it should be easy enough for users to write ``import splice``,
  but more complex cases are more complex.


* Since the mechanism to control the levels of binders is *module-granular*,
  code in certain situations is necessary to be defined across two modules, for
  instance, the following was previously accepted under ``ImplicitStagePersistence``::

    module M where
      data B = MkB
      x = [| MkB |]

  However to be level-correct with ``NoImplicitStagePersistence`` it needs to be
  split over two modules::

    module M where
      import quote N
      x = [| MkB |]

    module N where
      data B = MkB



Backward Compatibility
----------------------

Since ``ImplicitStagePersistence`` is enabled by default, this proposal is
backwards compatible.  Existing programs will continue to work unchanged, though
they may not benefit from available performance improvements.

Were ``NoImplicitStagePersistence`` to become the default in a future language
edition, this would be a breaking change, but we do not propose this pending
implementation and experience with the feature.


Future Directions
-----------------

Multiple levels within a single module
######################################

One possible design that mitigates the need for module-level granularity of
imports, inspired by the Racket language, is the introduction of an
additional ``macro`` keyword that introduces bindings at a different level.

A ``macro`` annotated binding will introduce a binding at the -1 level, without
requiring it to be ``splice`` imported from a different module.

We believe this proposal shouldn't include such a change for two reasons:

* First, our proposed design lays out the foundation for well-staged programs,
  and is forward-compatible/can be readily extended with such a ``macro``
  keyword.  Tentatively, the implementation could amount to splitting ``macro``
  bindings from non ``macro`` ones and elaborate the two sets of bindings into
  separate modules that use ``splice`` imports (and then GHC would handle them
  as described by this proposal).

* Second, we imagine the possible advent of local modules as described by
  https://github.com/ghc-proposals/ghc-proposals/pull/283 to bring forward all
  the convinience of the ``macro`` keyword without the need for additional
  language complexity (local modules are a much more general concept, but
  yields the same results wrt to having a dedicated ``macro``)


Level-correct package dependencies
##################################

The splice and quote imports in this proposal make it possible to express which
module dependencies are required at which stages.  Ultimately, it would make
sense to expose this distinction at the level of Cabal packages, so that Cabal
could build package dependencies only for the stages at which they are required.
This would primarily be of value in cross-compilation scenarios.

In the interests of keeping the work manageable, changes to Cabal are out of
scope for the current proposal, but we believe this proposal lays a foundation
for future work to improve Cabal's cross-compilation support.


Imports with explicit level numbers
###################################

The current proposal permits imports only at levels -1, 0 or 1. This means it is
not possible to introduce a binding for use in a splice contained within another
splice, which would require it to be at level -2.  (Note that nested quotes are
in any case not supported in GHC due to a separate restriction.)

An alternative would be to allow even finer grained control of splice imports so
that usage at level -2 or lower could be distinguished. This could be useful in
some cross-compilation situations. This is the approach suggested in the `Stage
Hygiene for Template Haskell proposal
<https://github.com/ghc-proposals/ghc-proposals/pull/243>`_.

The syntax in this proposal could be extended in a natural way to allow for this by adding an optional
integer component which specifies precisely what level the imported names should be allowed at::

    -- Can be used at -1
    import splice 1 A
    -- Can be used at -2
    import splice 2 A

Practically, by far the most common situation is 2 stages, so in the interests
of reducing complexity we do not propose supporting this at present.


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

* The extension could apply only to **home** modules (those from the package being compiled), because the benefits of
  splice imports are when using GHC's ``--make`` mode. As the proposal stands,
  for uniformity, any module used inside a top-level splice must be marked as
  a splice module, even if it's an external module.

* Since ``ExplicitStageImports`` is essentially useless when
  ``TemplateHaskell`` is disabled, we could have ``ExplicitStageImports`` imply
  ``TemplateHaskell``.  There is at least one case where this would be harmful:
  users may which to enable ``ExplicitStageImports`` globally for their
  project, but only carefully enable ``TemplateHaskell`` for a small number of
  modules.

* There are several proposals or the syntax of splice imports. Some have objected
  that the ``import splice`` suggestion is ungramatical, unlike ``import qualified`` or
  ``import hiding``.

  One possible alternative is ``$(import Foo)`` to represent a splice import, this
  syntax clashes with the existing syntax for declaration splices and significantly
  changes the structure of the import syntax.

  Another alternative suggested was ``import for splice`` which restores the
  gramatical nature of the import.

* We could consider disallowing a package quoting modules from itself and
  restrict quoting to modules imported from *different* packages. The problem
  with self quoting is that we lose some granularity regarding what exactly is
  needed at compile-time and runtime. By requiring users to specify the runtime
  dependencies in a different package we get a better compile-time vs runtime
  distinction which benefits our motivation.

  On the other hand, it's quite unfortunate to require having yet another
  package just for TH, and may drive away adoption...


Unresolved Questions
--------------------

* Hs-boot modules
* Type families
* Instances and orphans
* Defaulting?
* Class constraints
* Classes in general

* Respond to
  https://github.com/ghc-proposals/ghc-proposals/pull/412#issuecomment-905371210
  with a concrete example of this working with splice + quote imports.

* Respond to Sebastian's comment, explain how it works with our system.


.. import for splice -- imports to use within a splice, at level -1
.. import for quote  -- imports to be used within a quote, at level 1
.. import for stage -1  -- imports to be used at stage -1, ie at splice

.. NO PATH BASED CSP. Only lifted.
.. But using lift instances requires the corresponding module to be available at
.. both runtime and compile time. Bummer but no way around.
.. No ESI => if imported in module with TH, both. Otherwise, just runtime.
.. ESI => depending on how its imported, either runtime or compile time or both.

.. Interaction between CSP and ESI

.. 1. import splice A, either
..     * A is not ESI
..         * A is needed at compile time and runtime
..         * And all of its dependecies too.
..     * A is ESI
..         * A is needed at compile time
..         * Its normal and splice imports too
..         * Its quote imports needed at runtime, but not compile time
.. 1a. Module uses TH and import A


Implementation Plan
------------------

TODO