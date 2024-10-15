.. author:: Matthew Pickering, Rodrigo Mesquita
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


Implicit Lifting Extension
==========================

We propose ``ImplicitLifting``, an extension to disable/enable implicit lifting
of ill-staged expressions into well-staged ones by ``Lift``, as an orthogonal
complement to the proposal on ``ExplicitStageImports`` and
``NoImplicitStagePersistence``.

Motivation
----------

Currently, a Haskell program using Template Haskell can transparently use
values and bindings within top-level quotes. For instance, in the following
program::

    f :: Int -> Q Exp
    f x = [| x + 1 |]

``f`` receives an argument available at compile-time and returns an expression
which refers to that same argument at *runtime*. (Additionally, it references
``+``, a top-level identifier, but this proposal, as opposed to the
``ExplicitStageImports/NoImplicitStagePersistence`` proposal, is concerned only
with cross-stage persistence by ``Lift``).

This code is successfully compiled because GHC will introduce a call to
``lift``, a ``Lift`` typeclass member which *serializes* a compile-time value
into a runtime one::

    f :: Int -> Q Exp
    f x = [| $(lift x) + 1 |]

We propose to guard this implicit lifting behaviour behind ``ImplicitLifting``,
enabled by default.

Our motivation is twofold:

* Implicit lifting is not always desireable (see example below)

* As part of the holistic picture of Template Haskell considering
  level-correctness painted by the
  ``ExplicitStageImports/NoImplicitStagePersistence`` proposal, we believe this
  extension is necessary to have a compiler capable of staged programming
  free of implicit behaviours. If left out, we'd have a knob for path-based
  cross-stage persistence, but not one for implicit-lifting-based cross-stage
  persistence. ``ImplicitLifting`` is necessary for consistency!

The reason why implicit lifting is not always desireable is that it very often
causes much more to be serialized than necessary. When coupled with the stage
separation made possible by ``ExplicitStageImports``, we're putting performance
on the table by using implicit lifting::

    large record something field accessor but the large record is serialized

TODO: MATTHEW:

Proposed Change Specification
-----------------------------

``ImplicitLifting`` introduces ``lift`` calls automatically to make programs
stage correct (i.e. ``f x = [| x |]`` ==> ``f x = [| $(lift x) |]``), preserving the
current behaviour of Haskell programs. This is the current behaviour of the
compiler.

``NoImplicitLifting`` disables this implicit behaviour in favour of explicitly
writing out the ``lift`` calls.


Effect and Interactions
-----------------------

By allowing implicit lifting to be disabled, we enable users to opt-out of an
implicit behaviour that is not always helpful. Users who want to think more
carefully about what is lifted, perhaps for performance reasons, are given the
mechanisms to make explicitness required.

By default, ``ImplicitLifting`` is enabled, so the change is fully backwards
compatible.

Costs and Drawbacks
-------------------

None as far as we're aware.

Alternatives
------------

None for now.

Unresolved Questions
--------------------

None for now.

Implementation Plan
-------------------

The authors will implement the proposal.

.. Endorsements
.. -------------
.. (Optional) This section provides an opportunty for any third parties to express their
.. support for the proposal, and to say why they would like to see it adopted.
.. It is not mandatory for have any endorsements at all, but the more substantial
.. the proposal is, the more desirable it is to offer evidence that there is
.. significant demand from the community.  This section is one way to provide
.. such evidence.
