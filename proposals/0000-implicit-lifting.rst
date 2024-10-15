
We additionally propose ``ImplicitLifting``, an extension to disable/enable
implicit lifting of ill-staged expressions into well-staged ones by ``Lift``,
as an orthogonal complement to the above. The reasoning is that implicit
lifting is not always desireable (see example in the dedicated section), and
since we're re-thinking the implicit behaviours concering stages in this
proposal, it is fitting to also provide an extension to disable this particular
implicitness.

ImplicitLifting
~~~~~~~~~~~~~~~

``ImplicitLifting`` introduces ``lift`` calls automatically to make programs
stage correct (i.e. ``f x = [| x |]`` ==> ``f x = [| $(lift x) |]``), preserving the
current behaviour of Haskell programs.

``NoImplicitLifting`` disables this implicit behaviour in favour of explicitly
writing out the ``lift`` calls.

