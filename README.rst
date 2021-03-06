Mead
====

It is a tiny parse transform.

**License**: MIT

**Author**: Uvarov Michael (freeakk@gmail.com)


.. image:: https://secure.travis-ci.org/mad-cocktail/mead.png?branch=master
    :alt: Build Status
    :target: http://travis-ci.org/mad-cocktail/mead

.. code-block:: erlang

     -compile({parse_transform, mead}).

Example 1
---------

From ``lists`` (Erlang/OTP).

Before:

.. code-block:: erlang

    usplit_1(X, Y, [Z | L], R, Rs) when Z > Y ->
        usplit_1(Y, Z, L, [X | R], Rs);
    usplit_1(X, Y, [Z | L], R, Rs) when Z == Y ->
        usplit_1(X, Y, L, R, Rs);
    usplit_1(X, Y, [Z | L], R, Rs) when Z > X ->
        usplit_1(Z, Y, L, [X | R], Rs);
    usplit_1(X, Y, [Z | L], R, Rs) when Z == X ->
        usplit_1(X, Y, L, R, Rs);
    usplit_1(X, Y, [Z | L], [], Rs) ->
        usplit_1(X, Y, L, [Z], Rs);
    usplit_1(X, Y, [Z | L], R, Rs) ->
        usplit_1_1(X, Y, L, R, Rs, Z);
    usplit_1(X, Y, [], R, Rs) ->
        rumergel([[Y, X | R] | Rs], [], asc).

After:


.. code-block:: erlang

    usplit_1(X, Y, [Z | L], R, _) when Z > Y  -> me(Y, Z, L, [X | R], _);
    usplit_1(_, Y, [Z | L], _, _) when Z == Y -> me(_, Y, L, _, _);
    usplit_1(X, _, [Z | L], R, _) when Z > X  -> me(Z, _, L, [X | R], Rs);
    usplit_1(X, _, [Z | L], _, _) when Z == X -> me(X, _, L, _, _);
    usplit_1(_, _, [Z | L], [], _)            -> me(_, _, L, [Z], _);
    usplit_1(X, Y, [Z | L], R, Rs) ->
        usplit_1_1(X, Y, L, R, Rs, Z);
    usplit_1(X, Y, [], R, Rs) ->
        rumergel([[Y, X | R] | Rs], [], asc).

Example 2
---------

From ``cyrsasl_digest`` (ejabberd).

Before:

.. code-block:: erlang

    parse1([$= | Cs], S, Ts) ->
        parse2(Cs, lists:reverse(S), "", Ts);
    parse1([$, | Cs], [], Ts) ->
        parse1(Cs, [], Ts);
    parse1([$\s | Cs], [], Ts) ->
        parse1(Cs, [], Ts);
    parse1([C | Cs], S, Ts) ->
        parse1(Cs, [C | S], Ts);
    parse1([], [], T) ->
        lists:reverse(T);
    parse1([], _S, _T) ->
        bad.

After:

.. code-block:: erlang

    parse1([$= | Cs], S, Ts)   -> parse2(Cs, lists:reverse(S), "", Ts);
    parse1([$, | Cs], [], _)   -> me(Cs, _, _);
    parse1([$\s | Cs], [], _)  -> me(Cs, _, _);
    parse1([C | Cs], S, _)     -> me(Cs, [C | S], _);
    parse1([], [], T)          -> lists:reverse(T);
    parse1([], _S, _T)         -> bad.

Arguments
=========

This parse transform adds the hidden arguments.

Source code:

.. code-block:: erlang

    map(X, [H|T]) ->
        [F(H)|map(X, T)];
    map(F, []) ->
        [].

Code after this parse transform:

.. code-block:: erlang

    map(_1 = X, _2 = [H|T]) ->
        [F(H)|map(X, T)];
    map(_1 = F, _2 = []) ->
        [].

Other functions
===============

-  ``fun_me()`` - returns the current function as a HOF (fun F/A);
-  ``my_arguments()`` - returns arguments as a list;
-  ``my_name()`` - returns the name of the current function;
-  ``my_arity()`` - returns the count of arguments of the current
   function.

Examples
--------

These functions can be used for formatting debug messages.

.. code-block:: erlang

    response(KeyVals, User, Passwd, Nonce, AuthzId, A2Prefix) ->
        %% debug
        error_logger:info_msg("[~s~s] Args: ~w~n",
            [?MODULE, my_name(), my_arguments()]),
        ...
