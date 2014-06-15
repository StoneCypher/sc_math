
-module(sc_math).





-export([

    distance/2,
    eq_within/3,
    gcd/2,
    lcm/2,

    ceiling/1,
      ceil/1,
    floor/1,

    extrema/1

]).





%% @doc Returns the distance between two values; currently numeric only.

distance(X,Y) when is_number(X), is_number(Y) -> float(abs(X-Y)).  % useful for handling non-numeric types later





%% @doc Tests whether two numbers are equal to within a certain tolerance.

eq_within(X,Y, Dist) when abs(X-Y) =< Dist,           is_number(Dist) -> true;
eq_within(X,Y, Dist) when is_number(X), is_number(Y), is_number(Dist) -> false.





%% @doc Produces the greatest common divisor of two positive integers.
%%
%% Why is by-zero undefined?  Because gcd divides both numbers then gcd must 
%% not be larger than either number.  Because gcd is the denominator, then if 
%% either number is zero, the denominator must not exceed zero, and therefore 
%% must be zero.  Division by zero is undefined.  Therefore `gcd(0, _)' or
%% `gcd(_, 0)' must be undefined.

gcd(A, B) when A > 0, B > 0 -> gcd_i(A, B).

gcd_i(A, 0) -> A;
gcd_i(A, B) -> gcd_i(B, A rem B).





%% @doc Produces the least common multiple of two integers.

lcm(A, B) -> (A*B) div gcd(A, B).





%% @equiv ceiling(X)

-spec ceil(X :: number()) -> integer().

ceil(X) ->

     ceiling(X).





%% @doc <span style="color: green; font-weight: bold;">Tested</span> Returns the ceiling (round towards positive infinity) of a float. ```1> sc_math:ceil(0.5).
%% 1
%%
%% 2> sc_math:ceil(0).
%% 0
%%
%% 3> sc_math:ceil(0.0).
%% 0
%%
%% 4> sc_math:ceil(1.0).
%% 1
%%
%% 5> sc_math:ceil(-1.0).
%% -1
%%
%% 6> sc_math:ceil(-1.5).
%% -1
%%
%% 7> sc_math:ceil(-1).
%% -1
%%
%% 8> sc_math:ceil(1).
%% 1'''
%%
%% Unit, doc and stochastic property (int as float identity; float always smaller within 1; all results integers) tested.

-spec ceiling(X :: number()) -> integer().

ceiling(X)

    when X < 0 ->

    trunc(X);





ceiling(X) ->

    T = trunc(X),

    case X - T of

        0   -> T;
        0.0 -> T;
        _   -> T + 1

    end.





%% @doc <span style="color: green; font-weight: bold;">Tested</span> Takes the floor (round towards negative infinity) of a number.  This is different than `erlang:trunc/1', which removes the mantissa, in its
%% handling of negative numbers: trunc diminishes towards zero, not towards negative infinity (note examples 6 and 7 below.) ```1> sc_math:floor(0.5).
%% 0
%%
%% 2> sc_math:floor(0).
%% 0
%%
%% 3> sc_math:floor(0.0).
%% 0
%%
%% 4> sc_math:floor(1.0).
%% 1
%%
%% 5> sc_math:floor(-1.0).
%% -1
%%
%% 6> sc_math:floor(-1.5).
%% -2
%%
%% 7> erlang:trunc(-1.5).
%% -1
%%
%% 8> sc_math:floor(-1).
%% -1
%%
%% 9> sc_math:floor(1).
%% 1'''
%%
%% Unit, doc and stochastic property (int as float identity; float always larger within 1; all results integers) tested.

-spec floor(X :: number()) -> integer().

floor(X) when X < 0 ->

    TruncX = trunc(X),

    case X - TruncX of
        0   -> TruncX;
        0.0 -> TruncX;
        _   -> TruncX - 1
    end;





floor(X) ->

    trunc(X).






%% @doc <span style="color: green; font-weight: bold;">Tested</span> Returns the lowest and highest values in a list of one or more member in the form `{Lo,Hi}'.  Undefined over the empty list.  Mixed-type safe; sorts according to type order rules.  ```1> sc_math:extrema([1,2,3,4]).
%% {1,4}
%%
%% 2> sc_math:extrema([1,2,3,a,b,c]).
%% {1,c}'''
%%
%% 3> sc_math:extrema( [] ).
%% ** exception error: no function clause matching sc:extrema([])'''
%%
%% Unit, doc and stochastic (min and max are list members) tested.

-spec extrema(List::list()) -> { Low::any(), Hi::any() }.

extrema([First | _] = List)

    when is_list(List) ->

    Next = fun(Next,T) ->

        {Lo, Hi} = T,

        Lo2 = if
            Next < Lo -> Next;
            true      -> Lo
        end,

        Hi2 = if
            Next > Hi -> Next;
            true      -> Hi
        end,

        {Lo2, Hi2}

    end,

    lists:foldl(Next, {First,First}, List).
