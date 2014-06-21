
-module(sc_math).





-export([

    distance/2,
    eq_within/3,
    gcd/2,
    lcm/2,

    ceiling/1,
      ceil/1,
    floor/1,

    extrema/1,

    list_product/1,

    range_scale/1

]).





-export_type([

    numeric_list/0

]).





-type numeric_list() :: [ number() ]. %% All members of a numeric list must be number()s.





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





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Takes the product of all numbers in the list.  Offered mostly to make dependant code clearer. ```1> sc:list_product([1,2,5.4]).
%% 10.8'''

-spec list_product(A::numeric_list()) -> number().

list_product(List)

    when is_list(List) ->

    list_product(List, 1).





list_product([], Counter) ->

    Counter;





list_product([Head|Tail], Counter) ->

    list_product(Tail, Counter*Head).





%% @doc <span style="color:orange;font-style:italic">Stoch untested</span> Get the scale of a same-sign numeric range.  Gives nonsense results for non-numeric lists, or for lists which have both positive and negative members.  For a numeric list [4,5,6,12], the scale of the range 4..12 is 3:1, which is represented as 3.0 . ```1> sc_math:range_scale([3, 4, 5, 6]).
%% 2.0
%%
%% 2> sc_math:range_scale([3, 6]).
%% 2.0
%%
%% 3> sc_math:range_scale([6, 5, 3]).
%% 2.0
%%
%% 4> sc_math:range_scale([3, 4, 5, 6, 7, 7.5]).
%% 2.5
%%
%% 5> sc_math:range_scale([3, 10, 12, 99]).
%% 33.0
%%
%% 6> sc_math:range_scale([3, 3, 3]).
%% 1.0'''
%%
%% Unit and doc tested.
%%
%% @since Version 479

-spec range_scale(NumList::numeric_list()) -> number().

range_scale(Nums)

    when is_list(Nums) ->

    {Lo, Hi} = sc_math:extrema(Nums),
    Hi/Lo.
