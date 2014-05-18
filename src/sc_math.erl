
-module(sc_math).





-export([

    distance/2,
    eq_within/3

]).





%% @doc Returns the distance between two values; currently numeric only.

distance(X,Y) when is_number(X), is_number(Y) -> float(abs(X-Y)).  % useful for handling non-numeric types later





%% @doc Tests whether two numbers are equal to within a certain tolerance.

eq_within(X,Y, Dist) when abs(X-Y) =< Dist,           is_number(Dist) -> true;
eq_within(X,Y, Dist) when is_number(X), is_number(Y), is_number(Dist) -> false.
