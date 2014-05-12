
-module(sc_math).





-export([

    eq_within/3

]).





eq_within(X,Y, Dist) when abs(X-Y) =< Dist,           is_number(Dist) -> true;
eq_within(X,Y, Dist) when is_number(X), is_number(Y), is_number(Dist) -> false.
