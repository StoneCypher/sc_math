
-module(sc_math).





-export([

    distance/2,
    eq_within/3,
    gcd/2

]).





%% @doc Returns the distance between two values; currently numeric only.

distance(X,Y) when is_number(X), is_number(Y) -> float(abs(X-Y)).  % useful for handling non-numeric types later





%% @doc Tests whether two numbers are equal to within a certain tolerance.

eq_within(X,Y, Dist) when abs(X-Y) =< Dist,           is_number(Dist) -> true;
eq_within(X,Y, Dist) when is_number(X), is_number(Y), is_number(Dist) -> false.





%% @doc Produces the greatest common divisor of two integers.
%%
%% Why is by-zero undefined?  Because gcd divides both numbers then gcd must 
%% not be larger than either number.  Because gcd is the denominator, then if 
%% either number is zero, the denominator must not exceed zero, and therefore 
%% must be zero.  Division by zero is undefined.  Therefore `gcd(0,_)' or
%% `gcd(_,0)' must be undefined.

gcd(A, B) when A > 0, B > 0 -> gcd_i(A, B).

gcd_i(A, 0) -> A;
gcd_i(A, B) -> gcd_i(B, A rem B).
