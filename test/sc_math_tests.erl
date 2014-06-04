
-module(sc_math_tests).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").





distance_test_() ->

    { "distance/2 tests", [

        { "0.0 to 0.0",   ?_assert( 0.0 == sc_math:distance( 0.0,  0.0) ) },
        { "1.0 to 1.0",   ?_assert( 0.0 == sc_math:distance( 1.0,  1.0) ) },
        { "-1.0 to -1.0", ?_assert( 0.0 == sc_math:distance(-1.0, -1.0) ) },

        { "0.0 to 1.0",   ?_assert( 1.0 == sc_math:distance( 0.0,  1.0) ) },
        { "1.0 to 0.0",   ?_assert( 1.0 == sc_math:distance( 1.0,  0.0) ) },
        { "0.0 to -1.0",  ?_assert( 1.0 == sc_math:distance( 0.0, -1.0) ) },
        { "-1.0 to 0.0",  ?_assert( 1.0 == sc_math:distance(-1.0,  0.0) ) },
        { "1.0 to -1.0",  ?_assert( 2.0 == sc_math:distance( 1.0, -1.0) ) },
        { "-1.0 to 1.0",  ?_assert( 2.0 == sc_math:distance(-1.0,  1.0) ) },

        { "0.5 to 0.5",   ?_assert( 0.0 == sc_math:distance( 0.5,  0.5) ) },
        { "1.5 to 1.5",   ?_assert( 0.0 == sc_math:distance( 1.5,  1.5) ) },
        { "-1.5 to -1.5", ?_assert( 0.0 == sc_math:distance(-1.5, -1.5) ) },

        { "0.5 to 1.5",   ?_assert( 1.0 == sc_math:distance( 0.5,  1.5) ) },
        { "1.5 to 0.5",   ?_assert( 1.0 == sc_math:distance( 1.5,  0.5) ) },
        { "0.5 to -1.5",  ?_assert( 2.0 == sc_math:distance( 0.5, -1.5) ) },
        { "-1.5 to 0.5",  ?_assert( 2.0 == sc_math:distance(-1.5,  0.5) ) },
        { "1.5 to -1.5",  ?_assert( 3.0 == sc_math:distance( 1.5, -1.5) ) },
        { "-1.5 to 1.5",  ?_assert( 3.0 == sc_math:distance(-1.5,  1.5) ) },

        { "int distance is float",     ?_assert( 1.0 == sc_math:distance(1,   2  ) ) },
        { "float distance is float",   ?_assert( 1.0 == sc_math:distance(1.0, 2.0) ) },
        { "mixed distance 1 is float", ?_assert( 1.0 == sc_math:distance(1,   2.0) ) },
        { "mixed distance 2 is float", ?_assert( 1.0 == sc_math:distance(1.0, 2  ) ) }

    ] }.




eq_within_test_() ->

    { "eq_within", [

        { "true test",  ?_assert( true  == sc_math:eq_within(1.1, 1, 0.2)  ) },
        { "false test", ?_assert( false == sc_math:eq_within(1.1, 1, 0.05) ) }

    ] }.





gcd_test_() ->

    { "gcd", [

        { "8 12", ?_assert( 4 == sc_math:gcd(8,12) ) },
        { "12 8", ?_assert( 4 == sc_math:gcd(12,8) ) },
        { "1 3",  ?_assert( 1 == sc_math:gcd(1,3)  ) },
        { "7 9",  ?_assert( 1 == sc_math:gcd(7,9)  ) }

    ] }.





lcm_test_() ->

    { "lcm", [

        { "8 12", ?_assert( 24 == sc_math:lcm(8,12) ) },
        { "12 8", ?_assert( 24 == sc_math:lcm(12,8) ) },
        { "1 3",  ?_assert(  3 == sc_math:lcm(1,3)  ) },
        { "7 9",  ?_assert( 63 == sc_math:lcm(7,9)  ) }

    ] }.
