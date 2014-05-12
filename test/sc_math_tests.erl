
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

        { "int distance is float",     ?_assert( 1.0 == sc_math:distance(1,   2  ) ) },
        { "float distance is float",   ?_assert( 1.0 == sc_math:distance(1.0, 2.0) ) },
        { "mixed distance 1 is float", ?_assert( 1.0 == sc_math:distance(1,   2.0) ) },
        { "mixed distance 2 is float", ?_assert( 1.0 == sc_math:distance(1.0, 2  ) ) }

    ] }.