
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





prop_ceil_ints_as_floats_identity() ->

    ?FORALL( I,
             proper_types:integer(),

             sc_math:ceiling(I*1.0) =:= I

    ).





prop_ceil_floats_smaller_within_1() ->

    ?FORALL( R,
             proper_types:real(),

             (sc_math:ceiling(R) - R) < 1 andalso (sc_math:ceiling(R) - R) >= 0

           ).





prop_ceil_always_gives_integers() ->

    ?FORALL( N,
             proper_types:number(),

             is_integer(sc_math:ceiling(N))

           ).





ceil_test_() ->

    { "Ceil/Ceiling tests", [

        { "Manual value assertions", [

            { "0.5",  ?_assert(  1 =:= sc_math:ceiling(0.5)  ) },
            { "0",    ?_assert(  0 =:= sc_math:ceiling(0)    ) },
            { "0.0",  ?_assert(  0 =:= sc_math:ceiling(0.0)  ) },
            { "1.0",  ?_assert(  1 =:= sc_math:ceiling(1.0)  ) },
            { "-1.0", ?_assert( -1 =:= sc_math:ceiling(-1.0) ) },
            { "-1.5", ?_assert( -1 =:= sc_math:ceiling(-1.5) ) },
            { "-1",   ?_assert( -1 =:= sc_math:ceiling(-1)   ) },
            { "1",    ?_assert(  1 =:= sc_math:ceiling(1)    ) } 

        ] },

        { "Stochastic property assertions", [

            { "All integers-as-floats are identity", ?_assert( true =:= proper:quickcheck(prop_ceil_ints_as_floats_identity()) ) },
            { "All floats are smaller within 1",     ?_assert( true =:= proper:quickcheck(prop_ceil_floats_smaller_within_1()) ) },
            { "All numbers give integer results",    ?_assert( true =:= proper:quickcheck(prop_ceil_always_gives_integers())   ) }

        ] }

    ] }.





prop_floor_ints_as_floats_identity() ->

    ?FORALL( I,
             proper_types:int(),

             sc_math:floor(I*1.0) =:= I

           ).





prop_floor_floats_larger_within_1() ->

    ?FORALL( R,
             proper_types:real(),

             (R - sc_math:floor(R)) < 1 andalso (R - sc_math:floor(R)) >= 0

           ).





prop_floor_always_gives_integers() ->

    ?FORALL( N,
             proper_types:number(),

             is_integer(sc_math:floor(N))

           ).





floor_test_() ->

    { "Floor tests", [

        { "Manual value assertions", [

            {"0.5",  ?_assert(  0 =:= sc_math:floor(0.5)  ) },
            {"0",    ?_assert(  0 =:= sc_math:floor(0)    ) },
            {"0.0",  ?_assert(  0 =:= sc_math:floor(0.0)  ) },
            {"1.0",  ?_assert(  1 =:= sc_math:floor(1.0)  ) },
            {"-1.0", ?_assert( -1 =:= sc_math:floor(-1.0) ) },
            {"-1.5", ?_assert( -2 =:= sc_math:floor(-1.5) ) },
            {"-1",   ?_assert( -1 =:= sc_math:floor(-1)   ) },
            {"1",    ?_assert(  1 =:= sc_math:floor(1)    ) }

        ] },

        { "Stochastic property assertions", [

            {"All integers-as-floats are identity", ?_assert( true =:= proper:quickcheck(prop_floor_ints_as_floats_identity()) ) },
            {"All floats are larger within 1",      ?_assert( true =:= proper:quickcheck(prop_floor_floats_larger_within_1())  ) },
            {"All numbers give integer results",    ?_assert( true =:= proper:quickcheck(prop_floor_always_gives_integers())   ) }

        ] }

    ] }.




    
prop_extrema_min_max_are_members() ->

    ?FORALL( L,
             non_empty(list(proper_types:any())),

             case sc:extrema(L) of {Min,Max} -> {true,true} =:= {lists:member(Min,L),lists:member(Max,L)}; _ -> false end

           ).





extrema_test_() ->

    { "Extrema tests", [

        {"8,6,7,5,3,0,9",                           ?_assert( {0,9}      =:= sc:extrema( [8,6,7,5,3,0,9] ) ) },
        {"1,2,3,4",                                 ?_assert( {1,4}      =:= sc:extrema( [1,2,3,4]       ) ) },
        {"-1,-2,-3",                                ?_assert( {-3,-1}    =:= sc:extrema( [-1,-2,-3]      ) ) },
        {"-1.1,0,1.1",                              ?_assert( {-1.1,1.1} =:= sc:extrema( [-1.1,1.1]      ) ) },
        {"a,b,c",                                   ?_assert( {a,c}      =:= sc:extrema( [a,b,c]         ) ) },
        {"1,a,{}",                                  ?_assert( {1,{}}     =:= sc:extrema( [1,a,{}]        ) ) },
        {"1",                                       ?_assert( {1,1}      =:= sc:extrema( [1]             ) ) },
        {"1,2,3,a,b,c",                             ?_assert( {1,c}      =:= sc:extrema( [1,2,3,a,b,c]   ) ) },

        {"[] error undefined",                      ?_assertError(function_clause, sc:extrema([]) ) },

        {"Stochastic: min/max are members",         ?_assert( true =:= proper:quickcheck(prop_extrema_min_max_are_members()) ) },
        {"Stochastic error: not a list type error", ?_assertError(function_clause, sc:extrema([]) ) }

    ] }.