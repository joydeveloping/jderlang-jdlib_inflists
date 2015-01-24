%% @doc
%% Tests for jdlib_inflists.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib_inflists_test).

% Unit-test using.
-include_lib("eunit/include/eunit.hrl").

% Functions import.
-import(jdlib_inflists,
        [repeat/1, cycle/1, seq/2, seq/1, geometric_series/2,
         head/1, tail/1, ht/1,
         take/2, nth/2, drop/2, nthtail/2, sublist/2, sublist/3, split/2,
         map/2]).

%---------------------------------------------------------------------------------------------------
% Tests.
%---------------------------------------------------------------------------------------------------

-spec repeat_test() -> ok.
%% @doc
%% Function repeat test.
repeat_test() ->
    L = repeat(a),
    L1 = tail(L),
    L2 = tail(L1),
    L3 = tail(L2),
    L4 = tail(L3),
    ?assertEqual({a, a, a, a, a}, {head(L), head(L1), head(L2), head(L3), head(L4)}),
    ok.

%---------------------------------------------------------------------------------------------------

-spec cycle_test() -> ok.
%% @doc
%% Function cycle test.
cycle_test() ->
    ?assertThrow({badarg, []}, cycle([])),
    L = cycle([a, b]),
    L1 = tail(L),
    L2 = tail(L1),
    L3 = tail(L2),
    L4 = tail(L3),
    ?assertEqual({a, b, a, b, a}, {head(L), head(L1), head(L2), head(L3), head(L4)}),
    ok.

%---------------------------------------------------------------------------------------------------

-spec seq_test() -> ok.
%% @doc
%% Function seq test.
seq_test() ->
    ?assertEqual([5, 7, 9], take(drop(seq(1, 2), 2), 3)),
    ?assertEqual([10, 11, 12, 13, 14], take(drop(seq(1), 9), 5)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec geometric_series_test() -> ok.
%% @doc
%% Function geometric_series test.
geometric_series_test() ->
    ?assertEqual([12, 24, 48], take(drop(geometric_series(3, 2), 2), 3)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec take_drop_test() -> ok.
%% @doc
%% Function take test.
take_drop_test() ->
    IL1 = repeat(a),
    IL2 = cycle([1, 2, 3]),
    ?assertEqual([a, a, a, a, a], take(drop(IL1, 5), 5)),
    ?assertEqual([3, 1, 2, 3, 1], take(drop(IL2, 5), 5)),
    ?assertEqual(a, nth(IL1, 5)),
    ?assertEqual(2, nth(IL2, 5)),
    ?assertMatch({[1, 2, 3, 1], _}, split(IL2, 4)),
    ?assertEqual([3, 1, 2], sublist(IL2, 3, 3)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec map_test() -> ok.
%% @doc
%% Function map test.
map_test() ->
    IL1 = seq(1, 2),
    IL2 = map(IL1, fun(E) -> E + 10 end),
    ?assertEqual([11, 13, 15, 17, 19], take(IL2, 5)),
    IL3 = geometric_series(1, 2),
    IL4 = map(IL3, fun(E) -> E - 1 end),
    ?assertEqual([3, 7, 15], sublist(IL4, 3, 3)),
    ok.

%---------------------------------------------------------------------------------------------------

