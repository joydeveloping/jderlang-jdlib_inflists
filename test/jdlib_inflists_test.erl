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
        [iterate/3, iterate/2,
         repeat/1, cycle/1, seq/2, odds/0, evens/0, seq/1, naturals/0, naturals/1,
         geometric_series/2,
         fib/0, harmonic_series/0, anharmonic_series/0, grundy_series/0,
         head/1, tail/1, ht/1,
         take/2, nth/2, drop/2, nthtail/2, sublist/2, sublist/3, split/2,
         zip/2, zip_3/3, zipwith/3, unzip/1, unzip_3/1,
         map/2, adj_pairs_map/2, mapfold/3,
         add/2, sub/2, neg/1, mul/2, dvs/2, inv/1, square/1, sqrt/1, pow/2, sum/1, product/1,
         dirichlet_series/1, dirichlet_series/2,
         sparse/2, odds/1, evens/1, merge/2, sign_alternate/1, avg/1]).

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
    ?assertEqual([1, 3, 5, 7, 9], take(odds(), 5)),
    ?assertEqual([2, 4, 6, 8, 10], take(evens(), 5)),
    ?assertEqual([10, 11, 12, 13, 14], take(drop(seq(1), 9), 5)),
    ?assertEqual([1, 2, 3, 4, 5], take(naturals(), 5)),
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

-spec zip_test() -> ok.
%% @doc
%% Zip two infinite lists.
zip_test() ->
    IL1 = seq(3, 4),
    IL2 = geometric_series(2, 2),
    IL3 = zip(IL1, IL2),
    ?assertEqual([{3, 2}, {7, 4}, {11, 8}, {15, 16}, {19, 32}], take(IL3, 5)),
    IL4 = cycle([a, b, c]),
    IL5 = zip_3(IL4, IL2, IL1),
    ?assertEqual([{c, 8, 11}, {a, 16, 15}], sublist(IL5, 3, 2)),
    IL6 = zipwith(IL1, IL2, fun(E1, E2) -> E1 + E2 end),
    ?assertEqual([5, 11, 19], take(IL6, 3)),
    {UL1, UL2} = unzip(IL3),
    ?assertEqual([3, 7, 11], take(UL1, 3)),
    ?assertEqual([2, 4, 8], take(UL2, 3)),
    {UL3, UL4, UL5} = unzip_3(IL5),
    ?assertEqual([b, c, a], sublist(UL3, 2, 3)),
    ?assertEqual([4, 8, 16], sublist(UL4, 2, 3)),
    ?assertEqual([7, 11, 15], sublist(UL5, 2, 3)),
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
    IL5 = seq(1),
    IL6 = adj_pairs_map(IL5, fun(X, Y) -> X + Y end),
    ?assertEqual([7, 9, 11], sublist(IL6, 3, 3)),
    IL7 = adj_pairs_map(IL5, fun(X, Y) -> Y - X end),
    ?assertEqual([1, 1, 1, 1, 1], sublist(IL7, 5, 5)),
    ?assertEqual([1, 3, 6, 10, 15], take(sum(seq(1)), 5)),
    ?assertEqual([1, 2, 6, 24, 120], take(product(seq(1)), 5)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec mapfold_test() -> ok.
%% @doc
%% Function mapfold test.
mapfold_test() ->
    IL1 = seq(1),
    IL2 = mapfold(IL1, fun(X, Y) -> X + Y end, 0),
    ?assertEqual([1, 3, 6, 10, 15], take(IL2, 5)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec math_test() -> ok.
%% @doc
%% Function add, sub, mul, dvs, square, sqrt, pow.
math_test() ->
    IL1 = seq(2),
    IL2 = geometric_series(2, 2),
    ?assertEqual([4, 7, 12], take(add(IL1, IL2), 3)),
    ?assertEqual([3, 4, 5], take(add(IL1, 1), 3)),
    ?assertEqual([3, 5, 9], take(add(1, IL2), 3)),
    ?assertThrow({badarg, _}, add(1, 2)),
    ?assertEqual([0, -1, -4], take(sub(IL1, IL2), 3)),
    ?assertEqual([-3, -2, -1], take(sub(IL1, 5), 3)),
    ?assertEqual([1, -1, -5], take(sub(3, IL2), 3)),
    ?assertThrow({badarg, _}, sub(1, 2)),
    ?assertEqual([4, 12, 32], take(mul(IL1, IL2), 3)),
    ?assertEqual([10, 15, 20], take(mul(IL1, 5), 3)),
    ?assertEqual([6, 12, 24], take(mul(3, IL2), 3)),
    ?assertThrow({badarg, _}, mul(1, 2)),
    ?assertEqual([2.0, 3.0, 4.0], take(sqrt(square(IL1)), 3)),
    ?assertEqual([1.0, 8.0, 27.0, 64.0, 125.0], take(pow(seq(1), 3), 5)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec sparse_test() -> ok.
%% @doc
%% Function sparse test.
sparse_test() ->
    IL1 = seq(1),
    IL2 = sparse(IL1, 0),
    IL3 = sparse(IL1, 1),
    IL4 = sparse(IL1, 2),
    ?assertThrow({badarg, _}, sparse(IL1, -1)),
    ?assertEqual([1, 2, 3], take(IL2, 3)),
    ?assertEqual([1, 3, 5], take(IL3, 3)),
    ?assertEqual([1, 4, 7], take(IL4, 3)),
    ?assertEqual([a, a, a, a, a], take(odds(cycle([a, b])), 5)),
    ?assertEqual([b, b, b, b, b], take(evens(cycle([a, b])), 5)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec merge_test() -> ok.
%% @doc
%% Function merge test.
merge_test() ->
    IL1 = seq(1),
    IL2 = cycle([a, b, c]),
    IL3 = merge(IL1, IL2),
    ?assertEqual([1, a, 2, b, 3, c, 4, a], take(IL3, 8)),
    ok.

%---------------------------------------------------------------------------------------------------

-spec other_test() -> ok.
%% @doc
%% Other functions test.
other_test() ->
    ?assertEqual([1, -2, 3, -4, 5], take(sign_alternate(naturals()), 5)),
    ?assertEqual([1, 1, 2, 3, 5, 8, 13], take(fib(), 7)),
    ?assertEqual([1, -1, 1, -1], take(grundy_series(), 4)),
    ok.

%---------------------------------------------------------------------------------------------------

