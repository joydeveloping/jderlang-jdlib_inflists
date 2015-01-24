%% @doc
%% Infinite lists realization.
%%
%% Copyright Joy Developing.

% Module name.
-module(jdlib_inflists).

% Export.
-export([iterate/3, iterate/2,
         repeat/1, cycle/1, seq/2, seq/1, geometric_series/2,
         head/1, tail/1, ht/1,
         take/2, nth/2, drop/2, nthtail/2, sublist/2, sublist/3, split/2,
         map/2]).

%---------------------------------------------------------------------------------------------------
% Types.
%---------------------------------------------------------------------------------------------------

% Types export.
-export_type([inflist/0]).

% Define infinite list as record.
-record(inflist,
{
    h :: term(),
    acc :: term(),
    f :: fun((term(), term()) -> {term(), term()})
}).

% Inifinite list.
-type inflist() :: #inflist{}.

%---------------------------------------------------------------------------------------------------
% Infinite lists constructors.
%---------------------------------------------------------------------------------------------------

-spec iterate(H, Acc, F) -> inflist()
      when F :: fun((H, Acc) -> {H, Acc}),
           H :: term(),
           Acc :: term().
%% @doc
%% Create infinite list with head, accumulator and iterate function.
iterate(H, Acc, F) when is_function(F, 2) ->
    #inflist
    {
        h = H,
        acc = Acc,
        f = F
    };
iterate(_, _, F) ->
    throw({badarg, {F, wrong_arity}}).

%---------------------------------------------------------------------------------------------------

-spec iterate(H, F :: fun((H) -> H)) -> inflist()
      when H :: term().
%% @doc
%% Create infinite list with head and iterate function.
iterate(H, F) when is_function(F, 1) ->
    iterate
    (
        H,
        0,
        fun(Cur_H, _) ->
            {F(Cur_H), 0}
        end
    );
iterate(_, F) ->
    throw({badarg, {F, wrong_arity}}).

%---------------------------------------------------------------------------------------------------

-spec repeat(T :: term()) -> inflist().
%% @doc
%% Construct infinite list, containing one repeating element.
%% T -> [T, T, ..]
repeat(T) ->
    iterate
    (
        T,
        fun(_) ->
            T
        end
    ).

%---------------------------------------------------------------------------------------------------

-spec cycle(L :: list()) -> inflist().
%% @doc
%% Construct infinite list, containing infinite number of list L copies.
%% [E1, E2, E2] -> [E1, E2, E3, E1, E2, E3, ..]
cycle([]) ->
    throw({badarg, []});
cycle([H | T]) ->
    iterate
    (
        H,
        T,
        fun
            (_, []) ->
                {H, T};
            (_, [Cur_H | Cur_T]) ->
                {Cur_H, Cur_T}
        end
    ).

%---------------------------------------------------------------------------------------------------

-spec seq(From :: number(), Step :: number()) -> inflist().
%% @doc
%% Construct inflinite list [From, From + Step, From + 2 * Step, ..].
seq(From, Step) ->
    iterate
    (
        From,
        fun(H) ->
            H + Step
        end
    ).

%---------------------------------------------------------------------------------------------------

-spec seq(From :: number()) -> inflist().
%% @doc
%% Construct infinite list [From, From + 1, From + 2, ..].
seq(From) ->
    seq(From, 1).

%---------------------------------------------------------------------------------------------------

-spec geometric_series(Base :: number(), K :: number()) -> inflist().
%% @doc
%% Construct infinite list [Base, Base * K, Base * K^2, ..].
geometric_series(Base, K) ->
    iterate
    (
        Base,
        fun(H) ->
            H * K
        end
    ).

%---------------------------------------------------------------------------------------------------
% Take elements.
%---------------------------------------------------------------------------------------------------

-spec head(IL :: inflist()) -> term().
%% @doc
%% Head of infinite list.
head(IL) when is_record(IL, inflist) ->
    IL#inflist.h.

%---------------------------------------------------------------------------------------------------

-spec tail(IL :: inflist()) -> inflist().
%% @doc
%% Tail of infinite list.
tail(#inflist{h = H, acc = Acc, f = F} = IL) ->
    {New_H, New_Acc} = F(H, Acc),
    IL#inflist{h = New_H, acc = New_Acc}.

%---------------------------------------------------------------------------------------------------

-spec ht(IL :: inflist()) -> {term(), inflist()}.
%% @doc
%% Take head and tail simultaneously.
ht(IL) ->
    {head(IL), tail(IL)}.

%---------------------------------------------------------------------------------------------------

-spec take(IL :: inflist(), N :: integer()) -> list().
%% @doc
%% Take first elements of infinite list.
take(_, N) when (N < 0) ->
    throw({badarg, N});
take(IL, N) ->
    take(IL, N, []).

-spec take(IL :: inflist(), N :: integer(), [E]) -> [E]
      when E :: term().
%% @private
%% @doc
%% Take first elements of infinite list.
take(_, 0, R) ->
    lists:reverse(R);
take(IL, N, R) ->
    take(tail(IL), N - 1, [head(IL) | R]).

%---------------------------------------------------------------------------------------------------

-spec nth(IL :: inflist(), N :: integer()) -> term().
%% @doc
nth(_, N) when (N < 0) ->
    throw({badarg, N});
nth(IL, N) ->
    lists:last(take(IL, N)).

%---------------------------------------------------------------------------------------------------

-spec drop(IL :: inflist(), N :: integer()) -> inflist().
%% @doc
%% Drop first elements of infinite list.
drop(_, N) when (N < 0) ->
    throw({badarg, N});
drop(IL, 0) ->
    IL;
drop(IL, N) ->
    drop(tail(IL), N - 1).

%---------------------------------------------------------------------------------------------------

-spec nthtail(IL :: inflist(), N :: integer()) -> inflist().
%% @doc
%% Tail of infinite list without n elements.
nthtail(IL, N) ->
    drop(IL, N).

%---------------------------------------------------------------------------------------------------

-spec sublist(IL :: inflist(), N :: integer()) -> list().
%% @doc
%% Sublist from first position.
sublist(IL, N) ->
    take(IL, N).

%---------------------------------------------------------------------------------------------------

-spec sublist(IL :: inflist(), Start :: integer(), N :: integer()) -> list().
%% @doc
%% Sublist from given position.
sublist(IL, Start, N) ->
    take(drop(IL, Start - 1), N).

%---------------------------------------------------------------------------------------------------

-spec split(IL :: inflist(), N :: integer()) -> {list(), inflist()}.
%% @doc
%% Split infinite list by position.
split(_, N) when (N < 0) ->
    throw({badarg, N});
split(IL, N) ->
    split(IL, N, []).

-spec split(IL :: inflist(), N :: integer(), [E]) -> {[E], inflist()}
      when E :: term().
%% @doc
%% Split infinite list by position.
split(IL, 0, R) ->
    {lists:reverse(R), IL};
split(IL, N, R) ->
    split(tail(IL), N - 1, [head(IL) | R]).

%---------------------------------------------------------------------------------------------------
% Zip/unzip functions and functors.
%---------------------------------------------------------------------------------------------------

-spec map(IL :: inflist(), Map_F :: fun((term()) -> term())) -> inflist().
%% @doc
%% Apply function to every element of infinite list.
map(#inflist{h = H, acc = Acc, f = F}, Map_F) ->
    iterate
    (
        Map_F(H),
        {H, Acc},
        fun(_, {Cur_H, Cur_Acc}) ->
            {New_H, New_Acc} = F(Cur_H, Cur_Acc),
            {Map_F(New_H), {New_H, New_Acc}}
        end
    ).

%---------------------------------------------------------------------------------------------------

