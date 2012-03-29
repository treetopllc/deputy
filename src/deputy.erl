%% Copyright (c) 2012, Treetop Software LLC
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2012 Treetop Software LLC
%% @doc Declarative Type Conversion and Rule Checking.
%% @end

-module(deputy).

%% simple api
-export([check_proplist/2, check/2]).

%% advanced api which check/2 is built on.
-export([convert/2, check_rule/2, check/3, check_proplist/4]).

%% types
-type convert() :: boolean | integer | float | number.
-type convert_result() :: boolean() | number().
-type rule_fun() :: fun((term()) -> ok | {ok, term()} | error | stop).
-type rule() :: {convert, convert()} | {regexp, binary()}
    | {in, list()}
    | {'<', number()} | {'>', number()} | {'<=', number()} | {'>=', number()}
    | {min_size, pos_integer()} | {max_size, pos_integer()}
    | {size, pos_integer()}
    | {func, {atom(), atom()} | rule_fun()}.
-type rules() :: list({rule(), binary()}).
-export_type([convert/0, convert_result/0, rule_fun/0, rule/0, rules/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% ----------------------------------------------------------------------------
%% simple api
%% ----------------------------------------------------------------------------

%% @doc Check a value against a set of rules. Errors are accumulated.
%% Rules are iterated over in order.
%% @end
-spec check(term(), rules()) -> {ok, term()} | {error, list(binary())}.
check(Value, Rules) ->
    case check(Value, Rules, []) of
        {Value0, []} ->
            {ok, Value0};
        {_, Errors} ->
            {error, Errors}
    end.

%% @doc Check a property list against a set of rules for each property. Assumes
%% there's a ruleset for every value in the proplist and that no extra
%% values are present in the proplist.
%% @end
-spec check_proplist(list({binary(), term()}), list({binary(), rules()})) ->
    {ok, list({binary(), term()})} | {error, list({binary(), list(binary())})}.
check_proplist(Values, Rules) ->
    Values0 = lists:keysort(1, Values),
    Rules0 = lists:keysort(1, Rules),
    case check_proplist(Values0, Rules0, [], []) of
        {Values1, []} ->
            {ok, Values1};
        {_, Errors} ->
            {error, Errors}
    end.


%% ----------------------------------------------------------------------------
%% advanced api
%% ----------------------------------------------------------------------------

%% @doc Try to convert a value to a desired type.
-spec convert(binary(), convert()) -> convert_result() | error.
convert(Value, boolean) when is_boolean(Value) ->
    Value;
convert(Value, boolean) when is_binary(Value), Value == <<"true">> ->
    true;
convert(Value, boolean) when is_binary(Value), Value == <<"false">> ->
    false;
convert(Value, integer) when is_integer(Value) ->
    Value;
convert(Value, integer) when is_binary(Value) ->
    Value0 = binary_to_list(Value),
    try
        list_to_integer(Value0)
    catch
        error:badarg ->
            error
    end;
convert(Value, float) when is_float(Value) ->
    Value;
convert(Value, float) when is_binary(Value) ->
    Value0 = binary_to_list(Value),
    try
        list_to_float(Value0)
    catch
        error:badarg ->
            error
    end;
convert(Value, number) when is_number(Value) ->
    Value;
convert(Value, number) when is_binary(Value) ->
    Value0 = binary_to_list(Value),
    try
        list_to_number(Value0)
    catch
        error:badarg ->
            error
    end;
convert(_, _) ->
    error.

%% @doc Check rules accumulating errors.
check(Value, [], Errors) ->
    {Value, lists:reverse(Errors)};
check(Value, [{Rule, Message} | Rules], Errors) ->
    case check_rule(Value, Rule) of
        ok ->
            check(Value, Rules, Errors);
        {ok, Value0} ->
            check(Value0, Rules, Errors);
        error ->
            check(Value, Rules, [Message | Errors]);
        stop ->
            {error, [Message | Errors]}
    end.

%% @doc Check proplist rules accumulating errors.
check_proplist([], [], Results, Errors) ->
    {Results, Errors};
check_proplist([{Key, Value} | Values], [{Key, Rules0} | Rules], Results, Errors) ->
    {Value0, Errors0} = check(Value, Rules0, []),
    case Errors0 of
        [] ->
            check_proplist(Values, Rules, [{Key, Value0} | Results],
                Errors);
        _ ->
            check_proplist(Values, Rules, [{Key, Value0} | Results],
                [{Key, Errors0} | Errors])
    end.

%% @private
%% @doc Check built in rules.
check_rule(Value, {convert, Type}) when is_atom(Type) ->
    case convert(Value, Type) of
        error ->
            stop;
        Value0 ->
            {ok, Value0}
    end;
check_rule(Value, {regexp, Regexp}) when is_binary(Value), is_binary(Regexp) ->
    RegexpStr = binary_to_list(Regexp),
    ValueStr = binary_to_list(Value),
    case re:run(ValueStr, RegexpStr, []) of
        {match, _} ->
            ok;
        nomatch ->
            error
    end;
check_rule(Value, {in, Set}) when is_list(Set) ->
    case lists:member(Value, Set) of
        true ->
            ok;
        false ->
            error
    end;
check_rule(Value, {'<', Max})  when is_number(Value), is_number(Max) ->
    case Value < Max of
        true ->
            ok;
        false ->
            error
    end;
check_rule(Value, {'=<', Max})  when  is_number(Value), is_number(Max) ->
    case Value =< Max of
        true ->
            ok;
        false ->
            error
    end;
check_rule(Value, {'>', Min}) when is_number(Value), is_number(Min) ->
    case Value > Min of
        true ->
            ok;
        false ->
            error
    end;
check_rule(Value, {'>=', Min}) when is_number(Value), is_number(Min) ->
    case Value >= Min of
        true ->
            ok;
        false ->
            error
    end;
check_rule(Value, {max_size, Size}) when is_binary(Value), is_integer(Size) ->
    case Size >= 0 andalso byte_size(Value) =< Size of
        true ->
            ok;
        false ->
            error
    end;
check_rule(Value, {min_size, Size}) when is_binary(Value), is_integer(Size) ->
    case Size >= 0 andalso byte_size(Value) >= Size of
        true ->
            ok;
        false ->
            error
    end;
check_rule(Value, {size, Size}) when is_binary(Value), is_integer(Size) ->
    case byte_size(Value) == Size of
        true->
            ok;
        false ->
            error
    end;
check_rule(Value, {func, {Module, Function}}) when is_atom(Module),
        is_atom(Function) ->
    Module:Function(Value);
check_rule(Value, {func, Fun}) when is_function(Fun, 1) ->
    Fun(Value);
check_rule(_, _) ->
    error.


%% ----------------------------------------------------------------------------
%% private
%% ----------------------------------------------------------------------------

%% @private
%% @doc List to number conversion.
list_to_number(Str) ->
    try list_to_float(Str)
    catch error:badarg ->
        list_to_integer(Str)
    end.


%% ------------------------------------------------------------------
%% unit tests
%% ------------------------------------------------------------------

-ifdef(TEST).

list_to_number_test_() ->
    [?_assertEqual(5, list_to_number("5")),
     ?_assertEqual(5.0, list_to_number("5.0")),
     ?_assertException(error, badarg, list_to_number("abc"))
    ].

convert_boolean_test_() ->
    [?_assertEqual(true, convert(true, boolean)),
     ?_assertEqual(true, convert(<<"true">>, boolean)),
     ?_assertEqual(false, convert(<<"false">>, boolean)),
     ?_assertEqual(error, convert(<<"????">>, boolean))].

convert_integer_test_() ->
    [?_assertEqual(1, convert(1, integer)),
     ?_assertEqual(1, convert(<<"1">>, integer)),
     ?_assertEqual(error, convert(<<"1.1">>, integer)),
     ?_assertEqual(error, convert(<<"a">>, integer))].

convert_float_test_() ->
    [?_assertEqual(1.2, convert(1.2, float)),
     ?_assertEqual(1.2, convert(<<"1.2">>, float)),
     ?_assertEqual(error, convert(<<"a">>, float))].

convert_number_test_() ->
    [?_assertEqual(1, convert(1, number)),
     ?_assertEqual(1.2, convert(1.2, number)),
     ?_assertEqual(1.2, convert(<<"1.2">>, number)),
     ?_assertEqual(1, convert(<<"1">>, number)),
     ?_assertEqual(error, convert(<<"a">>, number))].

rule_convert_test_() ->
    [?_assertEqual({ok, 1}, check_rule(<<"1">>, {convert, number})),
     ?_assertEqual(stop, check_rule(<<"a">>, {convert, number}))].

rule_regexp_test_() ->
    [?_assertEqual(ok, check_rule(<<"a">>, {regexp, <<"^a">>})),
     ?_assertEqual(error, check_rule(<<"b">>, {regexp, <<"^a">>}))].

rule_in_test_() ->
    [?_assertEqual(ok, check_rule(<<"a">>, {in, [<<"a">>, <<"b">>]})),
     ?_assertEqual(error, check_rule(<<"b">>, {in, [<<"a">>, <<"c">>]}))].

rule_lt_test_() ->
    [?_assertEqual(ok, check_rule(9, {'<', 10})),
     ?_assertEqual(error, check_rule(11, {'<', 10}))].

rule_lt_eq_test_() ->
    [?_assertEqual(ok, check_rule(9, {'=<', 10})),
     ?_assertEqual(ok, check_rule(10, {'=<', 10})),
     ?_assertEqual(ok, check_rule(9.2, {'=<', 10.0})),
     ?_assertEqual(error, check_rule(11, {'=<', 10}))].

rule_gt_test_() ->
    [?_assertEqual(ok, check_rule(11, {'>', 10})),
     ?_assertEqual(error, check_rule(9, {'>', 10}))].

rule_gt_eq_test_() ->
    [?_assertEqual(ok, check_rule(11, {'>=', 10})),
     ?_assertEqual(ok, check_rule(10, {'>=', 10})),
     ?_assertEqual(ok, check_rule(10.1, {'>=', 10.0})),
     ?_assertEqual(error, check_rule(9, {'>=', 10}))].

rule_min_size_test_() ->
    [?_assertEqual(ok, check_rule(<<"ab">>, {min_size, 2})),
     ?_assertEqual(ok, check_rule(<<"abc">>, {min_size, 2})),
     ?_assertEqual(error, check_rule(<<"a">>, {min_size, 2}))].

rule_max_size_test_() ->
    [?_assertEqual(ok, check_rule(<<"a">>, {max_size, 2})),
     ?_assertEqual(ok, check_rule(<<"ab">>, {max_size, 2})),
     ?_assertEqual(error, check_rule(<<"abc">>, {max_size, 2}))].

rule_size_test_() ->
    [?_assertEqual(ok, check_rule(<<"a">>, {size, 1})),
     ?_assertEqual(error, check_rule(<<"b">>, {size, 2}))].

rule_func_test_() ->
    [?_assertEqual(ok, check_rule(<<"a">>, {func, fun(Val) -> ok end})),
     ?_assertEqual(error, check_rule(<<"b">>, {func, fun(Val) -> error end}))].

check_test_() ->
    FloatMsg = <<"Must be a floating point number.">>,
    LtMsg = <<"Must be less than 4.0">>,
    InMsg = <<"Must be 2.0">>,
    Rules = [{{convert, float}, FloatMsg},
             {{'<', 4.0}, LtMsg},
             {{in, [2.0]}, InMsg}],
    [?_assertEqual({ok, 2.0}, check(<<"2.0">>, Rules)),
     ?_assertEqual({error, [FloatMsg]}, check(<<"a">>, Rules)),
     ?_assertEqual({error, [LtMsg, InMsg]}, check(<<"4.1">>, Rules))].

check_proplist_test_() ->
    FloatMsg = <<"Must be a floating point number">>,
    LtMsg = <<"Must be less than 200.0">>,
    GtMsg = <<"Must be greater than -100.0">>,
    MinSizeMsg = <<"Must be at least 5 characters">>,
    Rules = [
        {<<"temperature">>, [{{convert, float}, FloatMsg},
                {{'<', 200.0}, LtMsg}, {{'>', -100.0}, GtMsg}]},
        {<<"name">>, [{{min_size, 5}, MinSizeMsg}]}
    ],
    Values = [{<<"name">>, <<"Johnny">>}, {<<"temperature">>, <<"98.7">>}],
    BadValues = [{<<"name">>, <<"Tom">>}, {<<"temperature">>, <<"1000.0">>}],
    GoodResults = lists:keysort(1, [{<<"temperature">>, 98.7}, {<<"name">>, <<"Johnny">>}]),
    BadResults = lists:keysort(1, [{<<"temperature">>, [LtMsg]}, {<<"name">>, [MinSizeMsg]}]),
    {ok, GoodResults0} = check_proplist(Values, Rules),
    {error, BadResults0} = check_proplist(BadValues, Rules),
    GoodResults1 = lists:keysort(1, GoodResults0),
    BadResults1 = lists:keysort(1, BadResults0),
    [?_assertEqual(GoodResults, GoodResults1),
     ?_assertEqual(BadResults, BadResults1)].


-endif.
