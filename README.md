Deputy
======

Deputy is a declarative data conversion and validation library for Erlang.

It is meant to be useful for handling user input and reporting reasonably
useful messages back to a user.

For awesome -type based checking (which can certainly be used with this)
checkout github.com/extend/sheriff.

Simple Value Usage
------------------

```erlang
Rules = [{{convert, float}, <<"Must be a floating point number">>}, 
         {{'<', 10.0}, <<"Must be less than 10.0">>}],
{ok, 5.5} = deputy:check(<<"5.5">>, Rules),
{error, [{{'<', 10.0}, <<"Must be less than 10.0">>}]} =
    deputy:check(<<"40.0">>, Rules),
{error, [{{convert, float}, <<"Must be a floating point number">>}]} =
    deputy:check(<<"abcd">>, Rules),
```

Proplist Usage
--------------

```erlang
Rules = [{<<"name">> , [{{regexp, <<"[a-zA-Z0-9]+">>}, <<"Must contain only alphanumerical characters">>}]}],
Attributes = [{<<"name">>, <<"##BatMan##">>}],
{error, [{<<"name">>, [<<"Must...">>]}]} = deputy:check_proplist(Attributes, RuleSet, []),
```

Addon Type Conversion
---------------------

```erlang
binary_to_datetime(Datetime) ->
   ...
   {{Year, Month, Day}, {Hour, Min, Sec}}.

Rules = [{<<"datetime">>, 
