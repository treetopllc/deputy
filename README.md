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
{error, [<<"Must be less than 10.0">>]} =
    deputy:check(<<"40.0">>, Rules),
{error, [<<"Must be a floating point number">>]} =
    deputy:check(<<"abcd">>, Rules),
```

Proplist Usage
--------------

```erlang
Rules = [{<<"name">> , [{{regexp, <<"[a-zA-Z0-9]+">>}, <<"Must contain only alphanumerical characters">>}]}],
Attributes = [{<<"name">>, <<"##BatMan##">>}],
{error, [{<<"name">>, [<<"Must...">>]}]} = deputy:check_proplist(Attributes, Rules, []),
```

Addon Type Conversion and Validation
------------------------------------

```erlang
DateConvert = fun(Date) ->
    DateStr = binary_to_list(Date),
    case io_lib:fread("~4d-~2d-~2d", Date) of
        {ok, [Year, Month, Day]} ->
            {ok, {Year, Month, Day}};
        _ ->
            stop
    end.

CurrentDate = 
MinAge = 18,
%% bad age checker
CheckAge = fun(Date) ->
  Days = calendar:date_to_gregorian(Date),
  {Today, _} = calendar:local_time(),
  TodayDays = calendar:date_to_gregorian(Today),
  Age = (TodayDays - Days)/365.0,
  case Age > MinAge ->
      true -> ok
      false -> error
  end,

Rules = [{<<"birthday">>,
  [{{func, DateConvert}, <<"Must be a valid datetime string.">>},
   {{func, CheckAge}, <<"Must be at least 18 years old.">>}]}].
```
