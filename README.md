Deputy
======

Deputy is meant to compliment github.com/extend/sheriff in that
it provides a way to convert and validate many common strings,
numerical values, and property lists containing simple attribute
name => value mappings which don't necessarily map to erlang types easily
or well.

It also is meant to be useful in providing error reporting back to the
requestor which sheriff does not do and may never do.

In essence its meant to be user input processing from things like json
and query string encoded data. You may need to transform, check rules,
and report back errors. These things are not easy to do directly with a
functional language unless you do it declaratively!


Single Simple Value Usage
-------------------------

```erlang
Rules = [{{convert, float}, <<"Must be a floating point number">>}, 
         {{'<', 10.0}, <<"Must be less than 10.0">>}],
{ok, 5.5} = deputy:check(<<"5.5">>, Rules),
{error, [{{'<', 10.0}, <<"Must be less than 10.0">>}]} =
    deputy:check(<<"40.0">>, Rules),
{error, [{{convert, float}, <<"Must be a floating point number">>}]} =
    deputy:check(<<"abcd">>, Rules),
```

Proplist Rule Usage
-------------------

```erlang
RuleSet = [{<<"name">> , [{{regexp, <<"[a-zA-Z0-9]+">>}, <<"Must contain only alphanumerical characters">>}]}],
Attributes = [{<<"name">>, <<"##BatMan##">>}],
{error, [{<<"name">>, [...]}]} = deputy:check(Attributes, RuleSet),
```
