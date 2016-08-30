# bencode.erl

[Bencode](https://en.wikipedia.org/wiki/Bencode) Encode/Decode library for Erlang

=====

An OTP library

Build
-----

    $ rebar3 compile


Test
-----

    $ rebar3 eunit


Install
-------

Add to rebar.config:

```erlang
{ deps, [
	{ bencode, ".*", { git, "git://github.com/wrren/bencode.erl.git", { branch, "master" } } }
] }.
```

Use
---

```erlang

[1, 2, 3]                         =   bencode:decode( <<"i1ei2ei3e">> ),
[1, [<<"Hello">>], 2]             =   bencode:decode( <<"i1el5:Helloei2e">> ),
[#{ <<"Hello">> := <<"World">> }] =   bencode:decode( <<"d5:Hello5:Worlde">> ),

<<"i1ei2ei3e">>,                  =   bencode:encode( [1, 2, 3] ),
<<"i1el5:Helloei2e">>,            =   bencode:encode( [1, [<<"Hello">>], 2] ),
<<"d5:Hello5:Worlde">>,           =   bencode:encode( [#{ <<"Hello">> => <<"World">> }] ),


```