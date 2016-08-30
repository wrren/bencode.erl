-module( bencode_tests ).
-author( "Warren Kenny <warren.kenny@gmail.com.com>" ).
-include_lib( "eunit/include/eunit.hrl" ).

decode_test() ->
    ?assertMatch( [1, 2, 3],                            bencode:decode( <<"i1ei2ei3e">> ) ),
    ?assertMatch( [1, [<<"Hello">>], 2],                bencode:decode( <<"i1el5:Helloei2e">> ) ),
    ?assertMatch( [#{ <<"Hello">> := <<"World">> }],    bencode:decode( <<"d5:Hello5:Worlde">> ) ).

encode_test() ->
    ?assertMatch( <<"i1ei2ei3e">>,                      bencode:encode( [1, 2, 3] ) ),
    ?assertMatch( <<"i1el5:Helloei2e">>,                bencode:encode( [1, [<<"Hello">>], 2] ) ),
    ?assertMatch( <<"d5:Hello5:Worlde">>,               bencode:encode( [#{ <<"Hello">> => <<"World">> }] ) ).

encode_decode_test() ->
	?assertMatch( [1, 2, 3], 							bencode:decode( bencode:encode( [1, 2, 3] ) ) ),
	?assertMatch( [1, [<<"Hello">>], 2],                bencode:decode( bencode:encode( [1, [<<"Hello">>], 2] ) ) ),
    ?assertMatch( [#{ <<"Hello">> := <<"World">> }],    bencode:decode( bencode:encode( [#{ <<"Hello">> => <<"World">> }] ) ) ).