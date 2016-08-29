-module( bencode_tests ).
-author( "Warren Kenny <warren.kenny@gmail.com.com>" ).
-include_lib( "eunit/include/eunit.hrl" ).

decode_test() ->
    [1, 2, 3] = bencode:decode( <<"i1ei2ei3e">> ).