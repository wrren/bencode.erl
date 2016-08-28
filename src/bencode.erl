-module( bencode ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-export( [encode/1, decode/1] ).


encode( Val ) when is_integer( Val ) ->
    <<$i, Val/binary, $e>>.

decode( Val ) when is_binary( Val ) ->
    decode( want:list( Val ), [] ).

decode( [$i | T], Out ) ->
    { Rest, Integer } = decode_integer( T, $e ),
    decode( Rest, [Integer | Out] );

decode( [$l | T], Out ) ->
    { Rest, List } = decode_list( T ),
    decode( Rest, [ List | Out ] );

decode( [$e | T], Out ) ->
    { lists:reverse( Out ), T };

decode( Value = [_StringStart | _], Out ) ->
    { Rest, String } = decode_string( Value ),
    decode( Rest, [ String | Out ] ).

decode_string( Value ) ->
    { Rest, Length } = decode_integer( Value, $: ),
    { lists:nthtail( Length, Rest ), lists:sublist( Rest, Length ) }.

decode_list( Value ) ->
    decode( Value, [] ).

decode_integer( Value, EndDelimiter ) ->
    decode_integer( Value, [], EndDelimiter ).

decode_integer( [ EndDelimiter | T ], Value, EndDelimiter ) ->
    { T, want:integer( lists:reverse( Value ) ) };

decode_integer( [ Char | T ], Value, EndDelimiter ) ->
    decode_integer( T, [ Char | Value ], EndDelimiter ).

