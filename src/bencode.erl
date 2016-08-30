-module( bencode ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-export( [encode/1, decode/1] ).
-type value() :: binary() | integer() | list() | map().

%%
%%  Encode a list of erlang terms to a binary string using bencoding
%%
-spec encode( value() ) -> binary().
encode( Val ) ->
    encode( Val, [] ).

-spec encode( [value()], [any()] ) -> binary().
encode( [Val | T], Out ) when is_list( Val ) ->
    encode( T, [[$l, encode( Val ), $e] | Out] );

encode( [Val | T], Out ) when is_binary( Val ) ->
    encode( T, [[want:string( byte_size( Val ) ), $:, Val] | Out ] );

encode( [Val | T], Out ) when is_integer( Val ) or is_float( Val ) ->
    encode( T, [[$i, want:string( Val ), $e] | Out] );

encode( [Val | T], Out ) when is_map( Val ) ->
    encode( T, [[$d, lists:flatten( lists:foldl( fun( { Key, Value }, D ) -> [encode( [Key] ), encode( [Value] ) | D] end, [], maps:to_list( Val ) ) ), $e] | Out ] );

encode( [], Out ) ->
    want:binary( lists:flatten( lists:reverse( Out ) ) ).

%%
%%  Given a binary string describing bencoded values, decode the string into a list of erlang terms
%%
-spec decode( binary() ) -> [value()].
decode( Val ) when is_binary( Val ) ->
    decode( want:string( Val ), [] ).

-spec decode( [any()], [any()] ) -> [value()].
decode( [$i | T], Out ) ->
    { Rest, Integer } = decode_integer( T, $e ),
    decode( Rest, [Integer | Out] );

decode( [$l | T], Out ) ->
    { Rest, List } = decode_list( T ),
    decode( Rest, [ List | Out ] );

decode( [$e | T], Out ) ->
    { T, lists:reverse( Out ) };

decode( [$d | T], Out ) ->
    { Rest, Dict } = decode_dict( T ),
    decode( Rest, [Dict | Out] );

decode( Value = [_StringStart | _], Out ) ->
    { Rest, String } = decode_string( Value ),
    decode( Rest, [ String | Out ] );

decode( [], Out ) ->
    lists:reverse( Out ).

decode_string( Value ) ->
    { Rest, Length } = decode_integer( Value, $: ),
    { lists:nthtail( Length, Rest ), want:binary( lists:sublist( Rest, Length ) ) }.

decode_list( Value ) ->
    decode( Value, [] ).

decode_dict( T ) ->
    { Rest, List } = decode_list( T ),
    { _, PropList } = lists:foldl( fun( Key, { key, PropList } ) -> { value, [Key | PropList] }; 
                                        ( Value, { value, [Key | PropList] } ) -> { key, [{ Key, Value } | PropList] } end,
                                        { key, [] },
                                        List ),
    { Rest, maps:from_list( lists:reverse( PropList ) ) }.

decode_integer( Value, EndDelimiter ) ->
    decode_integer( Value, [], EndDelimiter ).

decode_integer( [ EndDelimiter | T ], Value, EndDelimiter ) ->
    { T, want:integer( lists:reverse( Value ) ) };

decode_integer( [ Char | T ], Value, EndDelimiter ) ->
    decode_integer( T, [ Char | Value ], EndDelimiter ).

