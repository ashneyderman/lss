% @author Alex Shneyderman <a.shneyderman@gmail.com>
% @copyright (C) 2011, Alex Shneyderman
% @since May 26, 2011
-module(binl).

-export([show_length/0,new_ping/0,simple_bin/0,simple_bin1/0,produce_bin/0,consume_bin/1,create_messages/1]).

-define(START_MSG_MARKER,<<16#FF,16#FF,16#FF,16#FF,16#FF,16#FF,16#FF,16#FF>>).

show_length() ->
    PingMsg = { ping, "CID-1231231:1122:12312312" },
    PingBin = term_to_binary( PingMsg ),
    PingBinLength = byte_size( PingBin ),
    io:format( "Binary length is ~p~n", [ PingBinLength ] ).

new_ping() ->
    PingMsg = { ping, "CID-1231231:1122:12312312" },
    PingBin = term_to_binary( PingMsg ),
    PingBinLength = byte_size( PingBin ),
    Msg = <<PingBinLength:2, PingBin>>,
    io:format("BP1~n"),
    MsgLen = byte_size( Msg ),
    io:format("MsgLen: ~p, Msg: ~p~n", [MsgLen,Msg]).

simple_bin() ->
    Bin = <<1,2,3,4,5>>,
    Length = byte_size( Bin ),
    io:format("~p:~p~n", [Bin,Length]),
    Msg = <<Length:16>>,
    io:format( "~p~n", [ Msg ] ).
 
simple_bin1() ->
    Bin = <<0,0,0,0,1,2,3,4,5,0,0,0,0,1,2,3,4,0,0,0,0,1,2,3>>,
    Parts = binary:split( Bin, <<0,0,0,0>>, [global] ),
    io:format("~p~n",[Parts]).
%%     Length = byte_size( Bin ),
%%     io:format("~p:~p~n", [Bin,Length]),
%%     <<L1:16>> = binary:part( Bin, 0, 2 ),
%%     TheRest = binary:part( Bin, 2, byte_size( Bin ) - 2 ),
%%     io:format("~p:~p~n", [L1,TheRest]).

produce_bin() ->
    Marker = <<16#FF,16#FF,16#FF,16#FF,16#FF,16#FF,16#FF,16#FF>>,
    Term1 = {ping, [{client_id, "CID-123123:12312:2299"}, 
                    {message_id, "MSG-29992382:29922:39993"}]},
    Term2 = {ping, [{client_id, "CID-123123:12312:2299"}, 
                    {message_id, "MSG-8588330:29922:39993"}]},
    Result = list_to_binary( [ Marker, term_to_binary( Term1 ), Marker, term_to_binary( Term2 ), Marker ] ),
    %%io:format("~p:~p:~p~n",[byte_size(term_to_binary( Term1 )),byte_size(term_to_binary( Term2 )),byte_size(Result)]),
    Result.

consume_bin( <<>> ) ->
    [];
consume_bin( Binary ) ->
    Marker = <<16#FF,16#FF,16#FF,16#FF,16#FF,16#FF,16#FF,16#FF>>,
    BinChunks = binary:split( Binary, Marker, [ global ] ),
    produce_messages( [], BinChunks ).

produce_messages( SoFar, [BinTerm | Tail] ) ->
    case produce_msg( BinTerm ) of
        undefined -> produce_messages( SoFar, Tail );
        Term -> produce_messages( lists:append( SoFar, [Term] ), Tail ) 
    end;
produce_messages( SoFar, [] ) ->
    SoFar.
    
produce_msg( <<>> ) ->
    undefined;
produce_msg( BinTerm ) ->
    binary_to_term( BinTerm ). 

create_messages(Term) when not is_list(Term) ->
    create_messages([Term]);
create_messages(Terms) when is_list(Terms) ->
    lists:flatten( [ [ term_to_binary(Term), ?START_MSG_MARKER ] || Term <- Terms ] ).

