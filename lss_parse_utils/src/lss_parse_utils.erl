% @author Alex Shneyderman <a.shneyderman@gmail.com>
% @copyright (C) 2011, Alex Shneyderman
% @since May 26, 2011

%-------------------------------------------------------------------- 
% @doc
% Helper module that contains routines that parse a binary stream 
% and produce messages (tuples) to be consumed by LSS. The operation
% can work in reverse as well - given a tuple a binary could be 
% produced.
% @end
%--------------------------------------------------------------------
-module(lss_parse_utils).

-export([parse_messages/1,create_messages/1]).
-define(START_MSG_MARKER,<<16#FF,16#FF,16#FF,16#FF,16#FF,16#FF,16#FF,16#FF>>).

%-------------------------------------------------------------------- 
% @doc 
% Parsing messages out of the chunks of binaries that arrived on the socket.
% The function returns a list of messages taht were parsed and left over binary 
% chunks.
%
% @spec parse_messages(Binaries) -> { Messages } |
%                                   { Messages, LeftOver }
%       Binaries = [binary()]
%       Messages = [term()]
%       LeftOver = binary()
%
% @end
%--------------------------------------------------------------------
parse_messages( Binaries ) ->
    consume_binary( list_to_binary( lists:reverse( Binaries ) ) ).

%-------------------------------------------------------------------- 
% @doc 
% Takes a term or a list of terms and converts them into a list 
% of binaries that can be sent out via the wire. Terms are separates
% by the START_MSG_MARKER, which is appended to the end of each term.
%
% @spec create_messages(Term|Terms) -> { Messages } 
%       Term = term()
%       Terms = [term()]
%       Messages = [binary()]
% @end
%--------------------------------------------------------------------
create_messages([]) -> [];
create_messages([Term|Terms]) -> lists:append( create_messages(Term), create_messages(Terms) );
create_messages(Term) when is_tuple(Term)-> [term_to_binary(Term), ?START_MSG_MARKER].

% module functions
%--------------------------------------------------------------------
consume_binary( <<>> ) -> { [] };
consume_binary( Binary ) ->
    BinChunks = binary:split( Binary, ?START_MSG_MARKER, [ global ] ),
    Messages = produce_msgs( [], BinChunks ),
    case lists:last( BinChunks ) of
        <<>> ->
            { Messages };
        Other ->
            { Messages, Other }
    end.

produce_msgs( SoFar, [] ) -> SoFar;
produce_msgs( SoFar, [BinTerm,<<>> | Tail] ) -> % <<>> stands for the message separator, so BinTerm represents a full message
    case produce_msg( BinTerm ) of
        undefined -> produce_msgs( SoFar, Tail );
        Term -> produce_msgs( lists:append( SoFar, [Term] ), Tail ) 
    end.


produce_msg( <<>> ) -> undefined;
produce_msg( BinTerm ) -> binary_to_term( BinTerm ).                    
