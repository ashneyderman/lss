%%%-------------------------------------------------------------------
%%% @author Aleksandr Shneyderman <sandor@alexmac.local>
%%% @copyright (C) 2011, Aleksandr Shneyderman
%%% @doc
%%%
%%% @end
%%% Created :  6 May 2011 by Aleksandr Shneyderman <sandor@alexmac.local>
%%%-------------------------------------------------------------------
-module(timer_test).

%% API
-export([test/0,timer_callback/0]).

%%%===================================================================
%%% API
%%%===================================================================
test() ->
    {ok, TRef} = timer:apply_interval(1000, timer_test, timer_callback, [] ), 
    io:format( "Timer created ~p~n",[TRef] ),
    TRef.
    
timer_callback() ->
    io:format( "Timer call came in ~n" ).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
