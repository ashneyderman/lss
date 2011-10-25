%%%-------------------------------------------------------------------
%%% @author Alex Shneyderman <a.shneyderman@gmail.com>
%%% @copyright (C) 2011, Alex Shneyderman
%%% @doc
%%% TODO: Add description to dev_server_ctl
%%% @end
%%% Created : May 14, 2011 by Alex Shneyderman <a.shneyderman@gmail.com>
%%%-------------------------------------------------------------------

-module(dev_server_ctl).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start/1,state/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
                listen_port,
                lsocket,
                children = []
               }).

%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Server functions
%% ====================================================================
start(Port) -> gen_server:start_link({local,?MODULE},?MODULE,[Port],[]).
state() -> gen_server:call( ?MODULE, state ).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Port]) ->
    { ok, LSocket } = gen_tcp:listen( Port, [ binary,
                                             { active, true },
                                             { reuseaddr, true },
                                             { packet, 2 } ] ),
    {ok, #state{ lsocket=LSocket, listen_port=Port }, 0 }.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(state, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(timeout,#state{ lsocket = LSocket, children = CurrentChildren } = State )->
    {ok, Pid } = dev_server:new(),
    io:format("Process created: ~p~n",[erlang:process_info( Pid, registered_name )]),
    {ok, Socket} = gen_tcp:accept( LSocket ),
    io:format("Accepted socket for peer ~p~n",[inet:peername( Socket )]),
    gen_tcp:controlling_process( Socket, Pid ),
    io:format("Passed controll of socket communication over to process ~p~n",[ erlang:process_info( Pid, registered_name ) ]),
    { noreply, State#state{ children = lists:append( CurrentChildren, [Pid] ) }, 0 };
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

