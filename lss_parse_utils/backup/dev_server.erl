%%%-------------------------------------------------------------------
%%% @author Alex Shneyderman <a.shneyderman@gmail.com>
%%% @copyright (C) 2011, Alex Shneyderman
%%% @doc
%%% This module is used to handle all command communcation towards the
%%% remote device. It basically serves as a server side shadow of the 
%%% device.
%%% @end
%%% Created :  5 May 2011 by Alex Shneyderman <a.shneyderman@gmail.com>
%%%-------------------------------------------------------------------
-module(dev_server).
-behaviour(gen_server).

-define(DEFAULT_PING_INTERVAL, 3000).

%% API
-export([stats/1,
         ping/1,
         stop/1,
         state/1,
         new/0]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-record(device_state,   {device_id=not_connected_yet,
                         server_id, 
                         total_pings=0,
                         ping_interval=?DEFAULT_PING_INTERVAL, 
                         started_on,
                         msg_buffer}).

%% -record(device_command, {name, scheduled_on, acknowldged_on}).

%%%===================================================================
%%% API
%%%===================================================================
stats(DeviceID) -> gen_server:call(DeviceID, {stats}).
ping (DeviceID) -> gen_server:call(DeviceID, {ping}).
stop (DeviceID) -> gen_server:cast(DeviceID, {quit}).
state(DeviceID) -> gen_server:call(DeviceID, {state}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server that will be waiting to accept a connection from
%% a device.
%%
%% @spec create(ServerID, LSocket) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
new() ->
    ServerID = generate_server_id(),
    io:format("Generated new ServerID:~p~n",[ServerID]),
    gen_server:start_link({local, ServerID}, ?MODULE, [ServerID], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([ServerID]) ->
    InitState = #device_state{server_id=ServerID,
                              started_on=calendar:local_time()},
    {ok, InitState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({stats}, _From, State) ->
    #device_state{device_id=DeviceID,started_on=StartedOn,total_pings=TotalPings} = State, 
    Reply = [{device_id, DeviceID},
             {no_of_pings, TotalPings},
             {up_time,calendar:time_difference(calendar:local_time(),StartedOn)}],
    {reply, Reply, State};
handle_call({state}, _From, State) ->
    {reply, State, State};
handle_call({ping}, _From, State ) ->
    #device_state{device_id=DeviceID,total_pings=TotalPings} = State,
    io:format("Ping is issued on device:~p; total pings so far ~p~n", [DeviceID,TotalPings]),
    {reply,ok,State#device_state{total_pings=TotalPings+1}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({quit}, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, _Socket, RawData}, State) ->
    #device_state{ server_id = ServerID } = State,
    io:format( "Server ~p received binary = ~p~n", [ServerID,RawData] ),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    #device_state{ server_id = ServerID } = State,
    io:format( "Server ~p received tcp_close event~n", [ServerID] ),
    {stop,normal,State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
generate_server_id() ->
    {P1,P2,P3} = erlang:now(),
    SList = [ "SID-", lists:flatten(io_lib:format("~p", [P1])), ":", lists:flatten(io_lib:format("~p", [P2])), ":", lists:flatten(io_lib:format("~p", [P3])) ],
    list_to_atom(lists:flatten( SList )).