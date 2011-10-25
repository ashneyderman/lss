% @author Alex Shneyderman <a.shneyderman@gmail.com>
% @copyright (C) 2011, Alex Shneyderman
% @since May 21, 2011

-module(lss_agent).
-behaviour(gen_server).

-export([start_link/2,stop/1,ping/1]). % public APIs
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). % gen_server callbacks

-record(state, { tid, socket, data_buffer = [], ping_timer = undefined }).

% public APIs
%--------------------------------------------------------------------
start_link( Port, PingInterval ) -> gen_server:start_link(?MODULE, [Port,PingInterval], []).
ping( Pid ) when is_pid( Pid ) -> gen_server:call( Pid, {ping} ).
stop( Pid ) when is_pid( Pid ) -> gen_server:call( Pid, {stop} ).

% gen_server callbacks
%--------------------------------------------------------------------

%
% @private
% @doc Handling server initialization
%
% @spec init(InitArgs) -> {ok, State}          |
%                         {ok, State, Timeout} |
%                         ignore               |
%                         {stop, Reason}
%       InitArgs = [term()]
%       State = term()
%       Timeout = interger()
%       Reason = term()
% @end
%--------------------------------------------------------------------
init([Port,PingInterval]) ->
    case gen_tcp:connect("localhost",Port,[binary, 
                                             { active, true },
                                             { reuseaddr, true },
                                             { packet, 2 }]) of
        {ok, Socket} ->
            {ok, TRef} = timer:apply_interval(PingInterval * 1000, ?MODULE, ping, [self()] ),
            {ok, #state{ tid = generate_client_id(), socket = Socket, ping_timer = TRef } };
        {tcp_error, _Socket, Reason} ->
            {stop, Reason}
    end.

%
% @private
% @doc  Handling all non synchronous requests
%
% @spec handle_call(Request,From,State) -> {reply, Reply, State}          |
%                                          {reply, Reply, State, Timeout} |
%                                          {noreply, State}               |
%                                          {noreply, State, Timeout}      |
%                                          {stop, Reason, Reply, State}   | (terminate/2 is called)
%                                          {stop, Reason, State}            (terminate/2 is called)
%       Request = term()
%       From = pid()
%       State = term()
% @end
%--------------------------------------------------------------------
handle_call({ping}, _From, #state{ socket = Socket, tid = ClientID } = State) ->
    Ping = {ping, {client_id,ClientID},
                  {message_id,generate_msg_id()}},
    gen_tcp:send(Socket, lss_parse_utils:create_messages([Ping])),
    {reply, ok, State};

handle_call({stop}, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%
% @private
% @doc  Handling all async requests
%
% @spec handle_cast(Request,State) -> {noreply, State}               |
%                                     {noreply, State, Timeout}      |
%                                     {stop, Reason, State}            (terminate/2 is called)
%       Request = term()
%       State = term()
% @end
%--------------------------------------------------------------------
handle_cast(_Request, State) ->
    {noreply, State}.

%
% @private
% @doc Handling all non call/cast requests
%
% @spec handle_info(Message,State) -> {noreply, State}               |
%                                     {noreply, State, Timeout}      |
%                                     {stop, Reason, State}            (terminate/2 is called)
%       Message = term()
%       State = term()
% @end
%--------------------------------------------------------------------
handle_info({tcp, _Socket, Binary}, #state{data_buffer = DataBuffer} = State) ->
    case lss_parse_utils:parse_messages([Binary | DataBuffer]) of
        {Messages} ->
            [self() ! Msg || Msg <- Messages],
            {noreply, State#state{ data_buffer = [] }}; 
        {Messages, NewDataBuffer} ->
            [self() ! Msg || Msg <- Messages],
            {noreply, State#state{data_buffer = [NewDataBuffer]}} 
    end;

handle_info({tcp_closed, _Socket }, #state{ tid = _TestAgentID } = State) ->
    %error_logger:info_msg( "Socket is closed for ~p~n", [TestAgentID] ),
    {stop, normal, State};

handle_info({pong, {client_id, _ClientID},
                   {message_id, _MessageID}}, State ) ->
    %error_logger:info_msg("received pong ~p ~n",[MessageID]),
    {noreply, State};

handle_info(terminate, State) ->
    {stop, normal, State};

handle_info(Message, #state{ tid = TestAgentID } = State) ->
    error_logger:error_msg( "~p received an unexepcted message ~p~n", [TestAgentID,Message] ),
    {noreply, State}.

%
% @private
% @doc Handling process termination
%
% @spec terminate(Reason,State) -> ok
%       Reason = term()
%       State = term()
% @end
%--------------------------------------------------------------------
terminate(_Reason, #state{socket = Socket, tid = _TestAgentID, ping_timer = PingTRef } = _State) ->
    %error_logger:info_msg( "Terminating ~p.~n", [TestAgentID] ),
    timer:cancel(PingTRef),
    gen_tcp:close(Socket),
    ok.

%
% @private
% @doc Handling code updates for the process
%
% @spec terminate(OldVersion,State,Extra) -> {ok, NewState}
%       OldVersion = term()
%       State = term()
%       Extra = term()
%       NewState = term()
% @end
%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% private functions
%--------------------------------------------------------------------
generate_client_id() ->
    {P1,P2,P3} = erlang:now(),
    SList = [ "TID-", lists:flatten(io_lib:format("~p", [P1])), ":", lists:flatten(io_lib:format("~p", [P2])), ":", lists:flatten(io_lib:format("~p", [P3])) ],
    list_to_atom(lists:flatten( SList )).

generate_msg_id() ->
    {P1,P2,P3} = erlang:now(),
    SList = [ "MSG-", lists:flatten(io_lib:format("~p", [P1])), ":", lists:flatten(io_lib:format("~p", [P2])), ":", lists:flatten(io_lib:format("~p", [P3])) ],
    list_to_atom(lists:flatten( SList )).
