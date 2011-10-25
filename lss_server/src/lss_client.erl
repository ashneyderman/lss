% @author Alex Shneyderman <a.shneyderman@gmail.com>
% @copyright (C) 2011, Alex Shneyderman
% @since May 28, 2011

-module(lss_client).
-behaviour(gen_server).

-export([start_link/0,set_socket/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {cid,
                socket,
                addr,
                data_buffer = [],
                total_pings = 0,
                total_bytes = 0}).

% public APIs
%--------------------------------------------------------------------
start_link() -> gen_server:start_link(?MODULE, [], []).
set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) -> gen_server:call(Pid, {set_socket, [Socket]}).    

% gen_server callbacks
%--------------------------------------------------------------------

%
% @private
% @doc 
% Whenever a gen_server is started using gen_server:start/3,4 or 
% gen_server:start_link/3,4 this function is called by the new
% process to initialize.
%
% @spec init(Args) -> {ok,State} |
%                     {ok,State,Timeout} | 
%                     {ok,State,hibernate} | 
%                     {stop,Reason} | 
%                     ignore
%       Args = term()
%       State = term()
%       Timeout = int() | infinity
%       Reason = term()
% @end
%--------------------------------------------------------------------
init([]) -> {ok, #state{cid = generate_client_id()}}.

%
% @private
% @doc  
% Whenever a gen_server receives a request sent using gen_server:call/2,3 
% or gen_server:multi_call/2,3,4, this function is called to handle the 
% request.
%
% @spec handle_call(Request, From, State) -> {reply,Reply,NewState} | 
%                                            {reply,Reply,NewState,Timeout} | 
%                                            {reply,Reply,NewState,hibernate} |
%                                            {noreply,NewState} | 
%                                            {noreply,NewState,Timeout} |
%                                            {noreply,NewState,hibernate} |
%                                            {stop,Reason,Reply,NewState} | 
%                                            {stop,Reason,NewState}
%       Request = term()
%       From = {pid(),Tag}
%       State = term()
%       Reply = term()
%       NewState = term()
%       Timeout = int() | infinity
%       Reason = term()
% @end
%--------------------------------------------------------------------
handle_call({set_socket,[Socket]}, _From, #state{cid = ClientID} = State) ->
    Addr = inet:peername( Socket ),
    lss_event_manager:notify_connect(ClientID),
    {reply, ok, State#state{socket = Socket, addr = Addr}};
handle_call(Request, _From, State) ->
    error_logger:error_msg("Unable to process request ~p~n", [Request]),
    {reply, ok, State}.

%
% @private
% @doc  
% Whenever a gen_server receives a request sent using gen_server:cast/2 
% or gen_server:abcast/2,3, this function is called to handle the request.
%
% @spec handle_cast(Request, State) -> {noreply,NewState} | 
%                                      {noreply,NewState,Timeout} |
%                                      {noreply,NewState,hibernate} |
%                                      {stop,Reason,NewState}
%       Request = term()
%       State = term()
%       NewState = term()
%       Timeout = int() | infinity
%       Reason = term() 
% @end
%--------------------------------------------------------------------
handle_cast(_Request, State) ->
    {noreply, State}.

%
% @private
% @doc 
% This function is called by a gen_server when a timeout occurs or when 
% it receives any other message than a synchronous or asynchronous 
% request (or a system message).
%
% @spec handle_info(Info, State) -> {noreply,NewState} | 
%                                   {noreply,NewState,Timeout} |
%                                   {noreply,NewState,hibernate} |
%                                   {stop,Reason,NewState}
%       Info = timeout | term()
%       State = term()
%       NewState = term()
%       Timeout = int() | infinity
%       Reason = normal | term()
% @end
%--------------------------------------------------------------------
handle_info({tcp, _Socket, Binary}, #state{data_buffer = DataBuffer, total_bytes = TotalBytes} = State) ->
    case lss_parse_utils:parse_messages([Binary | DataBuffer]) of
        {Messages} ->
            [self() ! Msg || Msg <- Messages], % TODO: this is a job for cast calls
            {noreply, State#state{ data_buffer = [], total_bytes = TotalBytes + byte_size(Binary)}}; 
        {Messages, NewDataBuffer} ->
            [self() ! Msg || Msg <- Messages], % TODO: this is a job for cast calls
            {noreply, State#state{data_buffer = [NewDataBuffer], total_bytes = TotalBytes + byte_size(Binary)}} 
    end;

handle_info({tcp_closed, Socket}, #state{socket = Socket, cid = ClientID} = State) ->
    %error_logger:info_msg("~p received tcp_close event~n",[ClientID]),
    lss_event_manager:notify_disconnect(ClientID),
    {stop,normal,State};

handle_info({ping, {client_id, _DeviceID},
                   {message_id, MessageID}}, #state{socket = Socket,
                                                    cid = ClientID,  
                                                    total_pings = TotalPings, 
                                                    total_bytes = TotalBytes} = State) ->
    %error_logger:info_msg("received ping ~p from ~p~n",[MessageID, ClientID]),
    lss_event_manager:notify_ping(ClientID,TotalBytes,calendar:local_time()),
    Pong = {pong, {client_id,ClientID},
                  {message_id,MessageID}},
    gen_tcp:send(Socket, lss_parse_utils:create_messages(Pong)),
    {noreply, State#state{total_pings = TotalPings + 1}};

handle_info(Message, #state{cid = ClientID} = State) ->
    error_logger:error_msg("~p received an unexpected message that will be ignored: ~p~n", [ClientID,Message]),
    {noreply, State}.

%
% @private
% @doc 
% This function is called by a gen_server when it is about to terminate. 
% It should be the opposite of Module:init/1 and do any necessary 
% cleaning up. When it returns, the gen_server terminates with Reason. 
% The return value is ignored.
%
% @spec terminate(Reason,State) -> ok
%       Reason =  normal | 
%                 shutdown | 
%                 {shutdown,term()} | 
%                 term()
%       State = term()
% @end
%--------------------------------------------------------------------
terminate(_Reason, #state{cid = ClientID, 
                          socket = Socket} = _State) ->
    lss_event_manager:notify_disconnect(ClientID),
    gen_tcp:close(Socket),
    ok.

%
% @private
% @doc 
% This function is called by a gen_server when it should update its 
% internal state during a release upgrade/downgrade, i.e. when the 
% instruction {update,Module,Change,...} where Change={advanced,Extra} 
% is given in the appup file. See OTP Design Principles for more 
% information.
%
% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%       OldVsn = Vsn | {down, Vsn}
%       Vsn = term()
%       State = term() 
%       NewState = term()
%       Extra = term()
% @end
%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% private functions
%--------------------------------------------------------------------
generate_client_id() ->
    {P1,P2,P3} = erlang:now(),
    SList = [ "CID-", lists:flatten(io_lib:format("~p", [P1])), ":", lists:flatten(io_lib:format("~p", [P2])), ":", lists:flatten(io_lib:format("~p", [P3])) ],
    list_to_atom(lists:flatten( SList )).
