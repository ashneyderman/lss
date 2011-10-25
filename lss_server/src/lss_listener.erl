% @author Alex Shneyderman <a.shneyderman@gmail.com>
% @copyright (C) 2011, Alex Shneyderman
% @since May 28, 2011

%
% @doc 
% LSS listener module listens for an incoming connection from a perspective
% client and instructs LSS to create a separate process that is usually 
% handling the messsages from the remote client device.
% @end
%--------------------------------------------------------------------

-module(lss_listener).
-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {port,           % Port we are listening on
                listener,       % Listening socket
                module,         % Message handling module
                supervisor}).  % Supervisor name    

% public APIs
%--------------------------------------------------------------------

%
% @doc 
% Creates a listener that will be listening on the given Port. When a new 
% connection arrives this module will spawn a new process identifeid by 
% ClientModule argument while placing it under a supervisor identified by
% SupervisorName.
%
% @spec start_link(Port, ClientModule, SupervisorName) -> { ok, Pid } | 
%                                                         ignore |
%                                                         { error, Error }
%       Port = int()
%       ClientModule = atom()
%       SupervisorName = atom()
%       Pid = pid()
%       Error = term()
% @end
%--------------------------------------------------------------------
start_link(Port, ClientModule, SupervisorName) when is_integer(Port), 
                                                    is_atom(ClientModule), 
                                                    is_atom(SupervisorName) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, ClientModule,SupervisorName], []).

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
init([Port,ClientModule,SupervisorName]) ->
    process_flag(trap_exit, true),
    { ok, LSocket } = gen_tcp:listen( Port, [ binary,
                                             { active, true },
                                             { reuseaddr, true },
                                             { packet, 2 } ] ),
    {ok, #state{ port = Port, 
                 listener = LSocket, 
                 module = ClientModule,
                 supervisor = SupervisorName }, 0}.

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
handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

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
handle_info(timeout,#state{listener = LSocket, supervisor = Super, module = Mod} = State) ->
    %error_logger:info_msg("Waiting for the next peer to connect ...\n"),

    {ok, CSocket} = gen_tcp:accept(LSocket),
    %error_logger:info_msg("Accepted socket for peer ~p.\n",[inet:peername(CSocket)]),

    {ok, Cid} = supervisor:start_child(Super, []),
    %error_logger:info_msg("New process is created ~p.\n",[erlang:process_info(Cid, registered_name)]),

    gen_tcp:controlling_process(CSocket, Cid),
    %error_logger:info_msg("Passed control of socket ~p communication over to process ~p.\n",
    %                                [inet:peername(CSocket), 
    %                                 erlang:process_info(Cid, registered_name)]),

    Mod:set_socket(Cid, CSocket),
    %error_logger:info_msg("Notified ~p process of its socket.\n",[erlang:process_info(Cid, registered_name)]),
    {noreply, State, 0};

handle_info(Info, State) ->
    error_logger:error_msg("Info received ~p~n",[Info]),
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
terminate(_Reason, _State) ->
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
