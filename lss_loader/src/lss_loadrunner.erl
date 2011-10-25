% @author Alex Shneyderman <a.shneyderman@gmail.com>
% @copyright (C) 2011, Alex Shneyderman
% @since May 29, 2011

-module(lss_loadrunner).
-behaviour(gen_server).

-export([start_link/2,start_new_agent/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {agent_super_name,
                loader_spec,
                total_started = 0,
                startup_timer = undefined}).

% public APIs
%--------------------------------------------------------------------
start_link(AgentSuper,LoaderSpec) ->
    gen_server:start_link({local,?MODULE},?MODULE,[AgentSuper,LoaderSpec],[]).

start_new_agent() ->
    gen_server:cast(?MODULE,new_agent).

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
%       Timeout = int()>=0 | infinity
%       Reason = term()
% @end
%--------------------------------------------------------------------
init([AgentSuper,LoaderSpec]) ->
    {_TotalAgents,StartupFreq} = LoaderSpec,
    {ok, TRef} = timer:apply_interval(StartupFreq, ?MODULE, start_new_agent, [] ),
    {ok, #state{ agent_super_name = AgentSuper, loader_spec = LoaderSpec, startup_timer = TRef}}.

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
%       Timeout = int()>=0 | infinity
%       Reason = term()
% @end
%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
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
%       Timeout = int()>=0 | infinity
%       Reason = term() 
% @end
%--------------------------------------------------------------------
handle_cast(new_agent, #state{agent_super_name = AgentSuperName,
                              loader_spec = LoaderSpec,
                              total_started = TotalStarted,
                              startup_timer = STimer} = State) ->
    supervisor:start_child(AgentSuperName, []),
    {TotalAgents,_} = LoaderSpec,
    case more(TotalStarted + 1, TotalAgents) of
        no -> timer:cancel(STimer),
              {noreply,State#state{total_started = TotalStarted + 1}};
        yes-> {noreply,State#state{total_started = TotalStarted + 1}}
    end;

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
%       Timeout = int()>=0 | infinity
%       Reason = normal | term()
% @end
%--------------------------------------------------------------------
handle_info(timeout, #state{loader_spec = LoaderSpec, 
                            total_started = TotalStarted} = State) ->
    {TotalAgents,StartupFreq} = LoaderSpec,
    start_new_agent(),
    NextTimeout = 60000 / StartupFreq,
    case more(TotalStarted + 1, TotalAgents) of
        no ->  {noreply,State#state{ total_started = TotalStarted + 1 }};
        yes -> {noreply,State#state{ total_started = TotalStarted + 1 },NextTimeout}
    end;
handle_info(_Info, State) ->
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
%       State = NewState = term()
%       Extra = term()
% @end
%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% private functions
%--------------------------------------------------------------------
more(Current, TotalNeeded) when Current < TotalNeeded -> yes;
more(Current, TotalNeeded) when Current == TotalNeeded -> no.
