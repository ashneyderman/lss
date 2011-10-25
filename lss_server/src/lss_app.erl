% @author Alex Shneyderman <a.shneyderman@gmail.com>
% @copyright (C) 2011, Alex Shneyderman
% @since 18 May, 2011 

-module(lss_app).
-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).
-export([start_client/0]).

-define(DEF_PORT,8000).

% public APIs
%--------------------------------------------------------------------
start_client() -> supervisor:start_child(lss_client_sup, []).

% application callbacks
%--------------------------------------------------------------------

%
% @private
% @doc
% This  function is called whenever an application is started using 
% application:start/1,2, and should start the processes of the
% application. If the application is structured according to the OTP 
% design principles as a supervision tree, this means starting the 
% top supervisor of the tree.
%
% @spec start(StartType, StartArgs) -> {ok, Pid} | 
%                                      {ok, Pid, State} | 
%                                      {error, Reason}
%       StartType = normal | {takeover, Node} | {failover, Node}
%       Node = node()
%       StartArgs = term()
%       Pid = pid()
%       State = term()
% @end
%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    ListenPort = get_app_env(listen_port, ?DEF_PORT),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenPort, lss_client]).

%
% @private
% @doc
% This  function is called whenever an application has stopped. It is
% intended to be the opposite of Module:start/2 and should do any 
% necessary cleaning up. The return value is ignored.
%
% @spec stop(State) -> void()
%       State = term()
% @end
%--------------------------------------------------------------------
stop(_State) ->
    ok.

% supervisor callback
%--------------------------------------------------------------------

%
% @private
% @doc
% Whenever a supervisor is started using supervisor:start_link/2,3, 
% this function is called by the new process to find out about restart 
% strategy, maximum restart frequency and child specifications.
%
% @spec init(Args) -> {
%                       ok,
%                       {
%                           {RestartStrategy,MaxR,MaxT},
%                           [ChildSpec]
%                       }
%                     } | 
%                     ignore
%       Args = term()
%       RestartStrategy = one_for_all | one_for_one | rest_for_one | simple_one_for_one
%       MaxR = int()
%       MaxT = int()
%       ChildSpec = {Id,StartFunc,Restart,Shutdown,Type,Modules}
%       Id = term()
%       StartFunc = {ChildModule,ChildFunction,ChildArgs}
%       ChildModule = atom()
%       ChildFunction = atom()
%       ChildArgs = [term()]
%       Restart = permanent | transient | temporary
%       Shutdown = brutal_kill | int() | infinity
%       Type = worker | supervisor
%       Modules = [Module] | dynamic
%       Module = atom()
% @end
%--------------------------------------------------------------------
init([Port,ClientModule]) ->
    { ok, 
        { 
            { one_for_one, 0, 1 }                                        % {RestartStrategy,MaxR,MaxT} 
            ,                                                            % RestartStrategy: one_for_all | one_for_one | rest_for_one | simple_one_for_one
            [                                                            
                {                                                        % start -- ChildSpec:lss_event_manager
                  lss_event_manager,                                     % Id: term()
                  {lss_event_manager,create,[]},                         % StartFunc: {ChildModule,ChildFunction,ChildArgs}
                  permanent,                                             % Restart: permanent | transient | temporary
                  2000,                                                  % Shutdown: brutal_kill | int()>=0 | infinity
                  worker,                                                % Type: worker | supervisor
                  [lss_event_manager]                                    % Modules: [Module] | dynamic
                }                                                        % end   -- ChildSpec:lss_event_manager
                ,                                                              
                {                                                        % start -- ChildSpec:lss_listener
                  lss_listener,                                          % Id: term()
                  {lss_listener,start_link,[Port,ClientModule,lss_client_sup]}, % StartFunc: {ChildModule,ChildFunction,ChildArgs}
                  permanent,                                             % Restart: permanent | transient | temporary
                  2000,                                                  % Shutdown: brutal_kill | int()>=0 | infinity
                  worker,                                                % Type: worker | supervisor
                  [lss_listener]                                         % Modules: [Module] | dynamic                 
                }                                                        % end   -- ChildSpec:lss_listener
                ,
                {                                                        % start -- ChildSpec:lss_client_sup
                  lss_client_sup,                                        % Id: term()
                  {supervisor,start_link,[{local, lss_client_sup}, ?MODULE, [ClientModule]]}, % StartFunc: {ChildModule,ChildFunction,ChildArgs}
                  permanent,                                             % Restart: permanent | transient | temporary
                  infinity,                                              % Shutdown: brutal_kill | int()>=0 | infinity
                  supervisor,                                            % Type: worker | supervisor
                  []                                                     % Modules: [Module] | dynamic
                }                                                        % end   -- ChildSpec:lss_client_sup
                ,
                {                                                        % start -- ChildSpec:lss_store
                  lss_store,                                             % Id: term()
                  {lss_store,start_link,[]},                             % StartFunc: {ChildModule,ChildFunction,ChildArgs}
                  permanent,                                             % Restart: permanent | transient | temporary
                  2000,                                                  % Shutdown: brutal_kill | int()>=0 | infinity
                  worker,                                                % Type: worker | supervisor
                  [lss_store]                                            % Modules: [Module] | dynamic
                }                                                        % end   -- ChildSpec:lss_store
            ]
        } 
    };
init([ClientModule]) ->
    { ok, 
        { 
            { simple_one_for_one, 0, 1 }                                 % {RestartStrategy,MaxR,MaxT} 
            ,                                                            % RestartStrategy: one_for_all | one_for_one | rest_for_one | simple_one_for_one
            [
                {                                                        % start -- ChildSpec: <lss_client>
                  undefined,                                             % Id: term()
                  {ClientModule,start_link,[]},                          % StartFun = {M, F, A}
                  temporary,                                             % Restart  = permanent | transient | temporary
                  10000,                                                 % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                                % Type     = worker | supervisor
                  []                                                     % Modules  = [Module] | dynamic                 
                } 
            ]
        } 
    }.

%
% @private
% @doc
% TODO: description
%
% @spec get_app_env(Opt, Default) -> {ok,  Value} | 
%                                    error
%       Opt = term()
%       Default = term()
%       Value = term()
% @end
%--------------------------------------------------------------------
get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
    {ok, Val} -> Val;
    _ ->
        case init:get_argument(Opt) of
        [[Val | _]] -> Val;
        error       -> Default
        end
    end.