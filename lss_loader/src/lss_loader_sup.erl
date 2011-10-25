% @author Alex Shneyderman <a.shneyderman@gmail.com>
% @copyright (C) 2011, Alex Shneyderman
% @since May 29, 2011

-module(lss_loader_sup).
-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

% public APIs
%--------------------------------------------------------------------
start_link(Port,PingInterval) ->
    supervisor:start_link({local,?MODULE},?MODULE,[Port,PingInterval]).

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
%       MaxR = MaxT = int()>=0
%       ChildSpec = {Id,StartFunc,Restart,Shutdown,Type,Modules}
%       Id = term()
%       StartFunc = {ChildModule,ChildFunction,ChildArgs}
%       ChildModule = ChildFunction = atom()
%       ChildArgs = [term()]
%       Restart = permanent | transient | temporary
%       Shutdown = brutal_kill | int()>=0 | infinity
%       Type = worker | supervisor
%       Modules = [Module] | dynamic
%       Module = atom()
% @end
%--------------------------------------------------------------------
init([Port,PingInterval]) ->
    {
        ok,
        {
            {one_for_all,0,1},                  % {RestartStrategy,MaxR,MaxT} 
            [                                   % RestartStrategy: one_for_all | one_for_one | rest_for_one | simple_one_for_one
                {                               % start -- ChildSpec
                 lss_agent_sup,                 % Id: term()
                 {lss_agent_sup,start_link,[Port,PingInterval]}, % StartFunc: {ChildModule,ChildFunction,ChildArgs}
                 permanent,                     % Restart: permanent | transient | temporary
                 infinity,                      % Shutdown: brutal_kill | int()>=0 | infinity
                 supervisor,                    % Type: worker | supervisor
                 [lss_agent_sup]                % Modules = [Module] | dynamic
                }                               % end   -- ChildSpec
                ,
                {                               % start -- ChildSpec
                 lss_loadrunner,                % Id: term()
                 {lss_loadrunner,start_link,[lss_agent_sup,{100,100}]}, % StartFunc: {ChildModule,ChildFunction,ChildArgs}
                 permanent,                     % Restart: permanent | transient | temporary
                 20000,                         % Shutdown: brutal_kill | int()>=0 | infinity
                 worker,                        % Type: worker | supervisor
                 [lss_loadrunner]               % Modules = [Module] | dynamic
                }                               % end   -- ChildSpec
            ]
        }
    }.

% private functions
%--------------------------------------------------------------------
