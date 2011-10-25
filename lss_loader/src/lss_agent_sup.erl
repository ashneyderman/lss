% @author Alex Shneyderman <a.shneyderman@gmail.com>
% @copyright (C) 2011, Alex Shneyderman
% @since May 27, 2011

-module(lss_agent_sup).
-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

start_link(Port, PingInterval) ->
    supervisor:start_link({local,?MODULE},?MODULE,[Port,PingInterval]).

%
% @private
% @doc
% TODO: description
%
% @spec init(InitArgs) -> {ok,  {SupFlags,  [ChildSpec]}} |
%                         ignore                          |
%                         {error, Reason}
%       InitArgs = [term()]
% @end
%--------------------------------------------------------------------
init([Port,PingInterval]) ->
    {
        ok,
        {
            { simple_one_for_one,0,1 },            % SupFlags 
            [
                {                                  % ChildSpec
                    lss_agent,
                    {lss_agent,start_link,[Port,PingInterval]},
                    temporary,
                    10000,
                    worker,
                    [lss_agent]
                }
            ]
        }
    }.