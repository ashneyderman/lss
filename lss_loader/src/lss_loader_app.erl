% @author Alex Shneyderman <a.shneyderman@gmail.com>
% @copyright (C) 2011, Alex Shneyderman
% @since May 29, 2011

-module(lss_loader_app).
-behaviour(application).

-export([start/2, stop/1]).

-define(DEFAULT_PORT,8000).
-define(DEFAULT_PING_INTERVAL,30).

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
    case lss_loader_sup:start_link(?DEFAULT_PORT,?DEFAULT_PING_INTERVAL) of
	   {ok, Pid} -> {ok, Pid};
	   Error -> Error
	end.

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