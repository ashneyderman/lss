% @author Alex Shneyderman <a.shneyderman@gmail.com>
% @copyright (C) 2011, Alex Shneyderman
% @since May 28, 2011

-module(lss_event_manager).

-export([create/0,add_handler/2,del_handler/2,notify_connect/1,notify_disconnect/1,notify_ping/3]).

%
% @doc 
% Starts event manager for Live Socket Server notifications stream.
% This is organized via standard gen_event. The event manager will
% be named 'lss_event_manager'.
%
% @spec create() -> { ok, Pid } |
%                   {error,{already_started,Pid}}
%       Pid = pid()
% @end
%--------------------------------------------------------------------
create() -> 
    gen_event:start_link({local,?MODULE}). 

%
% @doc 
% Adds events handler to 'lss_event_manager' event manager. See man
% pages for gen_event:add_handler to see details about Handler 
% argument.
%
% @spec add_handler(Handler, Args) -> ok | 
%                                     {'EXIT',Reason} | 
%                                     term()
%       Handler = Module | {Module,Id}
%       Module = atom()
%       Id = term()
%       Args = term()
%       Reason = term()
% @end
%--------------------------------------------------------------------
add_handler(Handler, Args) -> 
    gen_event:add_handler(?MODULE, Handler, Args).

%
% @doc 
% Removes event handler from 'lss_event_manager' list of handler/listeners
%
% @spec del_handler(Handler, Args) -> {error,module_not_found} | 
%                                     {'EXIT',Reason} | 
%                                     term()
%       Handler = Module | {Module,Id}
%       Module = atom()
%       Id = term()
%       Args = term()
%       Reason = term()
% @end
%--------------------------------------------------------------------
del_handler(Handler, Args) -> 
    gen_event:delete_handler(?MODULE, Handler, Args).

%
% @doc 
% Creates device connection notification that will be delivered to all
% interested handlers/listeners. ClientID argument identifies the 
% device; notification itself is delivered via gen_event. 
%
% @spec notify_connect(ClientID) -> ok
%       ClientID = term()
% @end
%--------------------------------------------------------------------
notify_connect(ClientID) -> 
    gen_event:notify(?MODULE, {device_connect, ClientID}).

%
% @doc 
% Creates device connection notification that will be delivered to all
% interested handlers/listeners. ClientID argument identifies the 
% device; notification itself is delivered via gen_event. 
%
% @spec notify_disconnect(ClientID) -> ok
%       ClientID = term()
% @end
%--------------------------------------------------------------------
notify_disconnect(ClientID) -> 
    gen_event:notify(?MODULE, {device_disconnect, ClientID}).

%
% @doc 
% Creates device ping notification that will be delivered to all
% interested handlers/listeners. ClientID argument identifies the 
% device, the rest of the arguments are some of the ping/device 
% attributes; notification itself is delivered via gen_event.
%
% @spec notify_ping(ClientID,TotalDataBytes,PingTime) -> ok 
%       ClientID = term()
%       TotalDataBytes = int()
%       PingTime = {Date,Time}
%       Date = date()
%       Time = time()
% @end
%--------------------------------------------------------------------
notify_ping(ClientID, TotalDataBytes, PingTime) -> 
    gen_event:notify(?MODULE, {device_ping, ClientID, TotalDataBytes, PingTime}).
