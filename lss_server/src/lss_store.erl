% @author Alex Shneyderman <a.shneyderman@gmail.com>
% @copyright (C) 2011, Alex Shneyderman
% @since May 28, 2011

%-------------------------------------------------------------------- 
% @doc 
% Module designated to store tuples of <i>#device_record</i> for each
% device that communicates with LSS. The module collects information 
% on the device lifecycle events and can be queried when such a need 
% arises.
% @end
%--------------------------------------------------------------------
-module(lss_store).
-behaviour(gen_server).

-export([start_link/0,
         device_connect/1,
         device_disconnect/1,
         device_ping/2,
         show_all/0,
         show_one/1,
         summarize/0,
         reset/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../include/lss_records.hrl").
-record(state,{table_id}).
-record(summary,{ total_bytes = 0, 
                  total_pings = 0, 
                  total_active = 0, 
                  total_inactive = 0 }).

% public APIs
%--------------------------------------------------------------------

%
% @doc 
% Starts up the data store that keeps track of the devices. Implementation
% creates an ETS table to store information.
%
% @spec start_link() -> {ok,Pid} | 
%                       ignore | 
%                       {error,Error}
%       Pid = pid()
%       Error = term()
% @end
%--------------------------------------------------------------------
start_link() -> 
    case gen_server:start_link({local,?MODULE},?MODULE,[],[]) of 
        {ok,Pid} ->
            lss_store_events:start_listening(),
            {ok, Pid};
        Otherwise ->
            Otherwise
    end.

%
% @doc 
% Method should be called if device is connected for the first time. 
% A new record will be created if none exists for the given ClientID. 
% Otherwise, an old record will be used. In either case the record will
% be initialized with the current timestamp for the device_connected 
% field.
%
% @spec device_connect(ClientID) -> ok
%       ClientID = term()
% @end
%--------------------------------------------------------------------
device_connect(ClientID) -> 
    gen_server:cast(?MODULE,{device_connect,ClientID}).

%
% @doc 
% Method should be called if device is disconnected. The device_connected 
% field is going to be updated on the record that represents the device
% identified by the ClientID.
%
% @spec device_disconnect(ClientID) -> ok
%       ClientID = term()
% @end
%--------------------------------------------------------------------
device_disconnect(ClientID) -> 
    gen_server:cast(?MODULE,{device_disconnect,ClientID}).

%
% @doc 
% Method should be called if device pinged the server. The total_pings,
% last_ping_time and total_data_bytes fields are going to be updated on 
% the record that represents the device identified by the ClientID.
%
% @spec device_ping(ClientID,TotalDataBytes) -> ok
%       ClientID = term()
%       TotalDataBytes = int()
% @end
%--------------------------------------------------------------------
device_ping(ClientID, TotalDataBytes) ->
    gen_server:cast(?MODULE,{device_ping,ClientID,TotalDataBytes}).

%
% @doc 
% Returns all device_record's known to the system.
%
% @spec show_all() -> [DeviceRecord]
%       DeviceRecord = term()
% @end
%--------------------------------------------------------------------
show_all() -> 
    gen_server:call(?MODULE,show_all).

%
% @doc 
% Returns a device_record for the ClientID.
%
% @spec show_one(ClientID) -> DeviceRecord
%       DeviceRecord = term()
% @end
%--------------------------------------------------------------------
show_one(ClientID) -> 
    gen_server:call(?MODULE,{show_one,ClientID}).

%
% @doc 
% Outputs server statistics to stdout.
%
% @spec summarize() -> ok
% @end
%--------------------------------------------------------------------
summarize() ->
    gen_server:cast(?MODULE,summarize).


%
% @doc 
% Wipes the tables containing the device data and statistics clean.
%
% @spec reset() -> ok
% @end
%--------------------------------------------------------------------
reset() ->
    gen_server:cast(?MODULE,reset).

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
init(_Args) -> 
    TableID = ets:new(device_records,[set,protected,named_table,{keypos,2}]),
    {ok, #state{table_id = TableID}}.

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
handle_call(show_all, _From, #state{table_id = TableID} = State) ->
    {reply, ets:tab2list(TableID), State};
handle_call({show_one,ClientID}, _From, #state{table_id = TableID} = State) ->
    [Result] = ets:lookup(TableID, ClientID),
    {reply, Result, State};
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
%       Timeout = int() | infinity
%       Reason = term() 
% @end
%--------------------------------------------------------------------
handle_cast({device_connect,ClientID}, #state{table_id = TableID} = State) ->
    ets:insert(TableID, #device_record{cid = ClientID, 
                                       connect_time = calendar:local_time()}),
    {noreply,State};

handle_cast({device_disconnect,ClientID}, #state{table_id = TableID} = State) ->
    Now = calendar:local_time(),
    [Rec] = ets:lookup(TableID, ClientID), % we expect only one record since the table is a set
    NextRec = Rec#device_record{disconnect_time = Now,
                                last_update_time = Now},
    ets:insert(TableID,NextRec),
    {noreply,State};

handle_cast({device_ping,ClientID,TotalDataBytes}, #state{table_id = TableID} = State) ->
    Now = calendar:local_time(),
    [Rec] = ets:lookup(TableID, ClientID), % we expect only one record since the table is a set
    NextRec = Rec#device_record{total_pings = Rec#device_record.total_pings + 1,
                                last_ping_time = Now,
                                total_data_bytes = TotalDataBytes,
                                last_update_time = Now},
    ets:insert(TableID,NextRec),
    {noreply,State};

handle_cast(summarize, #state{table_id = TableID} = State) ->
    Summary = produce_summary(ets:tab2list(TableID)),
    io:format("LSS Summary~nActive   :~p~nInactive :~p~nPings    :~p~nBytes    :~p~n", 
              [ Summary#summary.total_active, Summary#summary.total_inactive,
                Summary#summary.total_pings, Summary#summary.total_bytes ] ),
    
    {noreply, State};

handle_cast(reset, #state{table_id = TableID} = State) ->
    ets:delete_all_objects(TableID),
    {noreply, State};

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
    lss_store_events:stop_listening(),
    ets:delete(device_records),
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
produce_summary( [] ) ->
    #summary{ total_bytes = 0,
              total_pings = 0,
              total_active = 0,
              total_inactive = 0 };
produce_summary( [Record | Tail] ) ->
    #device_record{disconnect_time=RecDisconnectTime, 
                   total_pings=RecTotalPings, 
                   total_data_bytes=RecTotalDataBytes} = Record,
    Current = produce_summary( Tail ),
    case is_active(RecDisconnectTime) of
        yes -> #summary{ total_bytes = Current#summary.total_bytes + RecTotalDataBytes,
                         total_pings = Current#summary.total_pings + RecTotalPings,
                         total_active = Current#summary.total_active + 1,
                         total_inactive = Current#summary.total_inactive };
        no  -> #summary{ total_bytes = Current#summary.total_bytes + RecTotalDataBytes,
                         total_pings = Current#summary.total_pings + RecTotalPings,
                         total_active = Current#summary.total_active,
                         total_inactive = Current#summary.total_inactive + 1 }
    end.     

is_active( undefined ) -> yes;
is_active( _DisconnectTime ) -> no.