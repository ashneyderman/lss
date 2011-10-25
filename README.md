### Live Socket Server

It is a set of modules to try out [Erlang](http://www.erlang.org "Erlang Site") 
processes and load them too. It is my playgraound for Erlang. So, if you hope
to find anything useful in here, you are probably in the wrong place.

There are 3 modules in here `lss_parser_utils`, `lss_server` and `lss_loader`. The parser 
utils are just that: they provide parsing functionality that is common to both the 
server and the loader. `lss_server` is a module that contains logic for handlng agents
pings, it also provide statistics for how many agents are running and what is the traffic 
in terms of bytes through the server. `lss_loader` is the application that is used to 
load the server; just to see how it the server holds up. 

There is a `rebar` included in all of the modules. So, to compile the modules cd to 
its directory and do 

<code>&gt; ./rebar clean compile</code>

Afterwards, you should be able to startup `lss_server` as OTP app and from another shell
startup `lss_loader`. To successfully run both the loader and the server ebin directories
of both have to be on the path of the erl shell. In addition, ebin directory of the parser
utils sould be on on the path of both. I usually start the experiment with the following
command line for the server:

<code>
(21:04:55) ~/projects/erlang/`lss_n_tools`/`lss_server` > erl -pz ./ebin ../`lss_parse_utils`/ebin

Erlang R14B (erts-5.8.1) [source] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.8.1  (abort with ^G)

1> application:start(`lss_app`).

ok

</code>

Once the server (`lss_app`) is up we could start the loader. From the loader directory start
the erlang shell with exactly the same shell command as for the server. At the erlang shell 
rbing up the loader app:

<code>
1> application:start(`lss_loader`).

ok
</code>

Now if you care to see what is going with the server you can go back to its erlang shell and
check its activity as so:

<code>
2> `lss_store`:summarize().

ok

LSS Summary

Active   :100

Inactive :0

Pings    :200

Bytes    :19360

</code>