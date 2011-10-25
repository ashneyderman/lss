#!/usr/bin/env escript
%%! -pz /Users/sandor/projects/erlang/lss_n_tools/lss_server/ebin /Users/sandor/projects/erlang/lss_n_tools/lss_parse_utils/ebin 

main(Args) ->
    application:start(lss_app).