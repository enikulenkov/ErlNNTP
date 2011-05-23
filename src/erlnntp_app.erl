-module(erlnntp_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_,_) ->
    configuration_handler:start(),
    {ok, Pid} = erlnntp_sup:start_link(),
    {ok, Pid}.

stop (_) ->
    ok.
