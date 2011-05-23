-module(erlnntp_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_,_) ->
    configuration_handler:start(),
    try application:start(syslog) of
        ok ->
            {ok, Pid} = erlnntp_sup:start_link(),
            {ok, Pid}
    catch
        _:_ ->
            io:format ("Failed to start erlang-syslog application. Check its existance"),
            {error, syslog_failed}
    end.

stop (_) ->
    ok.
