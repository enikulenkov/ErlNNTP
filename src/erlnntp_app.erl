-module(erlnntp_app).
-behaviour(application).
-export([start/2, stop/1]).
-include("log_macros.hrl").

start(_,_) ->
    try application:start(syslog) of
        ok ->
            syslog:open ("ErlNNTP", [cons, perror, pid], local0),
            configuration_handler:start(),
            {ok, Pid} = erlnntp_sup:start_link(),
            {ok, Pid}
    catch
        _:_ ->
            io:format ("Failed to start erlang-syslog application. Check its existance"),
            {error, syslog_failed}
    end.

stop (_) ->
    ?LOG_INFO("Closing syslog application"),
    application:stop(syslog),
    ?LOG_INFO("Application closed"),
    ok.
