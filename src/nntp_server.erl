-module(nntp_server).
-export([start/0, stop/0]).

%%These are the start functions used to start and initialize server

start() ->
    register(nntp_server, spawn (?MODULE, init, [])).

init() ->
    PortNum = configuration_handler:read (conf_listening_port),
    {ok, ListenSocket} = gen_tcp:listen (PortNum, [{active, false}]),
    wait_connect (ListenSocket, 0).

wait_connect (ListenSocket, Count) ->
    {ok, Socket} = gen_tcp:accept (ListenSocket),
    Pid = spawn (connection_handler, start, [Socket]),
    gen_tcp:controlling_process(Socket, Pid),
    wait_connect(ListenSocket, Count+1).

stop() -> ok.
