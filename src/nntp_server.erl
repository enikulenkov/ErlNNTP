-module(nntp_server).
-behaviour(gen_server).
-export([start/0, stop/0, init/1, wait_connect/2, start_link/0]).

%% @doc Register nntp_server and starts the initialization of it.

-spec(start() -> true).

start() ->
    register(nntp_server, spawn (?MODULE, init, [])).

%% @doc Read the port for listening to from config file, fetch the server Listening Socket on this port and run the cycle for incoming connections

-spec(init(_) -> none()).

init(_) ->
   PortNum = configuration_handler:read (conf_listening_port),
   {ok, ListenSocket} = gen_tcp:listen (PortNum, [{packet,0},{active, false}]),
   Pid = spawn_link (?MODULE, wait_connect, [ListenSocket, 0]),
   {ok, Pid}.

handle_call (_, _, _) -> ok.

handle_cast (_, _, _) -> ok.



%% @doc Accept incoming connections. Spawns new handling process for each connection. The arguments are server listening socket and number of accepted connections.

-spec(wait_connect(ListenSocket::any(), Count::integer()) -> none()).

wait_connect (ListenSocket, Count) ->
    {ok, Socket} = gen_tcp:accept (ListenSocket),
    erlnntp_sup:start_connection_handler(Socket),
%%    Pid = spawn (connection_handler, start, [Socket]),
%%    ok = gen_tcp:controlling_process(Socket, Pid),
    wait_connect(ListenSocket, Count+1).

%% @doc Stops the server and unregister it.

-spec(stop() -> ok).

stop() -> 
    unregister (nntp_server),
    ok.

start_link() ->
   gen_server:start_link ({local, nntp_server}, ?MODULE, [], []).
