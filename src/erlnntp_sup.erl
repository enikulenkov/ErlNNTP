-module(erlnntp_sup).
-behaviour(supervisor).

-export([start_link/0,start_connection_handler/1, stop_connection_handler/1]).
%supervisor exports
-export([init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_connection_handler (Socket) ->
    supervisor:start_child (?MODULE, {Socket, {connection_handler, start_link, [Socket]}, permanent, 10000, worker, [connection_handler]}). 

stop_connection_handler (Socket) ->
    supervisor:terminate_child (?MODULE, Socket),
    supervisor:delete_child (?MODULE, Socket).

init(_) ->
    {ok, {{one_for_one, 10, 10},
            [
             {nntp_server, {nntp_server, start_link,[]},
              permanent, 5000, worker, [nntp_server]},
             {db_handler, {db_handler, start_link, []},
              permanent, 5000, worker, [db_handler]}
            ]}}. 
