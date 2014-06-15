-module (startit).

-export ([start/0]).

start() ->
    application:start(erlnntp).
