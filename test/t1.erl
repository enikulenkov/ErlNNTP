-module(t1).
-export([delete_test_collection/1]).
-include_lib("eunit/include/eunit.hrl").
-define (MESSAGES_FILE, "messages.txt").

create_collection_test() ->
    emongo:insert (test1, "groups", [{"name", "t1"},{"articles_count",0},{"low_bound",1}, {"high_bound",0},{"short_descr", "Test collection"},{"status", "y"}]),    sequence_srv:update(),
    {ok, Socket} = connect_to_erlnntp(),
    {ok, Response1} = gen_tcp:recv (Socket,0),
    ?assertEqual ("200", get_code (Response1)),
    gen_tcp:send (Socket, "GROUP t1\r\n"),
    {ok, Response2} = gen_tcp:recv (Socket, 0),
    ?assertEqual ("211", get_code (Response2)),
    {ok, Fd} = file:open (?MESSAGES_FILE, [read]),
    {ok, Line} = file:read_line (Fd),
    N = list_to_integer(string:strip (Line,right,$\n)),
    fill_db(Socket, Fd, N),
    file:close (Fd),
    gen_tcp:close (Socket).

connect_to_erlnntp () ->
    gen_tcp:connect(localhost, 119, [{active, false}]).

fill_db (_Socket, _Fd, 0) -> ok;

fill_db (Socket, Fd, N) ->
    gen_tcp:send (Socket, "POST\r\n"),
    {ok, Response1} = gen_tcp:recv (Socket, 0),
    ?assertEqual ("340", get_code (Response1)),
    Message = read_message (Fd),
    io:format ("Message is ~p~n", [Message]),
    lists:map (fun (Line) ->
                  gen_tcp:send (Socket, Line ++ "\r\n")
               end,
               Message),
    {ok, Response2} = gen_tcp:recv(Socket,0),
    ?assertEqual ("240", get_code (Response2)),
    fill_db (Socket, Fd, N - 1).

read_message (Fd) ->
    case file:read_line (Fd) of
        {ok, "--\n"} -> [];
        {ok, Line} -> [string:strip (Line,right,$\n) | read_message(Fd)] 
    end.

delete_test_collection (ColName) ->
    emongo:delete (test1, "articles", [{"group", ColName}]),
    emongo:delete (test1, "groups", [{"name",ColName}]).

%session_test_() ->
%    [create_collection_test(), first_test()].

get_code (List) ->
    [Head | _Others] = string:tokens(List, " "),
    Head.

first_test()->
    {ok, Socket} = gen_tcp:connect(localhost, 119, [{active, false}]),
    {ok, Response} = gen_tcp:recv(Socket,0),
    ?assertEqual ("200", get_code(Response)),
    gen_tcp:send (Socket, "GROUP t1\r\n"),
    {ok, Response1} = gen_tcp:recv(Socket,0),
    ?assertEqual ("211", get_code(Response1)),
%    gen_tcp:send (Socket, "OVER\r\n"),
%    {ok, Response2} = gen_tcp:recv(Socket,0),
%    ?assertEqual ("224 Overview information follows:\r\n1\tTest\tEgor\tSat, 28 May 2011 18:50:03 +0200\t1@id.com\t\t0\t0\r\n.\r\n", Response2),
    delete_test_collection ("t1").
