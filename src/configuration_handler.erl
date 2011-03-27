-module(configuration_handler).
-export([start/0, read/1, main_loop/1]).

start() ->
    SettingsTable = read_config ("/etc/erlnntp.conf"),
    register(configuration_handler, spawn (?MODULE, main_loop, [SettingsTable])).

main_loop(SettingsTable) ->
    receive
        {Pid, conf_listening_port} -> 
            [{_,Port}] = ets:lookup (SettingsTable,"Port"),
            Pid ! {reply, list_to_integer(Port)},
            main_loop(SettingsTable)
    end.

read_config (FileName) ->
    {ok, Fd} = file:open (FileName, [read]),
    SettingsTable = ets:new(myTable, []),
    read_content_of_config (Fd, SettingsTable).

read_content_of_config (Fd, SettingsTable) ->
    case file:read_line (Fd) of
        {ok, Line} -> 
            {Keyword, Value} = process_line (Line),
            ets:insert (SettingsTable, {Keyword, Value}),
            read_content_of_config (Fd, SettingsTable);
        eof -> 
            file:close(Fd),
            SettingsTable
    end.

process_line (Line) ->
    StrippedLine = string:strip (Line,both,$\n),
    TabPos = string:chr (StrippedLine, $=),
    Keyword= string:substr (StrippedLine,1,TabPos-1),
    Value  = string:substr (StrippedLine, TabPos+1),
    {Keyword, Value}.

read (conf_listening_port) ->
    configuration_handler ! {self(), conf_listening_port},
    receive
        {reply, Reply} -> Reply
    end.
    
