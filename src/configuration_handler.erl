-module(configuration_handler).
-export([start/0, read/1]).

start() ->
    read_config ("/etc/erlnntp.conf").

read_config (FileName) ->
    {ok, Fd} = file:open (FileName, [read]),
    SettingsTable = ets:new(settings, [named_table]),
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

read (conf_greeting_message) ->
    read_value ("Greeting", str);

read (conf_listening_port) ->
    read_value ("Port", int).

read_value (Parameter, int) ->
    case ets:lookup (settings, Parameter) of
        [{_, Value}] ->
            list_to_integer(Value);
        [] ->
            [{_, Value}] = ets:lookup (default_settings, Parameter),
            Value
    end;

read_value (Parameter, str) ->
    case ets:lookup (settings, Parameter) of
        [{_, Value}] ->
            Value;
        [] ->
            [{_, Value}] = ets:lookup (default_settings, Parameter),
            Value
    end.
