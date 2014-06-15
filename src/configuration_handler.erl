-module(configuration_handler).
-export([start/0, read/1]).
-define(CONFIG_FILE, "/etc/erlnntp.conf").
-include("log_macros.hrl").

start() ->
    fill_default_settings(),
    read_config (?CONFIG_FILE).
    

read_config (FileName) ->
    try file:open (FileName, [read]) of
        {ok, Fd} ->
            SettingsTable = ets:new(settings, [named_table]),
            read_content_of_config (Fd, SettingsTable),
            ?LOG_INFO ("Config file parsed successfully")
    catch _:_ ->
        ?LOG_WARNING("Can't read config file, default settings are used instead")
    end.

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
    read_value ("Port", int);

read (conf_log_level) ->
    read_value ("Log_level", int).

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

%%Default settings
fill_default_settings () ->
    Defaults = ets:new(default_settings,[named_table]),
    ets:insert(Defaults, {"Port", 119}),
    ets:insert(Defaults, {"Greeting", "ErlNNTP is ready"}),
    ets:insert(Defaults, {"Log_level", 1}),
    ets:insert(Defaults, {"ServerName", "erlnntp.com"}).
