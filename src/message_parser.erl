-module (message_parser).
-export ([parse/1, init_state/0]).

parse (Message) ->
    Tokens = string:tokens (Message, " \r\n\t"),
    case Tokens of
        [] ->
            {error, empty_message};
        _  ->
            Pid = spawn (?MODULE, init_state, []),
            analyze_tokens (Tokens, Pid)
    end.

analyze_tokens ([], _) ->
    {error, bad_syntax};

analyze_tokens (Tokens, Pid) -> 
    [CurrToken | OtherTokens] = Tokens,
    case OtherTokens of
        [] ->
            Pid ! {self(), CurrToken, last};
        _  ->
            Pid ! {self(), CurrToken}
    end,
    receive 
        ok -> analyze_tokens (OtherTokens, Pid);
        {error, Error} -> {error, Error};
        {finished, ParsedMessage} -> {ok, ParsedMessage}
    end.

init_state () ->
    receive 
        {Pid, Token} -> 
            case string:to_upper(Token) of 
                "CAPABILITIES" -> 
                    Pid ! {finished, {capabilities,[]}};
                "MODE" ->
                    Pid ! ok,
                    mode_state ();
                "GROUP" ->
                    Pid ! ok,
                    group_state ();
                "LISTGROUP" ->
                    Pid ! ok,
                    listgroup_state ([], group);
                "ARTICLE" ->
                    Pid ! ok,
                    article_state ();
                "LIST" ->
                    Pid ! ok,
                    list_state ();
                _ ->
                    Pid ! {error, not_recognized_command}
            end;
        {Pid, Token, last} ->
            case string:to_upper(Token) of
                "QUIT" ->
                    Pid ! {finished, {quit, []}};
                "LISTGROUP" ->
                    Pid ! {finished, {listgroup, []}};
                "LAST" ->
                    Pid ! {finished, {last, []}};
                "NEXT" ->
                    Pid ! {finished, {next, []}};
                "ARTICLE" ->
                    Pid ! {finished, {article, []}};
                "POST" ->
                    Pid ! {finished, {post, []}};
                "LIST" ->
                    Pid ! {finished, {list_cmd, []}};
                _ ->
                    Pid ! {error, not_recognized_command}
            end
    end.

mode_state () ->
    receive
        {Pid, Token, last} ->
            case Token of 
                "READER" ->
                    Pid ! {finished, {mode_reader,[]}};
                _ ->
                    Pid ! {error, bad_syntax}
            end
    end.

group_state() ->
    receive
        {Pid, GroupName, last} ->
            Pid ! {finished, {group,[{group_name, GroupName}]}}
    end.

article_state() ->
    receive
        {Pid, SomeId, last} ->
            case string:chr (SomeId, $@) of
                0 ->
                    try
                        NumId = list_to_integer (SomeId),
                        Pid ! {finished , {article, [{num, NumId}]}}
                    catch
                        _:_ -> {error, bad_syntax}
                    end;
                _ ->
                    Pid ! {finished, {article, [{message_id, SomeId}]}}
            end;
        {Pid, _} -> Pid ! {error, bad_syntax}
    end.

listgroup_state (Opts, Waiting_for) ->
    receive
        {Pid, Token} ->
            case Waiting_for of 
                group ->
                    NewOpts = [{group_name, Token} | Opts],
                    Pid ! ok,
                    listgroup_state (NewOpts, range)
            end;
        {Pid, Token, last} ->
            case Waiting_for of
                group ->
                    NewOpts = [{group_name, Token} | Opts],
                    Pid ! {finished, {listgroup, NewOpts}};
                range ->
                    [Range_first, Range_last] = string:tokens(Token, "-"),
                    CheckInt = is_integer (Range_first) and is_integer(Range_last),
                    case CheckInt of
                        true -> 
                            NewOpts = [{range, Range_first, Range_last} | Opts],
                            Pid ! {finished, {listgroup, NewOpts}};
                        false ->
                            Pid ! {error, bad_syntax}
                    end
            end
    end.

list_state() ->
    receive
        {Pid, Message, last} ->
            case string:to_upper (Message) of
                "NEWSGROUPS" ->
                    Pid ! {finished, {list_newsgroups, []}}
            end
    end.
