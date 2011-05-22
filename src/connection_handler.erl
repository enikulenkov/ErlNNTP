-module (connection_handler).
-export ([start/1, process_loop/3]).
-behaviour(gen_server).
-include("messages.hrl").
-include("types.hrl").

%% @doc Initialize connection with client by sending
%% greeting message and starting two loops. First for doing actual work 
%% contained in messages (writing to DB, switching modes) and second for 
%% receiving messages from socket and parse them.

-spec(start(Socket::any()) -> any()).

init (Socket) ->
    Greeting_Message = ["200 " | configuration_handler:read(conf_greeting_message)],
    send_message (Socket, Greeting_Message),
    Pid = spawn (?MODULE, process_loop, [transmit, invalid, invalid]),
    spawn (?MODULE, get_requests, [Socket, Pid, []]),
    {ok, [transmit, invalid, invalid]}.

start_link (Socket) ->
    gen_server:start_link (?MODULE, [Socket], []).

%% @doc Receive messages from client and parse them by passing them to
%% message_parser:parse function. Process the results of parsing:
%% handling errors if any or pass the process loop otherwise.

get_requests(Socket, Pid, Buf) ->
    case ?MODULE:read_line(Socket, Buf) of
        {ok, Request, Buf1} ->
            io:format ("Message recieved ~p \n", [Request]),
            case message_parser:parse (Request) of
                {ok, {quit,[]}} -> 
                    send_message (Socket, ?ACKNOWLEDGE_QUIT),
                    close_connection (Pid, Socket);
                {ok, {post, []}} ->
                    process_post_cmd (Socket);
                {ok, Parsed_message} -> Pid ! {self(), Parsed_message},
                    receive
                        {ok, Response} ->
                            send_message(Socket, Response),
                            get_requests(Socket, Pid, Buf1);
                        {ok, multiline, Response} ->
                            send_message (multiline, Socket, Response),
                            get_requests(Socket, Pid, Buf1);
                        {error, _} -> 
                            send_message(Socket, ?INTERNAL_ERROR)
                    end;
                {error, bad_syntax} -> 
                    send_message (Socket, ?ERROR_BAD_SYNTAX);
                {error, empty_message} ->
                    ok;
                {error, not_recognized_command} ->
                    send_message (Socket, ?ERROR_UNKNOWN_COMMAND)
            end,
            ?MODULE:get_requests(Socket, Pid, Buf1);
        {error, closed} ->
            exit;
        Other -> io:format ("~p received?",[Other])
    end.

%% @doc Loop that process messages being in transmit mode.
%% Messages are delivered in the form of tuple {message_type, [Opts]},
%% where message_type denotes the type of message (MODE READER for example) and
%% Opts is the list of tuples with message properties (user credentials for 
%% example).

process_loop (Mode, CurGroup, CurArticle) ->
    receive
       {Pid, {capabilities,[]}} ->
           case Mode of
               reader -> Pid ! {ok, ?CAPABILITIES_READER};
               transmit -> Pid ! {ok, ?CAPABILITIES_TRANSMIT}
           end,
           process_loop(Mode, CurGroup, CurArticle);
       {Pid, {mode_reader,[]}} ->
           Pid ! {ok, ?SWITCHED_TO_MODE_READER},
           process_loop(reader, CurGroup, CurArticle);
       {Pid, {group, [{group_name,GroupName}]}} ->
           case db_handler:get_group_info(GroupName) of
               {group, Group} ->
                   GroupDescription = get_group_description(Group),
                   Response = "211 " ++ GroupDescription,
                   Pid ! {ok, Response},
                   process_loop (Mode, GroupName, Group#group.low_bound);
               {error, no_such_group} -> 
                   Pid ! {ok, ?ERROR_NONEXISTENT_GROUP},
                   process_loop (Mode, CurGroup, CurArticle)
           end;
       {Pid, {listgroup, Opts}} ->
           case Opts of
               [] ->
                   case CurGroup of 
                       invalid -> 
                           Pid ! {ok, ?ERROR_NO_NEWSGROUP_SELECTED},
                           process_loop (Mode, CurGroup, CurArticle);
                       _ ->
                           ArticleNumbers = db_handler:get_article_numbers_from_group(CurGroup),
                           GroupDescription = get_group_descr(CurGroup),
                           Response =["211 " ++ GroupDescription ++ " list follows", ArticleNumbers],
                           Pid ! {ok, multiline, Response},
                           process_loop (Mode, CurGroup, db_handler:get_first_number_from_group(CurGroup))
                   end; 
               [{group_name, GroupName}] ->
                   GroupDescription = get_group_descr(GroupName),
                   case GroupDescription of 
                       "" ->
                           Pid ! {ok, ?ERROR_NONEXISTENT_GROUP},
                           process_loop(Mode, CurGroup, CurArticle);
                       _ ->
                           ArticleNumbers = db_handler:get_article_numbers_from_group(GroupName),
                           Response =["211 " ++ GroupDescription ++ " list follows" | ArticleNumbers],
                           Pid ! {ok, multiline, Response},
                           process_loop (Mode, GroupName, db_handler:get_first_number_from_group (GroupName))
                   end;
               [{range, RangeFrom, RangeTo},{group_name, GroupName}] ->
                   GroupDescription = get_group_descr(GroupName),
                   case GroupDescription of 
                       "" ->
                           Pid ! {ok, ?ERROR_NONEXISTENT_GROUP},
                           process_loop(Mode, CurGroup, CurArticle);
                       _ ->
                           ArticleNumbers = db_handler:get_article_numbers_from_group_r(GroupName, RangeFrom, RangeTo),
                           Response =["211 " ++ GroupDescription ++ " list follows" | ArticleNumbers],
                           Pid ! {ok, multiline, Response},
                           process_loop (Mode, GroupName, db_handler:get_first_number_from_group (GroupName)) 
                   end
           end;
       {Pid, {last,[]}} ->
           case CurGroup of
               invalid ->
                   Pid ! {ok, ?ERROR_NO_NEWSGROUP_SELECTED},
                   process_loop (Mode, CurGroup, CurArticle);
               _ ->
                   case CurArticle of
                       invalid ->
                           Pid ! {ok, ?ERROR_CURRENT_ARTICLE_INVALID},
                           process_loop (Mode, CurGroup, CurArticle);
                       _ ->
                           ok
                    end
            end;
       {Pid, {next,[]}} ->
           case CurGroup of
               invalid ->
                   Pid ! {ok, ?ERROR_NO_NEWSGROUP_SELECTED},
                   process_loop (Mode, CurGroup, CurArticle);
               _ ->
                   case CurArticle of
                       invalid ->
                           Pid ! {ok, ?ERROR_CURRENT_ARTICLE_INVALID},
                           process_loop (Mode, CurGroup, CurArticle);
                       _ ->
                           ok
                    end
            end;
        {Pid, {article, []}} ->
            case CurGroup of 
                invalid ->
                    Pid ! {ok, ?ERROR_NO_NEWSGROUP_SELECTED},
                    process_loop (Mode, CurGroup, CurArticle);
                _ ->
                    case CurArticle of
                        invalid ->
                            Pid ! {ok, ?ERROR_CURRENT_ARTICLE_INVALID},
                            process_loop (Mode, CurGroup, CurArticle);
                        _ ->
                            {ok,Article} = db_handler:read_article (CurGroup, CurArticle),
                            Response = ["220 " ++ Article#article.number ++ " " ++ Article#article.id | Article],
                            Pid ! {ok, multiline, Response},
                            process_loop (Mode, CurGroup, CurArticle)
                    end
            end;
        {Pid, {article, [{num, ArtNum}]}} ->
            case CurGroup of
                invalid ->
                    Pid ! {ok, ?ERROR_NO_NEWSGROUP_SELECTED},
                    process_loop(Mode, CurGroup, CurArticle);
                _ ->
                    case db_handler:read_article ({group_and_article_number,CurGroup, ArtNum}) of
                        {error, _} -> 
                            Pid ! {ok, ?ERROR_NO_ARTICLE_WITH_NUMBER},
                            process_loop (Mode, CurGroup, CurArticle);
                        {ok, Article} ->
                            Response = ["220 " ++ integer_to_list(Article#article.number) ++ " " ++ Article#article.id | [common_funcs:get_full_message_body(Article)]],
                            Pid ! {ok, multiline, Response},
                            process_loop (Mode, CurGroup, ArtNum)
                    end
            end;
        {Pid, {article, [{message_id, MessageId}]}} ->
            case db_handler:read_article (MessageId) of
                {error, _} -> 
                    Pid ! {ok, ?ERROR_NO_ARTICLE_WITH_ID},
                    process_loop (Mode, CurGroup, CurArticle);
                {ok, Article} ->
                    Response = ["220 " ++ "0 " ++ MessageId | Article],
                    Pid ! {ok, multiline, Response},
                    process_loop (Mode, CurGroup, CurArticle)
            end;
        {Pid, {list_cmd, []}} ->
            GroupNames = db_handler:get_group_list_entries(),
            Response = ["215 list of newsgroups follows" | GroupNames],
            Pid ! {ok, multiline, Response},
            process_loop (Mode, CurGroup, CurArticle);
        {Pid, {list_newsgroups, []}} ->
            GroupDescrs = db_handler:get_group_descrs(),
            Response = ["215 information follows" | GroupDescrs],
            Pid ! {ok, multiline, Response},
            process_loop (Mode, CurGroup, CurArticle);
        {_, exit} -> ok
    end.

%% @doc Send message to the client. Message parameter is not formatted message
%% i.e. without \r\n final sequence.

send_message (Socket, Message) ->
    FormattedMessage = Message ++ "\r\n",
    io:format ("Response: ~p \n", [FormattedMessage]),
    gen_tcp:send(Socket, FormattedMessage).

send_message (multiline, Socket, Message) ->
    MultilineMessage = multiline_cycle (Message, ""),
    FormattedMessage = MultilineMessage ++ ".\r\n",
    io:format ("Response: ~p \n", [FormattedMessage]),
    gen_tcp:send(Socket, FormattedMessage).

multiline_cycle ([], Result) -> Result;

multiline_cycle (Lines, Result) ->
    [Line | Others] = Lines,
    multiline_cycle (Others, Result ++ Line ++ "\r\n").

get_group_description (#group{} = Group) ->
    string:join ([integer_to_list(Group#group.articles_count), integer_to_list (Group#group.low_bound), integer_to_list(Group#group.high_bound), Group#group.name], " ").

get_group_descr(GroupName) ->
    case db_handler:get_group_info(GroupName) of
        {group, Group} ->
            get_group_description(Group);
        {error, no_such_group} -> 
            ""
    end.


%%Procedures to read and parse client message

read_message_from_client (Socket) ->
    read_message_from_client (Socket, [], []).

read_message_from_client (Socket, Acc, Buf) ->
    case read_line (Socket, Buf) of
        {ok, ".", _} ->
            io:format ("Tochka received"), 
            {ok, Acc};
        {ok, Data, Buf1} ->
            io:format ("Current Acc is ~p~n",[Acc]),
            read_message_from_client(Socket, join_line (Acc, Data), Buf1);
        Other -> {error, Other}
    end.

read_line (Socket, Buf) ->
    case split_line(Buf) of
        more ->
            case gen_tcp:recv (Socket,0) of
                {ok, Cs} ->
                    io:format ("received : ~p~n",[Cs]),
                    Buf1 = Buf ++ Cs,
                    read_line (Socket, Buf1);
                Error ->
                    Error
            end;
        Done -> Done
    end.

split_line (Cs) ->
    split_line (Cs, []).

split_line ([$\r,$\n|Cs], Buf) ->
    {ok, lists:reverse (Buf), Cs};
split_line ([X|Cs], Buf) ->
    split_line (Cs, [X|Buf]);
split_line ([], _) ->
    more.

join_line ([], X) ->
    X;
join_line (Y, X) ->
    Y ++ "\r\n" ++ X.

                        
close_connection (LoopPid, Socket) ->
    LoopPid ! {self(), exit},
    ok = gen_tcp:close (Socket).

process_post_cmd (Socket) ->
    send_message (Socket, ?INPUT_ARTICLE),
    case read_message_from_client (Socket) of
        {error, _} ->
            io:format ("It was error"),
            send_message (Socket, ?POSTING_FAILED);
        {ok, Message} ->
            case email_parser:parse_headers (Message) of
                {ok, Parsed_Message} ->
                    email_parser:dump_email (Parsed_Message),
                    db_handler:write_article(Message, Parsed_Message),
                    send_message (Socket, ?ARTICLE_RECEIVED);
                {error, _} ->
                    io:format ("Error in headers format"),
                    send_message (Socket, ?POSTING_FAILED)
            end
    end.
