-module (connection_handler).
-export ([get_requests/3, read_line/2]).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2, start_link/1]).
-include("messages.hrl").
-include("types.hrl").
-include("log_macros.hrl").

%% @doc Initialize connection with client by sending
%% greeting message and starting two loops. First for doing actual work 
%% contained in messages (writing to DB, switching modes) and second for 
%% receiving messages from socket and parse them.

init (Socket) ->
    ?LOG_INFO (io_lib:format("Client connected at socket ~p", [Socket])),
    Greeting_Message = ["200 " | configuration_handler:read(conf_greeting_message)],
    send_message (Socket, Greeting_Message),
    spawn_link (?MODULE, get_requests, [Socket, self(), []]),
    {ok, [transmit, invalid, invalid]}.

start_link (Socket) ->
    gen_server:start_link (?MODULE, Socket, []).

%% @doc Receive messages from client and parse them by passing them to
%% message_parser:parse function. Process the results of parsing:
%% handling errors if any or pass the process loop otherwise.

get_requests(Socket, Pid, Buf) ->
    case ?MODULE:read_line(Socket, Buf) of
        {ok, Request, Buf1} ->
            ?LOG_INFO (io_lib:format ("Message recieved ~p \n", [Request])),
            case message_parser:parse (Request) of
                {ok, {quit,[]}} -> 
                    send_message (Socket, ?ACKNOWLEDGE_QUIT),
                    close_connection (Pid, Socket);
                {ok, {post, []}} ->
                    process_post_cmd (Socket);
                {ok, Parsed_message} -> 
                    Reply = gen_server:call (Pid, Parsed_message),
                    case Reply of
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

handle_call ({capabilities, []}, _From, State=[Mode, _CurGroup, _CurArticle]) ->
    case Mode of
        reader -> {reply,{ok, ?CAPABILITIES_READER},State};
        transmit -> {reply,{ok, ?CAPABILITIES_TRANSMIT}, State}
    end;
 
handle_call ({mode_reader, []}, _From, _State=[_Mode, CurGroup, CurArticle]) ->
    {reply, {ok, ?SWITCHED_TO_MODE_READER}, [reader, CurGroup, CurArticle]};

handle_call ({group, [{group_name,GroupName}]}, _From, State=[Mode, _CurGroup, _CurArticle]) ->
    ?LOG_INFO ("Processing GROUP request..."),
    case db_handler:get_group_info(GroupName) of
        {group, Group} ->
            GroupDescription = get_group_description(Group),
            Response = "211 " ++ GroupDescription,
            {reply, {ok, Response}, [Mode, GroupName, Group#group.low_bound]};
        {error, no_such_group} -> 
            {reply, {ok, ?ERROR_NONEXISTENT_GROUP}, State}
    end;

handle_call ({listgroup, []}, _From, State=[Mode, CurGroup, _CurArticle]) ->
    case CurGroup of 
        invalid -> 
            {reply, {ok, ?ERROR_NO_NEWSGROUP_SELECTED}, State};
        _ ->
            ArticleNumbers = db_handler:get_article_numbers_from_group(CurGroup),
            GroupDescription = get_group_descr(CurGroup),
            Response =["211 " ++ GroupDescription ++ " list follows", ArticleNumbers],
            {reply, {ok, multiline, Response}, [Mode, CurGroup, db_handler:get_first_number_from_group(CurGroup)]} 
    end; 
 
handle_call ({listgroup, [{group_name, GroupName}]}, _From, State=[Mode, _CurGroup, _CurArticle]) ->
    GroupDescription = get_group_descr(GroupName),
    case GroupDescription of 
        "" ->
            {reply, {ok, ?ERROR_NONEXISTENT_GROUP}, State};
        _ ->
            ArticleNumbers = db_handler:get_article_numbers_from_group(GroupName),
            Response =["211 " ++ GroupDescription ++ " list follows" | ArticleNumbers],
            {reply, {ok, multiline, Response}, [Mode, GroupName, db_handler:get_first_number_from_group (GroupName)]}
    end;
    

handle_call ({listgroup, [{range, RangeFrom, RangeTo},{group_name, GroupName}]}, _From, State=[Mode, _CurGroup, _CurArticle]) ->
    GroupDescription = get_group_descr(GroupName),
    case GroupDescription of 
        "" ->
            {reply, {ok, ?ERROR_NONEXISTENT_GROUP}, State};
        _ ->
            ArticleNumbers = db_handler:get_article_numbers_from_group_r(GroupName, RangeFrom, RangeTo),
            Response =["211 " ++ GroupDescription ++ " list follows" | ArticleNumbers],
            {reply, {ok, multiline, Response}, [Mode, GroupName, db_handler:get_first_number_from_group(GroupName)]}
    end;

handle_call ({last, []}, _From, State=[_Mode, CurGroup, CurArticle]) ->
    case CurGroup of
        invalid ->
            {reply, {ok, ?ERROR_NO_NEWSGROUP_SELECTED}, State};
        _ ->
            case CurArticle of
                invalid ->
                    {reply, {ok, ?ERROR_CURRENT_ARTICLE_INVALID}, State};
                _ ->
                    ok
            end
    end;

handle_call ({next, []}, _From, State=[_Mode, CurGroup, CurArticle]) ->
    case CurGroup of
       invalid ->
           {reply, {ok, ?ERROR_NO_NEWSGROUP_SELECTED}, State};
       _ ->
           case CurArticle of
               invalid ->
                   {reply, {ok, ?ERROR_CURRENT_ARTICLE_INVALID}, State};
               _ ->
                   ok
            end
    end;


handle_call ({article, []}, _From, State=[_Mode, CurGroup, CurArticle]) ->
    case CurGroup of 
       invalid ->
           {reply, {ok, ?ERROR_NO_NEWSGROUP_SELECTED}, State};
       _ ->
           case CurArticle of
               invalid ->
                   {reply, {ok, ?ERROR_CURRENT_ARTICLE_INVALID}, State};
               _ ->
                   {ok,Article} = db_handler:read_article ({group_and_article_number, CurGroup, CurArticle}),
                   Response = ["220 " ++ integer_to_list(Article#article.number) ++ " " ++ Article#article.id | [common_funcs:get_full_message_body(Article)]],
                   {reply, {ok, multiline, Response}, State}
           end
   end;

handle_call ({article, [{num, ArtNum}]}, _From, State=[Mode, CurGroup, _CurArticle]) ->
    case CurGroup of
       invalid ->
           {reply, {ok, ?ERROR_NO_NEWSGROUP_SELECTED}, State};
       _ ->
           case db_handler:read_article ({group_and_article_number,CurGroup, ArtNum}) of
               {error, _} -> 
                   {reply, {ok, ?ERROR_NO_ARTICLE_WITH_NUMBER}, State};
               {ok, Article} ->
                   Response = ["220 " ++ integer_to_list(Article#article.number) ++ " " ++ Article#article.id | [common_funcs:get_full_message_body(Article)]],
                   {reply, {ok, multiline, Response}, [Mode, CurGroup, ArtNum]}
           end
   end;
 
handle_call ({article, [{message_id, MessageId}]}, _From, State) ->
    case db_handler:read_article (MessageId) of
        {error, _} -> 
            {reply, {ok, ?ERROR_NO_ARTICLE_WITH_ID}, State};
        {ok, Article} ->
            Response = ["220 " ++ "0 " ++ MessageId | Article],
            {reply, {ok, multiline, Response}, State}
    end;
 
handle_call ({head, []}, _From, State=[_Mode, CurGroup, CurArticle]) ->
    case CurGroup of 
       invalid ->
           {reply, {ok, ?ERROR_NO_NEWSGROUP_SELECTED}, State};
       _ ->
           case CurArticle of
               invalid ->
                   {reply, {ok, ?ERROR_CURRENT_ARTICLE_INVALID}, State};
               _ ->
                   {ok,Article} = db_handler:read_article ({group_and_article_number, CurGroup, CurArticle}),
                   Response = ["221 " ++ integer_to_list(Article#article.number) ++ " " ++ Article#article.id | [Article#article.head]], {reply, {ok, multiline, Response}, State}
           end
   end;

handle_call ({head, [{num, ArtNum}]}, _From, State=[Mode, CurGroup, _CurArticle]) ->
    case CurGroup of
       invalid ->
           {reply, {ok, ?ERROR_NO_NEWSGROUP_SELECTED}, State};
       _ ->
           case db_handler:read_article ({group_and_article_number,CurGroup, ArtNum}) of
               {error, _} -> 
                   {reply, {ok, ?ERROR_NO_ARTICLE_WITH_NUMBER}, State};
               {ok, Article} ->
                   Response = ["221 " ++ integer_to_list(Article#article.number) ++ " " ++ Article#article.id | [Article#article.head]],
                   {reply, {ok, multiline, Response}, [Mode, CurGroup, ArtNum]}
           end
   end;
 
handle_call ({head, [{message_id, MessageId}]}, _From, State) ->
    case db_handler:read_article (MessageId) of
        {error, _} -> 
            {reply, {ok, ?ERROR_NO_ARTICLE_WITH_ID}, State};
        {ok, Article} ->
            Response = ["221 " ++ "0 " ++ MessageId | [Article#article.head]],
            {reply, {ok, multiline, Response}, State}
    end;

handle_call ({list_cmd, []}, _From, State) ->
    GroupNames = db_handler:get_group_list_entries(),
    Response = ["215 list of newsgroups follows" | GroupNames],
    {reply, {ok, multiline, Response}, State};
 
handle_call ({list_newsgroups, []}, _From, State) ->
    GroupDescrs = db_handler:get_group_descrs(),
    Response = ["215 information follows" | GroupDescrs],
    {reply, {ok, multiline, Response}, State};

handle_call ({over, []}, _From, State=[_Mode, CurGroup, CurArticle]) ->
    case CurGroup of
        invalid ->
            {reply, {ok, ?ERROR_NO_NEWSGROUP_SELECTED}, State};
        _ ->
            case CurArticle of
                invalid ->
                    {reply, {ok, ?ERROR_CURRENT_ARTICLE_INVALID}, State};
                _ ->
                    Headers = [integer_to_list(CurArticle)| db_handler:get_header_values (CurGroup, CurArticle)],
                    Str_tabbed_headers = separate_by_tab(one,Headers),
                    Response = ["224 Overview information follows:" , Str_tabbed_headers],
                    {reply, {ok, multiline, Response}, State}
            end
    end.

handle_cast (_, _) -> ok.

handle_info (_Info, State) -> 
    {noreply, State}.

terminate (_Reason, _State) -> ok.

code_change (_, _, _) -> ok.
 
%% @doc Send message to the client. Message parameter is not formatted message
%% i.e. without \r\n final sequence.

send_message (Socket, Message) ->
    FormattedMessage = Message ++ "\r\n",
    ?LOG_INFO (io_lib:format ("Response: ~p \n", [FormattedMessage])),
    gen_tcp:send(Socket, FormattedMessage).

send_message (multiline, Socket, Message) ->
    MultilineMessage = multiline_cycle (Message, ""),
    FormattedMessage = MultilineMessage ++ ".\r\n",
    ?LOG_INFO(io_lib:format ("Response: ~p \n", [FormattedMessage])),
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

                        
close_connection (_LoopPid, Socket) ->
    ok = gen_tcp:close (Socket),
    erlnntp_sup:stop_connection_handler (Socket).
    

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

separate_by_tab (many, Lists) ->
    _Result = [separate_by_tab (one, List) || List <- Lists];

separate_by_tab (one, List) ->
    TabList = [Str ++ "\t" || Str <- List],
    Flattened = lists:flatten(TabList),
    _Result = string:substr (Flattened, 1, string:len(Flattened) - 1).
