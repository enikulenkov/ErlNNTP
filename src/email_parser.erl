-module(email_parser).
-export([parse_headers/1, split_message/1, email_get_header_i/2, get_string_header_i/2, dump_email/1]).


legal_rfc822_header_char(X) when X =< 0, X =< 32 -> false;
legal_rfc822_header_char(127)                    -> false;
legal_rfc822_header_char(_)                      -> true.

parse_headers([$\r,$\n|T]) ->
    {ok, {[], T}};
parse_headers([$\r|T]) ->
    {ok, {[], T}};
parse_headers([$\n|T]) ->
    {ok, {[], T}};
parse_headers(Data) ->
    case parser_header_keyword(Data) of
        {ok, Header, Rest} ->     
            case parse_header_value(Rest) of
                {ok, Body, Rest1} ->
                    case parse_headers(Rest1) of
                        {ok, {Headers, T}} ->
                            {ok, {[{Header,Body}|Headers], T}};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

parser_header_keyword(Data) -> parser_header_keyword(Data, []).

parser_header_keyword([$:|T], L) -> {ok, string:to_upper(lists:reverse(L)), T};
parser_header_keyword([H|T], L) ->
    case legal_rfc822_header_char(H) of
        true ->
            parser_header_keyword(T, [H|L]);
        false ->
            {error, bad_character_in_header}
    end;
parser_header_keyword([], _L) ->
    {error, bad_header}.

parse_header_value(Str) -> parse_header_value(Str, []).

parse_header_value([$\r,$\n,$  |T], L) -> parse_header_value(T, L);
parse_header_value([$\n,$  |T], L) -> parse_header_value(T, L);
parse_header_value([$\r,$  |T], L) -> parse_header_value(T, L);

parse_header_value([$\r,$\n,$\t|T], L) -> parse_header_value(T, L);
parse_header_value([$\n,$\t|T], L) -> parse_header_value(T, L);
parse_header_value([$\r,$\t|T], L) -> parse_header_value(T, L);

parse_header_value([$\r,$\n|T], L) -> {ok, common_funcs:trim_whitespace(lists:reverse(L)), T};
parse_header_value([$\n|T],     L) -> {ok, common_funcs:trim_whitespace(lists:reverse(L)), T};
parse_header_value([$\r|T],     L) -> {ok, common_funcs:trim_whitespace(lists:reverse(L)), T};

parse_header_value([H|T],       L) -> parse_header_value(T, [H|L]);
parse_header_value([],         _L) -> {error, bad_body}.

dump_email({[], Body}) ->
    io:format("Body:~n~s~n", [Body]);
dump_email({[H|T], Body}) ->
    {Name, Value} = H,
    io:format("~s ==== ~s~n", [Name, Value]),
    dump_email({T, Body}).

email_get_header_i(M, N) ->
    UN = string:to_upper(N),
    email_get_header(M, UN).

email_get_header({[], _Body}, _Name) ->
    exit(header_not_found);
email_get_header({[H|T], Body}, Name) ->
    case H of
        {Name, Value} -> string:tokens(Value, ",");
        {_N, _V} -> email_get_header({T, Body}, Name)
    end.

get_string_header_i(M, N) ->
    UN = string:to_upper(N),
    get_string_header(M, UN).

get_string_header({[], _Body}, _Name) ->
    exit(header_not_found);
get_string_header({[H|T], Body}, Name) ->
    case H of
        {Name, Value} -> Value; 
        {_N, _V} -> get_string_header({T, Body}, Name)
    end.


split_message(M) ->
    D = string:str(M, "\r\n\r\n"),
    { string:substr(M, 1, D-1), string:substr(M, D+4) }.

