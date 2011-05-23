-module(common_funcs).
-export([parse_datetime/1, get_unix_timestamp/1, trim_whitespace/1,get_full_message_body/1, is_group_empty/1]).
-include("types.hrl").

%%Time functions

% Wed, 27 Oct 2010 09:14:06 -0400 (EDT)
% re:run(X, "(.{3}), ([0-9]+) (.{3}) ([0-9]+) ([0-9]+):([0-9]+):([0-9]+) (.[0-9]+) ")
parse_datetime(X) ->
    [_WD, Day, Month, Year, Hour, Minute, Second, Offset, _TZ] = re:split(X, "[ :]"),
    Date = {any_to_integer(Year), month_to_integer(Month), any_to_integer(Day)},
    Time = {any_to_integer(Hour), any_to_integer(Minute), any_to_integer(Second)},
    Ofs = binary_to_list(Offset),
    SOfs = any_to_integer(string:substr(Ofs, 2, 2)) * 3600 + any_to_integer(string:substr(Ofs, 4, 2)) * 60,
    UTC = calendar:datetime_to_gregorian_seconds( {Date,Time} ) -
        calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ),
    case string:substr(Ofs, 1, 1) of
        "-" -> UTC + SOfs;
        "+" -> UTC - SOfs
    end.

get_unix_timestamp({_MegaSecs, _Secs, _MicroSecs}=TS) ->
    calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(TS) ) -
        calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).

trim_whitespace_left(Input) ->
    {match, [H|_]} = re:run(Input, "[A-Za-z0-9_.,-]+.*"),
    {Start, Len} = H,
    string:substr(Input, Start+1, Len).

trim_whitespace_right(Input) ->
    {match, [H|_]} = re:run(Input, ".*[A-Za-z0-9_.,-]+"),
    {Start, Len} = H,
    string:substr(Input, Start+1, Len).

trim_whitespace(Input) ->
    trim_whitespace_left(trim_whitespace_right(Input)).

any_to_integer(X) ->
    if
        is_binary(X) ->
            list_to_integer(binary_to_list(X));
        is_list(X) ->
            list_to_integer(X)
    end.

month_to_integer(X) ->
    case binary_to_list(X) of
        "Jan" -> 1;
        "Feb" -> 2;
        "Mar" -> 3;
        "Apr" -> 4;
        "May" -> 5;
        "Jun" -> 6;
        "Jul" -> 7;
        "Aug" -> 8;
        "Sep" -> 9;
        "Oct" -> 10;
        "Nov" -> 11;
        "Dec" -> 12
    end.

%%Record routines
is_group_empty (#group{high_bound=High,
                       low_bound=Low}) ->
    (High == 0) and (Low == 0).

get_full_message_body (#article{head=Head,
                                body=Body}) ->
    Head ++ "\r\n\r\n" ++ Body.
