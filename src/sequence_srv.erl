-module(sequence_srv).
-export([get_next_number/1, update/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link () ->
    gen_server:start_link ({local, sequence},?MODULE, [], []).

init ([]) ->
    Dict = ets:new(dict, []),
    Highs = db_routines:get_group_high_bounds(),
    lists:map (
        fun (Entry={_GroupName, _High_Bound}) ->
                ets:insert (Dict, Entry)
        end,
        Highs),
    {ok, Dict}.

get_next_number (GroupName) ->
    gen_server:call (sequence, {get_next, GroupName}).

update () ->
    gen_server:call (sequence,update).

handle_call({get_next, GroupName}, _From, Dict) ->
    [{_GroupName, Last_Art_Num}] = ets:lookup(Dict, GroupName),
    ets:update_counter (Dict, GroupName, 1),
    {reply, {ok, Last_Art_Num + 1}, Dict};

handle_call (update, _From, _Dict) ->
    {ok, Dict} = init([]),
    {reply, {ok,Dict}, Dict}.

handle_cast(_,_) -> ok.

handle_info(_,_) -> ok.

terminate (_,_) -> ok.

code_change (_,_,_) -> ok.
