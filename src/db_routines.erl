-module(db_routines).
-export([get_group_info/1, write_article/1, get_all_group_article_numbers/1, get_all_group_article_numbers_r/3, get_group_first_number/1, get_group_names/0, get_article/2, get_group_list_entries/0]).
-include("types.hrl").
-define (pool, test1).

get_group_info (GroupName) ->
    case emongo:find_one (test1, "groups", [{"name", GroupName}]) of
        [Group] ->
            {ok, from_group_doc_to_rec(remove_id_field_from_doc(Group))};
        [] -> 
            {error, no_such_group} 
    end.

update_group (#group{name=GroupName}=Group) ->
    emongo:update (test1, "groups", [{"name", GroupName}], from_group_rec_to_doc(Group)).


remove_id_field_from_doc ([{<<"_id">>,_}|Meaningful]) ->
    Meaningful.
 
from_group_doc_to_rec ([{<<"name">>, Name},
                        {<<"articles_count">>, Count},
                        {<<"low_bound">>, Low}, 
                        {<<"high_bound">>, High},
                        {<<"short_descr">>,ShortDescr},
                        {<<"status">>,Status}]) ->
    #group {name = binary_to_list(Name),
            articles_count = Count,
            low_bound = Low,
            high_bound = High,
            short_descr = binary_to_list(ShortDescr),
            status = binary_to_list(Status)}.

from_group_rec_to_doc (#group{name=Name,
                              articles_count=Count,
                              low_bound=Low,
                              high_bound=High,
                              short_descr=ShortDescr,
                              status=Status}) ->
    [{"name", Name},
     {"articles_count", Count},
     {"low_bound", Low}, 
     {"high_bound", High},
     {"short_descr",ShortDescr},
     {"status",Status}].

write_article (#article{id=MessageId,
                        number=Number,
                        group=Group, 
                        head=Head, 
                        body=Body,
                        time=DateTime}) ->
    io:format("Saving to group: ~p~n", [Group]),
    emongo:insert (test1, "articles", [{"group", Group},{"number", Number},{"message_id", MessageId}, {"head", Head}, {"body", Body}, {"time", DateTime}]).

get_article (GroupName, ArticleNum) ->
    [Document] = emongo:find_one (test1, "articles",[{"group", GroupName},{"number", ArticleNum}]),
    from_article_doc_to_rec(remove_id_field_from_doc(Document)).

from_article_doc_to_rec ([{<<"group">>,GroupName},
                          {<<"number">>, Number},
                          {<<"message_id">>,MessageId},
                          {<<"head">>, Head},
                          {<<"body">>, Body},
                          {<<"time">>, DateTime}]) ->
    #article{group=GroupName,
             number=round(Number),
             id=binary_to_list(MessageId),
             head=binary_to_list(Head),
             body=binary_to_list(Body),
             time=DateTime}.

get_all_group_article_numbers (GroupName) ->
    Response = emongo:find_all (test1, "articles", [{"group", GroupName}],[{fields, ["number"]}]),
    extract_numbers_from_db_response (Response).

get_all_group_article_numbers_r (GroupName, RangeFrom, RangeTo) ->
    Response = emongo:find_all (test1, "articles", [{"group", GroupName}, {"number", [{gte, RangeFrom}, {lte, RangeTo}]}],[{fields, ["number"]}]),
    extract_numbers_from_db_response (Response).

extract_numbers_from_db_response (Response) ->
    lists:map (
        fun(Doc) ->
                {<<"number">>, Num} = remove_id_field_from_doc(Doc),
                Num
        end,
        Response).

get_group_first_number (GroupName) ->
    [Doc] = emongo:find_one (test1, "groups", [{"name", GroupName}], [{fields, ["low_bound"]}]),
    [{_ , Number}] = remove_id_field_from_doc(Doc).

get_group_names () ->
    Docs = emongo:find_all (test1, "groups", [], [{fields, ["name"]}]),
    lists:map (fun(X) ->
                [{<<"name">>, Name}] = remove_id_field_from_doc(X),
                binary_to_list(Name)
               end,
               Docs). 

get_group_short_descrs () ->
    Docs = emongo:find_all (test1, "groups", [], [{fields, ["name", "short_descr"]}]),
    lists:map (fun (X) ->
                [{<<"name">>,Name},{<<"short_descr">>, Descr}] = remove_id_field_from_doc (X),
                string:join ([binary_to_list(Name), binary_to_list(Descr)],"\t")
               end,
               Docs).

get_group_list_entries () ->
    Docs = emongo:find_all (test1, "groups", [], [{fields, ["name", "high_bound", "low_bound", "status"]}]),
    lists:map (fun (X) ->
                [{<<"name">>, Name}, {<<"low_bound">>, LowBound}, {<<"high_bound">>, HighBound}, {<<"status">>, Status}] = remove_id_field_from_doc(X),
                string:join ([binary_to_list(Name), integer_to_list(HighBound), integer_to_list(LowBound), binary_to_list(Status)], " ")
               end,
              Docs). 
