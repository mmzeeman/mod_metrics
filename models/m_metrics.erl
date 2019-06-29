-module(m_metrics).

-include_lib("zotonic.hrl").

-export([
    metric_id/2,
    ensure_metric_id/2,

    new_report_id/2,
    insert_datapoint/4
]).

%% Cache time for metric datapoints.
-define(MAXAGE_METRIC, 7200).

%%
%% Api
%%

new_report_id(Timestamp, Context) ->
    [{ReportId}] = z_db:q("INSERT INTO report (created) VALUES (to_timestamp($1)) RETURNING id", [Timestamp], Context),
    ReportId.

metric_id(Metric, Context) ->
    F = fun() ->
                MetricJSON = metric_json(Metric, z_context:site(Context)),
                case z_db:q("select id from metric where metric @> $1", [MetricJSON], Context) of
                    [] -> undefined;
                    [{Id}] -> Id
                end
        end,
    z_depcache:memo(F, {metric_id, Metric}, ?MAXAGE_METRIC, Context).
    
%
ensure_metric_id(Metric, Context) ->
    case metric_id(Metric, Context) of
        undefined ->
            Id = insert_metric_id(Metric, Context),
            z_depcache:flush({metric_id, Metric}, Context),
            Id;
        Id -> Id
    end.

%
insert_metric_id(Metric, Context) ->
    JSON = metric_json(Metric, Context),

    %% Try to get information on the metric
    Type = case exometer:info(Metric) of
               undefined -> undefined;
               Props -> proplists:get_value(type, Props)
           end,

    [{Id}] = z_db:q("INSERT INTO metric (metric, type) VALUES($1, $2) RETURNING id", [JSON, Type], Context), 
    Id.

% 
insert_datapoint(ReportId, MetricId, DataPoint, Context) ->
    JSON = datapoint_json(DataPoint, <<>>),
    z_db:q("INSERT INTO datapoint (report_id, metric_id, datapoint) VALUES ($1, $2, $3)",
           [ReportId, MetricId, JSON], Context),
    ok.


%%
%% Helpers
%%

metric_json(Metric, #context{}=Context) -> metric_json(Metric, z_context:site(Context));
metric_json([zotonic, Site|Metric], Site) when is_atom(Site) -> metric_json1(Metric, <<>>);
metric_json(Metric, _Site) -> metric_json1(Metric, <<>>).


metric_json1([], <<>>) -> <<"[]">>;
metric_json1([], <<Bin/binary>>) -> <<Bin/binary, $]>>;
metric_json1([P|Rest], <<>>) ->
    M = json_escape(P),
    metric_json1(Rest, <<$[, $", M/binary, $">>);
metric_json1([P|Rest], <<Bin/binary>>) ->
    M = json_escape(P),
    metric_json1(Rest, <<Bin/binary, $,, $", M/binary, $">>).

datapoint_json([], <<>>) -> <<"{}">>;
datapoint_json([], <<Acc/binary>>) -> <<Acc/binary, $}>>;
datapoint_json([{_, _}=KV|Rest], <<>>) ->
    KVBin = kv_json(KV),
    datapoint_json(Rest, <<${, KVBin/binary>>);
datapoint_json([{_, _}=KV|Rest], <<Acc/binary>>) ->
    KVBin = kv_json(KV),
    datapoint_json(Rest, <<Acc/binary, $,, KVBin/binary>>).

kv_json({K, V}) when is_integer(K) andalso is_number(V) ->
    JK = z_convert:to_binary(K),
    JV = z_convert:to_binary(V),
    <<$", JK/binary, $", $:, JV/binary>>;
kv_json({K, V}) when is_atom(K) andalso is_number(V) ->
    JK = json_escape(K),
    JV = z_convert:to_binary(V),
    <<$", JK/binary, $", $:, JV/binary>>.



json_escape(Atom) when is_atom(Atom) -> json_escape(z_convert:to_binary(Atom), <<>>);
json_escape(Bin) when is_binary(Bin) -> json_escape(Bin, <<>>).

json_escape(<<>>, <<Acc/binary>>) -> Acc;
json_escape(<<$", Rest/binary>>, <<Acc/binary>>) -> json_escape(Rest, <<Acc/binary, $", $\\>>);
json_escape(<<$\\, Rest/binary>>, <<Acc/binary>>) -> json_escape(Rest, <<Acc/binary, $\\, $\\>>);
json_escape(<<$/, Rest/binary>>, <<Acc/binary>>) -> json_escape(Rest, <<Acc/binary, $/, $\\>>);
json_escape(<<$\b, Rest/binary>>, <<Acc/binary>>) -> json_escape(Rest, <<Acc/binary, $b, $\\>>);
json_escape(<<$\f, Rest/binary>>, <<Acc/binary>>) -> json_escape(Rest, <<Acc/binary, $f, $\\>>);
json_escape(<<$\n, Rest/binary>>, <<Acc/binary>>) -> json_escape(Rest, <<Acc/binary, $n, $\\>>);
json_escape(<<$\r, Rest/binary>>, <<Acc/binary>>) -> json_escape(Rest, <<Acc/binary, $r, $\\>>);
json_escape(<<$\t, Rest/binary>>, <<Acc/binary>>) -> json_escape(Rest, <<Acc/binary, $t, $\\>>);
json_escape(<<C/utf8, Rest/binary>>, <<Acc/binary>>) -> json_escape(Rest, <<Acc/binary, C/utf8>>).
 


