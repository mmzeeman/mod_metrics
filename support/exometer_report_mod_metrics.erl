
-module(exometer_report_mod_metrics).

-behaviour(exometer_report).

-export([
    exometer_init/1,

    exometer_call/3,
    exometer_cast/2,
    exometer_info/2,

    exometer_report/5,
    exometer_report_bulk/3,

    exometer_subscribe/5,
    exometer_unsubscribe/4,

    exometer_newentry/2,

    exometer_setopts/4,
    exometer_terminate/2
]).

-include_lib("include/zotonic.hrl").
-include_lib("exometer_core/include/exometer.hrl").

-record(state, {
    last_timestamp,
    last_report_id,

    context
 }).

exometer_init(Options) ->
    ?DEBUG(Options),
    {context, Context} = proplists:lookup(context, Options),
    {ok, #state{context=Context}}.

exometer_report(_Metric, _DataPoint, _Extra, _Value, State) ->
    %?DEBUG({report, _Metric, _DataPoint, _Extra, _Value}),
    {ok, State}.

exometer_report_bulk(Found, _Extra, State) ->
    Now = z_utils:now(),

    T = fun(Ctx) ->
                Id = case Now =:= State#state.last_timestamp of
                    true ->
                        State#state.last_report_id;
                    false ->
                        [{ReportId}] = z_db:q("INSERT INTO metrics (created) VALUES (to_timestamp($1)) RETURNING id", [Now], Ctx),
                        ReportId
                end,
                [ z_db:q("INSERT INTO datapoint (report_id, metric, datapoint) VALUES ($1, $2, $3)",
                         [Id, metric_json(Metric), datapoint_json(DataPoint, <<>>)], Ctx) || {Metric, DataPoint} <- Found],
                {ok, Id}
    end,

    {ok, ReportId} = z_db:transaction(T, State#state.context),
    {ok, State#state{last_report_id=ReportId, last_timestamp=Now}}.

exometer_subscribe(_Metric, _DataPoint, _Interval, _SubscribeOpts, State) ->
    ?DEBUG({subscribe, _Metric, _DataPoint, _Interval, _SubscribeOpts}),
    {ok, State}.

exometer_unsubscribe(_Metric, _DataPoint, _Extra, State) ->
    ?DEBUG({unsubscribe, _Metric, _DataPoint, _Extra}),
    {ok, State}.

exometer_call(_Unknown, _From, State) ->
    ?DEBUG({call, _Unknown, _From}),
    {ok, State}.

exometer_cast(_Unknown, State) ->
    ?DEBUG({cast, _Unknown}),
    {ok, State}.

exometer_info(_Unknown, State) ->
    ?DEBUG({info, _Unknown}),
    {ok, State}.

exometer_newentry(_Entry, State) ->
    ?DEBUG({newentry, _Entry}),
    {ok, State}.

exometer_setopts(_Metric, _Options, _State, State) ->
    ?DEBUG({setopts, _Metric, _Options, _State}),
    {ok, State}.

exometer_terminate(_Reason, _State) ->
    ?DEBUG({terminate, _Reason}),
    ok.

%%
%% Helpers
%%

metric_json(Metric) -> metric_json(Metric, <<>>).

metric_json([], <<>>) -> <<"[]">>;
metric_json([], <<Bin/binary>>) -> <<Bin/binary, $]>>;
metric_json([P|Rest], <<>>) ->
    M = json_escape(P),
    metric_json(Rest, <<$[, $", M/binary, $">>);
metric_json([P|Rest], <<Bin/binary>>) ->
    M = json_escape(P),
    metric_json(Rest, <<Bin/binary, $,, $", M/binary, $">>).

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
 


