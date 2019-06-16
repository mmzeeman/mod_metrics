
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

    % ?DEBUG({report_bulk, Found}),

    % Query = insert_query(Found, []),

    {ok, State}.

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

insert_query([], Acc) ->
    lists:reverse(Acc);
insert_query([{[_,_|Metric], DataPoints}|Rest], Acc) ->
    ?DEBUG(jiffy:encode(Metric)),
    ?DEBUG(DataPoints).


