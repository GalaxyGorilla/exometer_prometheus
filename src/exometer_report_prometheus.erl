-module(exometer_report_prometheus).

-behaviour(exometer_report).

-export([fetch/0]).

-export([
    exometer_init/1,
    exometer_info/2,
    exometer_cast/2,
    exometer_call/3,
    exometer_report/5,
    exometer_subscribe/5,
    exometer_unsubscribe/4,
    exometer_newentry/2,
    exometer_setopts/4,
    exometer_terminate/2
]).


-record(state, {entries = [] :: list()}).

%% -------------------------------------------------------
%% Public API
%% -------------------------------------------------------

-spec fetch() -> exometer_prometheus_fmt:prom_text().
fetch() ->
    exometer_report:call_reporter(exometer_report_prometheus, {request, fetch}).


%% -------------------------------------------------------
%% exometer reporter callbacks
%% -------------------------------------------------------

exometer_init(Opts) ->
    case lists:member(enable_httpd, Opts) of
        true -> exometer_prometheus_httpd:start(Opts);
        false -> ok
    end,
    {ok, #state{}}.

exometer_subscribe(Metric, DataPoints, _Interval, Opts, State = #state{entries=Entries}) ->
    Name = exometer_prometheus_fmt:name(Metric),
    Help = proplists:get_value(help, Opts, <<"undefined">>),
    Type = case proplists:get_value(type, Opts, undefined) of
               undefined -> exometer_prometheus_fmt:type(exometer:info(Metric, type));
               SomeType  -> atom_to_binary(SomeType, latin1)
           end,
    Entry = {Metric, DataPoints, Name, Type, Help},
    {ok, State#state{entries = Entries ++ [Entry]}}.

exometer_unsubscribe(Metric, _DataPoints, _Extra, State = #state{entries=Entries}) ->
    {ok, State#state{entries = proplists:delete(Metric, Entries)}}.

exometer_call({request, fetch}, _From, State = #state{entries = Entries}) ->
    {reply, fetch_and_format_metrics(Entries), State};
exometer_call(_Req, _From, State) ->
    {ok, State}.

exometer_newentry(_Entry, State) -> {ok, State}.
exometer_report(_Metric, _DataPoint, _Extra, _Value, State) -> {ok, State}.
exometer_cast(_Unknown, State) -> {ok, State}.
exometer_info(_Info, State) -> {ok, State}.
exometer_setopts(_Metric, _Options, _Status, State) -> {ok, State}.
exometer_terminate(_Reason, _) -> ignore.


%% -------------------------------------------------------
%% internal
%% -------------------------------------------------------

fetch_and_format_metrics(Entries) ->
    Metrics = fetch_metrics(Entries),
    exometer_prometheus_fmt:metrics(Metrics).

fetch_metrics(Entries) ->
    fetch_metrics(Entries, []).

fetch_metrics([], Akk) ->
    Akk;
fetch_metrics([{Metric, DataPoints, Name, Type, Help} | Entries], Akk) ->
    case exometer:get_value(Metric, DataPoints) of
        {ok, DataPointValues} ->
            fetch_metrics(Entries, [{Metric, DataPointValues, Name, Type, Help} | Akk]);
        _Error ->
            fetch_metrics(Entries, Akk)
    end.

