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
    Name = make_metric_name(Metric),
    Help = proplists:get_value(help, Opts, <<"undefined">>),
    Type = case proplists:get_value(type, Opts, undefined) of
               undefined -> map_type(exometer:info(Metric, type));
               SomeType  -> ioize(SomeType)
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
    format_metrics(Metrics).

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

format_metrics(Metrics) ->
    Formatted = format_metrics(Metrics, []),
    iolist_to_binary(Formatted).

format_metrics([], Akk) ->
    Akk;
format_metrics([{Metric, DataPoints, Name, Type, Help} | Metrics], Akk) ->
    Payload = [[<<"# HELP ">>, Name, <<" ">>, Help, <<"\n">>,
                <<"# TYPE ">>, Name, <<" ">>, Type, <<"\n">>] |
               [[Name, map_datapoint(DPName), <<" ">>, ioize(Value), <<"\n">>]
               || {DPName, Value} <- DataPoints, is_valid_datapoint(DPName)]],
    Payload1 = maybe_add_sum(Payload, Name, Metric, Type),
    format_metrics(Metrics, [Payload1, <<"\n">> | Akk]).

make_metric_name(Metric) ->
    NameList = lists:join($_, lists:map(fun ioize/1, Metric)),
    NameBin = iolist_to_binary(NameList),
    re:replace(NameBin, "-|\\.", "_", [global, {return,binary}]).

ioize(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
ioize(Number) when is_float(Number) ->
    float_to_binary(Number, [{decimals, 4}]);
ioize(Number) when is_integer(Number) ->
    integer_to_binary(Number);
ioize(Something) ->
    Something.

maybe_add_sum(Payload, MetricName, Metric, <<"summary">>) ->
    {ok, [{mean, Mean}, {n, N}]} = exometer:get_value(Metric, [mean, n]),
    [Payload | [MetricName, <<"_sum ">>, ioize(Mean * N), <<"\n">>]];
maybe_add_sum(Payload, _MetricName, _Metric, _Else) ->
    Payload.

map_type(undefined)     -> <<"untyped">>;
map_type(counter)       -> <<"gauge">>;
map_type(gauge)         -> <<"gauge">>;
map_type(spiral)        -> <<"gauge">>;
map_type(histogram)     -> <<"summary">>;
map_type(function)      -> <<"gauge">>;
map_type(Tuple) when is_tuple(Tuple) ->
    case element(1, Tuple) of
        function -> <<"gauge">>;
        _Else    -> <<"untyped">>
    end.

map_datapoint(value)    -> <<"">>;
map_datapoint(one)      -> <<"">>;
map_datapoint(n)        -> <<"_count">>;
map_datapoint(50)       -> <<"{quantile=\"0.5\"}">>;
map_datapoint(90)       -> <<"{quantile=\"0.9\"}">>;
map_datapoint(Integer) when is_integer(Integer)  ->
    Bin = ioize(Integer),
    <<"{quantile=\"0.", Bin/binary, "\"}">>;
map_datapoint(Something)  ->
    %% this is for functions with alternative datapoints
    Bin = ioize(Something),
    <<"{datapoint=\"", Bin/binary, "\"}">>.

is_valid_datapoint(value) -> true;
is_valid_datapoint(one) -> true;
is_valid_datapoint(n) -> true;
is_valid_datapoint(Number) when is_number(Number) -> true;
is_valid_datapoint(count) -> false;
is_valid_datapoint(mean) -> false;
is_valid_datapoint(min) -> false;
is_valid_datapoint(max) -> false;
is_valid_datapoint(median) -> false;
is_valid_datapoint(ms_since_reset) -> false;
%% this is for functions with alternative datapoints
is_valid_datapoint(_Else) -> true.

