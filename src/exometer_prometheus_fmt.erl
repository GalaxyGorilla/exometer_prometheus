-module(exometer_prometheus_fmt).

-export([
    name/1,
    type/1,
    metrics/1
]).

-type prom_name()       :: iolist().
-type prom_type()       :: iolist().
-type prom_help()       :: iolist().
-type prom_text()       :: binary().
-type metric_info()     :: {exometer:name(),
                            exometer:value(),
                            prom_name(),
                            prom_type(),
                            prom_help()}.


%% -------------------------------------------------------
%% Public API
%% -------------------------------------------------------

-spec name(exometer:name()) -> binary().
name(MetricId) ->
    NameList = lists:join($_, lists:map(fun ioize/1, MetricId)),
    NameBin = iolist_to_binary(NameList),
    re:replace(NameBin, "-|\\.", "_", [global, {return,binary}]).

-spec type(atom()) -> binary().
type(undefined)     -> <<"untyped">>;
type(counter)       -> <<"gauge">>;
type(gauge)         -> <<"gauge">>;
type(spiral)        -> <<"gauge">>;
type(histogram)     -> <<"summary">>;
type(function)      -> <<"gauge">>;
type(Tuple) when is_tuple(Tuple) ->
    case element(1, Tuple) of
        function -> <<"gauge">>;
        _Else    -> <<"untyped">>
    end.

-spec metrics([metric_info()]) -> prom_text().
metrics(Metrics) ->
    Formatted = metrics(Metrics, []),
    iolist_to_binary(Formatted).


%% -------------------------------------------------------
%% Internal
%% -------------------------------------------------------

metrics([], Akk) ->
    Akk;
metrics([{Metric, DataPoints, Name, Type, Help} | Metrics], Akk) ->
    Payload = [[<<"# HELP ">>, Name, <<" ">>, Help, <<"\n">>,
                <<"# TYPE ">>, Name, <<" ">>, Type, <<"\n">>] |
               [[Name, map_datapoint(DPName), <<" ">>, ioize(Value), <<"\n">>]
               || {DPName, Value} <- DataPoints, is_valid_datapoint(DPName)]],
    Payload1 = maybe_add_sum(Payload, Name, Metric, Type),
    metrics(Metrics, [Payload1, <<"\n">> | Akk]).

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

