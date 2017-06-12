-module(exometer_report_prometheus).

-behaviour(exometer_report).

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


-record(state, {something :: term()}).

exometer_init(_Opts) -> {ok, #state{}}.
exometer_subscribe(_Metric, _DataPoints, _Interval, _Opts, State) -> {ok, State}.
exometer_unsubscribe(_Metric, _DataPoint, _Extra, State) -> {ok, State}.
exometer_call(_Req, _From, State) -> {ok, State}.
exometer_newentry(_Entry, State) -> {ok, State}.
exometer_report(_Metric, _DataPoint, _Extra, _Value, State) -> {ok, State}.
exometer_cast(_Unknown, State) -> {ok, State}.
exometer_info(_Info, State) -> {ok, State}.
exometer_setopts(_Metric, _Options, _Status, State) -> {ok, State}.
exometer_terminate(_Reason, _) -> ignore.
