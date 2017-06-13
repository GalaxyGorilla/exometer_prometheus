-module(exometer_prometheus_test).

-include_lib("eunit/include/eunit.hrl").

-define(REPORTER, exometer_report_prometheus).

-define(OPTS, [enable_httpd, {host, {127,0,0,1}}, {port, 8081}, {path, "/metrics"}]).

basic_test() ->
    error_logger:tty(false),

    Subscribers = [{reporters, [{?REPORTER, ?OPTS}]}],
    application:set_env(exometer, report, Subscribers),
    {ok, Apps} = application:ensure_all_started(exometer_prometheus),

    ok = exometer:update_or_create([some, counter], 1, counter, []),
    ok = exometer_report:subscribe(?REPORTER, [some, counter], [value, ms_since_reset], manual,
                                   [{help, "some exometer counter"}]),

    ok = exometer:update_or_create([some, other, counter], 1, counter, []),
    ok = exometer_report:subscribe(?REPORTER, [some, other, counter], [value, ms_since_reset], manual,
                                   [{type, counter}, {help, "some other exometer counter with forced type"}]),

    ok = exometer:update_or_create([some, gauge], 1, gauge, []),
    ok = exometer_report:subscribe(?REPORTER, [some, gauge], [value, ms_since_reset], manual, [{help, "some exometer gauge"}]),

    ok = exometer:update_or_create([some, spiral], 1, spiral, []),
    ok = exometer_report:subscribe(?REPORTER, [some, spiral], [count, one], manual, [{help, "some exometer spiral"}]),

    ok = exometer:update_or_create([some, histogram], 0, histogram, []),
    ok = exometer_report:subscribe(?REPORTER, [some, histogram], [50, 75, 90, 95, 99, 999, n, mean, max, min],
                                   manual, [{help, "some exometer histogram"}]),

    ok = exometer:new([some, function], {function, erlang, system_info, ['$dp'], value, [port_count, port_limit]}, []),
    ok = exometer_report:subscribe(?REPORTER, [some, function], [port_count, port_limit], manual, [{help, "some exometer function"}]),

    ?assertEqual(6, length(exometer_report:list_subscriptions(exometer_report_prometheus))),

    {ok, {{"HTTP/1.1",200,"OK"}, _, Body}} = httpc:request("http://localhost:8081/metrics"),
    ?debugFmt("~n~nResult of fetch:~n~n~s", [Body]),

    {ok, {{"HTTP/1.1",404,"Object Not Found"}, _, _}} = httpc:request("http://localhost:8081/blabla"),

    ok = exometer_report:unsubscribe_all(?REPORTER, [some, counter]),
    ?assertEqual(5, length(exometer_report:list_subscriptions(exometer_report_prometheus))),

    [application:stop(App) || App <- Apps],
    ok.

