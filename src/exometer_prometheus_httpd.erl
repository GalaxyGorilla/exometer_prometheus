-module(exometer_prometheus_httpd).

-export([start/1]).

%% httpd callback
-export([do/1]).

-include_lib("inets/include/httpd.hrl").

start(Opts) ->
    inets:start(),
    RequiredPath = proplists:get_value(path, Opts, "/metrics"),
    application:set_env(exometer_prometheus, path, RequiredPath),
    {ok, Pid} = inets:start(httpd, [
                        {modules, [?MODULE]},
                        {port, proplists:get_value(port, Opts, 8081)},
                        {bind_address, proplists:get_value(host, Opts, any)},
                        {server_name, "prometheus"},
                        {document_root, code:priv_dir(exometer_prometheus)},
                        {server_root, code:priv_dir(exometer_prometheus)}
                    ], inets),
    Pid.

do(Req) ->
    Method = Req#mod.method,
    URI = Req#mod.request_uri,
    {ok, RequiredPath} = application:get_env(exometer_prometheus, path),
    case {Method, URI} of
        {"GET", RequiredPath} ->
            Payload = exometer_report_prometheus:fetch(),
            ContentLength = integer_to_list(iolist_size(Payload)),
            RespHeaders = [{code, 200}, {content_length, ContentLength},
                           {content_type, "text/plain; version=0.0.4"}],
            {break, [{response, {response, RespHeaders, [Payload]}}]};
        _Else ->
            Payload = <<"404 - not found">>,
            ContentLength = integer_to_list(iolist_size(Payload)),
            RespHeaders = [{code, 404}, {content_length, ContentLength}],
            {break, [{response, {response, RespHeaders, [Payload]}}]}
    end.

