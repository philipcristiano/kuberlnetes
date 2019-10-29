-module(kuberlnetes_watcher).

-export([watch/3]).

watch(API, {ListOp, WatchOp}, []) ->
    ListResp = swaggerl:op(API, ListOp, []),
    Metadata = maps:get(<<"metadata">>, ListResp),
    ResourceVerstion = maps:get(<<"resourceVersion">>, Metadata),
    io:format("Version ~p~n", [ResourceVerstion]),
    WatchResp = swaggerl:op(API, WatchOp, [{<<"timeoutSeconds">>, 30}]),
    io:format("Watch ~p~n", [WatchResp]),
    ok;
watch(_API, _Op, []) ->
    ok.
