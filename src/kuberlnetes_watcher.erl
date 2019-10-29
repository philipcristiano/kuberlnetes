-module(kuberlnetes_watcher).

-export([watch/3]).

watch(API, {ListOp, WatchOp}, []) ->
    ListResp = swaggerl:op(API, ListOp, []),
    Metadata = maps:get(<<"metadata">>, ListResp),
    ResourceVerstion = maps:get(<<"resourceVersion">>, Metadata),
    io:format("Version ~p~n", [ResourceVerstion]),
    ParseFunc = swaggerl:async_op(API, WatchOp, []),
    io:format("Watch ~p~n", [ParseFunc]),

    loop(ParseFunc);
watch(_API, _Op, []) ->
    ok.

loop(Func) ->
    Msg = receive
        M -> io:format("Got message ~p~n", [M]),
             M
    end,
    Parsed = Func(Msg),
    io:format("Parsed ~p~n", [Parsed]),
    loop(Func).
