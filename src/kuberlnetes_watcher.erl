-module(kuberlnetes_watcher).

-export([watch/4]).

watch(Callback, API, {ListOp, WatchOp}, []) ->
    ListResp = swaggerl:op(API, ListOp, []),
    Items = maps:get(<<"items">>, ListResp),
    ok = callback_items(Callback, Items),
    Metadata = maps:get(<<"metadata">>, ListResp),
    ResourceVerstion = maps:get(<<"resourceVersion">>, Metadata),
    io:format("Version ~p~n", [ResourceVerstion]),
    ParseFunc = swaggerl:async_op(API, WatchOp, []),
    io:format("Watch ~p~n", [ParseFunc]),
    loop(Callback, ParseFunc).

loop(Callback, Func) ->
    Msg = receive
        M -> M
    end,
    Parsed = Func(Msg),
    Callback({loop, Parsed}),

    loop(Callback, Func).

callback_items(_Callback, []) ->
    ok;
callback_items(Callback, [H|T]) ->
    Callback({list, H}),
    callback_items(Callback, T).
