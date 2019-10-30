-module(kuberlnetes_watcher).

-export([watch/4]).

watch(Callback, API, Op, []) ->
    ListResp = swaggerl:op(API, Op, []),
    Items = maps:get(<<"items">>, ListResp),
    ok = callback_items(Callback, Items),
    Metadata = maps:get(<<"metadata">>, ListResp),
    ResourceVersion = maps:get(<<"resourceVersion">>, Metadata),

    WatchParams = [{<<"resourceversion">>, ResourceVersion},
                   {<<"watch">>, <<"true">>}],
    io:format("Params ~p~n", [WatchParams]),
    ParseFunc = swaggerl:async_op(API, Op, WatchParams),
    loop(Callback, ParseFunc).

loop(Callback, Func) ->
    Msg = receive
        M -> M
    end,
    Parsed = Func(Msg),
    case Parsed of
        ok -> ok;
        Obj -> Items = maps:get(<<"items">>, Obj),
               callback_items(Callback, Items)
    end,

    loop(Callback, Func).

callback_items(_Callback, []) ->
    ok;
callback_items(_Callback, ok) ->
    ok;
callback_items(Callback, [H|T]) ->
    Callback({list, H}),
    callback_items(Callback, T).
