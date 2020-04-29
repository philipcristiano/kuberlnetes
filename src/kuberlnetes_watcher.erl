-module(kuberlnetes_watcher).

-export([watch/4]).

watch(Callback, API, Op, []) ->
    ListResp = swaggerl:op(API, Op, []),
    Items = maps:get(<<"items">>, ListResp),
    ok = callback_items(Callback, Items),
    Metadata = maps:get(<<"metadata">>, ListResp),
    ResourceVersion = maps:get(<<"resourceVersion">>, Metadata),

    WatchParams = [{<<"resourceVersion">>, ResourceVersion},
                   {<<"watch">>, <<"true">>}],
    ParseFunc = swaggerl:async_op(API, Op, WatchParams),
    loop(Callback, ParseFunc).

loop(Callback, Func) ->
    Msg = receive
        M -> M
    end,
    Parsed = Func(Msg),
    case Parsed of
        ok -> ok;
        {status, 200} -> ok;
        done -> ok;
        Objects -> callback_message(Callback, Objects)
    end,

    loop(Callback, Func).

callback_message(_Callback, []) ->
    ok;
callback_message(Callback, [Msg | Rest]) ->
    Type = maps:get(<<"type">>, Msg),
    Obj = maps:get(<<"object">>, Msg),

    Callback({Type, Obj}),
    callback_message(Callback, Rest).

callback_items(_Callback, []) ->
    ok;
callback_items(_Callback, ok) ->
    ok;
callback_items(Callback, [H|T]) ->
    Callback({list, H}),
    callback_items(Callback, T).
