-module(kuberlnetes_process).
-compile({parse_transform, lager_transform}).

-export([run/3]).

run(Command, Args, Env) ->
    % Use the Env PATH to find the executable!
    PATH = os:getenv("PATH", undefined),
    ok = lager:debug("Finding command with PATH of ~p", [PATH]),

    Exec = os:find_executable(Command, PATH),
    ok = lager:debug("Command ~p", [Exec]),
    Port = erlang:open_port({spawn_executable, Exec},
        [stream, stderr_to_stdout, binary, exit_status,
         {args, Args}, {env, Env}]),
    ok = lager:debug("Port started, entering IO loop"),
    Status = loop(Port, []),
    ok = lager:debug("Status ~p", [Status]),
    Status.

loop(Port, Acc) ->
    receive
        {Port, {exit_status, Status}} -> {Status, Acc};
        {Port, {data, Bytes}} -> loop(Port, Acc ++ Bytes);
        Msg -> ok = lager:debug("Unhandled loop message ~p", [Msg])
    end.

% output_to_var({data, {eol, Data}}) ->
    % ok = lager:debug("eol ~p", [Data]);
% output_to_var({data, {noeol, Data}}) ->
    % ok = lager:debug("noeol ~p", [Data]);
% output_to_var({data, Data}) ->
    % ok = lager:debug("boop data ~p", [Data]).
