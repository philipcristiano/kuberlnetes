-module(kuberlnetes_process).

-export([run/3]).

run(Command, Args, Env) ->
    % Use the Env PATH to find the executable!
    PATH = os:getenv("PATH", "/bin:/usr/bin"),

    Exec = os:find_executable(Command, PATH),
    Port = erlang:open_port({spawn_executable, Exec},
        [stream, stderr_to_stdout, binary, exit_status,
         {args, Args}, {env, Env}]),
    Status = loop(Port, []),
    Status.

loop(Port, Acc) ->
    receive
        {Port, {exit_status, Status}} -> {Status, Acc};
        {Port, {data, Bytes}} -> loop(Port, Acc ++ Bytes);
        {Port, Msg} -> {error, unhandled_message, Msg}
    end.

% output_to_var({data, {eol, Data}}) ->
    % ok = lager:debug("eol ~p", [Data]);
% output_to_var({data, {noeol, Data}}) ->
    % ok = lager:debug("noeol ~p", [Data]);
% output_to_var({data, Data}) ->
    % ok = lager:debug("boop data ~p", [Data]).
