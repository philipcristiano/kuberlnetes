-module(kuberlnetes).
-include_lib("kernel/include/logger.hrl").

-export([load/0,
         load/1,
         spawn_watch/4,
         watch/3,
         watch/4]).

-define(TOKEN_PATH, "/var/run/secrets/kubernetes.io/serviceaccount/token").

watch(Callback, API, Op) ->
    watch(Callback, API, Op, []).

watch(Callback, API, Op, []) ->
    kuberlnetes_watcher:watch(Callback, API, Op, []).

spawn_watch(Callback, API, Op, []) ->
    erlang:spawn_link(kuberlnetes_watcher, watch, [Callback, API, Op, []]).

load() ->
    load([]).
load(Options) ->
    InCluster = filelib:is_file(?TOKEN_PATH),
    case InCluster of
        true -> load_in_cluster(Options);
        false -> load_kubeconfig(Options)
    end.

load_in_cluster(Options) ->
    ?LOG_DEBUG(#{msg => "Attempting in cluster kubernetes configuration"}),
    {ok, Token} = file:read_file(?TOKEN_PATH),
    CAFILE = "/var/run/secrets/kubernetes.io/serviceaccount/ca.crt",
    AuthHeaders = [
      {<<"Authorization">>, << <<"Bearer ">>/binary, Token/binary >>}],
    SSLOpts = [{ssl_options, [{cacertfile, CAFILE}]}],
    HeaderOption = [{default_headers, AuthHeaders}],
    AllOptions = SSLOpts ++ HeaderOption ++ Options,

    Server = "https://kubernetes.default.svc",
    SpecPath = Server ++ "/openapi/v2",
    Spec = swaggerl:load(SpecPath, AllOptions),
    API = swaggerl:set_server(Spec, Server),
    API.

load_kubeconfig(Options) ->
    ?LOG_DEBUG(#{msg => "Attempting kubeconfig kubernetes configuration"}),
    % Figure out where to load config file
    {ok, [[Home]]} = init:get_argument(home),
    DefaultPath = filename:join(Home, ".kube/config"),
    RelativeConfigPath = os:getenv("KUBECONFIG", DefaultPath),
    AbsConfigPath = filename:absname(RelativeConfigPath),

    % Load config
    [Config] = yamerl_constr:file(AbsConfigPath),

    % Get the server
    [ClusterPL | _] = proplists:get_value("clusters", Config, []),
    Cluster = proplists:get_value("cluster", ClusterPL, []),
    Server = proplists:get_value("server", Cluster, undefined),

    % Get user cert/key
    {Certificate, Key} = get_credentials(Config),
    % Load the spec
    SpecPath = Server ++ "/openapi/v2",
    SSLOptions = [{cert, Certificate},
                  {key, Key} ],
    HTTPOptions = [{ssl_options, SSLOptions}] ++ Options,
    Spec = swaggerl:load(SpecPath, HTTPOptions),

    % Set the API and return
    API = swaggerl:set_server(Spec, Server),
    API.

get_credentials(Config) ->
    [UserPL | _] =  proplists:get_value("users", Config, []),
    User = proplists:get_value("user", UserPL, []),
    get_exec_credentials(User).

    % CertificatePath = proplists:get_value("client-certificate", User),
    % KeyPath = proplists:get_value("client-key", User),

    % {CertificatePath, KeyPath}.

get_exec_credentials(User) ->
    Exec = proplists:get_value("exec", User),
    Args = proplists:get_value("args", Exec),
    Cmd = proplists:get_value("command", Exec),
    Env = [],

    {0, ConfigBytes} = kuberlnetes_process:run(Cmd, Args, Env),
    [Config] = yamerl_constr:string(ConfigBytes),
    ConfigStatus = proplists:get_value("status", Config),

    Cert = proplists:get_value("clientCertificateData", ConfigStatus),
    {_type, DerCert} = list_to_der(Cert),

    Key = proplists:get_value("clientKeyData", ConfigStatus),
    DerKey = list_to_der(Key),
    {DerCert, DerKey}.

list_to_der(L) ->
    Bin = erlang:list_to_binary(L),
    [PemEntry] = public_key:pem_decode(Bin),
    {Type, Der, _encrypt} = PemEntry,
    {Type, Der}.
