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
    Opts = request_operations_for_token(Token),
    SSLOpts = [{ssl_options, [{cacertfile, CAFILE}]}],
    HeaderOption = [{default_headers, maps:get(headers, Opts)}],
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
    [ClusterPL | _] = proplists:get_value("clusters", Config, [[], []]),
    Cluster = proplists:get_value("cluster", ClusterPL, []),
    Server = proplists:get_value("server", Cluster, undefined),

    CertAuthKey = "certificate-authority-data",
    ClusterSSLOpts = case proplists:is_defined(CertAuthKey, Cluster) of
        false -> [];
        true -> B64 = proplists:get_value(CertAuthKey, Cluster),
                      CACertPem = base64:decode(B64),
                      {_T, CACertDer} = binary_to_der(CACertPem),
                      [{cacerts, [CACertDer]}]
    end,

    % Get user cert/key/token
    ReqOpts = get_credentials(Config),
    HTTPSSLOpts = case maps:is_key(ssl_options, ReqOpts) of
       false -> [{ssl_options, ClusterSSLOpts}];
       true ->  ReqSSLOpts = maps:get(ssl_options, ReqOpts),
                [{ssl_options, ReqSSLOpts ++ ClusterSSLOpts}]
    end,

    HTTPTokenOpts = case maps:is_key(headers, ReqOpts) of
       false -> [];
       true -> [{default_headers, maps:get(headers, ReqOpts)}]
    end,

    HTTPOptions = HTTPSSLOpts ++ HTTPTokenOpts ++ Options,

    % Load the spec
    SpecPath = Server ++ "/openapi/v2",
    Spec = swaggerl:load(SpecPath, HTTPOptions),

    % Set the API and return
    API = swaggerl:set_server(Spec, Server),
    API.

get_credentials(Config) ->
    [UserPL | _] =  proplists:get_value("users", Config),
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


    ReqOpts = case proplists:is_defined("token", ConfigStatus) of
        true -> Token = proplists:get_value("token", ConfigStatus),
                request_operations_for_token(Token);
        false -> Cert = proplists:get_value(
                            "clientCertificateData",
                            ConfigStatus),
                Key = proplists:get_value("clientKeyData", ConfigStatus),
                request_operations_for_cert(Cert, Key)
    end,
    ReqOpts.

request_operations_for_token(Token) when is_list(Token)->
    BToken = erlang:list_to_binary(Token),
    request_operations_for_token(BToken);
request_operations_for_token(Token) when is_binary(Token)->
    Headers = [
      {<<"Authorization">>, << <<"Bearer ">>/binary, Token/binary >>}],
    #{headers => Headers}.

request_operations_for_cert(Cert, Key) ->
    {_type, DerCert} = list_to_der(Cert),
    DerKey = list_to_der(Key),
    SSLOptions = [{cert, DerCert},
                  {key, DerKey} ],
    #{ssl_options => SSLOptions}.

list_to_der(L) when is_list(L) ->
    Bin = erlang:list_to_binary(L),
    Der = binary_to_der(Bin),
    Der.

binary_to_der(Bin) ->
    [PemEntry] = public_key:pem_decode(Bin),
    {Type, Der, _encrypt} = PemEntry,
    {Type, Der}.
