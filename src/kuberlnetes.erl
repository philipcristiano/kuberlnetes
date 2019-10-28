-module(kuberlnetes).

-export([load/0]).


load() ->
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
    HTTPOptions = [{ssl_options, SSLOptions}],
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
