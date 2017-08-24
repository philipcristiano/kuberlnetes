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
    [UserPL | _] =  proplists:get_value("users", Config, []),
    User = proplists:get_value("user", UserPL, []),
    CertificatePath = proplists:get_value("client-certificate", User),
    KeyPath = proplists:get_value("client-key", User),

    % Load the spec
    SpecPath = Server ++ "/" ++ "swagger.json",
    SSLOptions = [{certfile, CertificatePath},
                  {keyfile, KeyPath}],
    HTTPOptions = [{ssl, SSLOptions}],
    Spec = swaggerl:load(SpecPath, HTTPOptions),

    % Set the API and return
    API = swaggerl:set_server(Spec, Server),
    API.
