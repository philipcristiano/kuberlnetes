-module(kuberlnetes).

-export([load/0,
         load_file/1,
         load_in_cluster/0]).


load() ->
    % Figure out where to load config file
    {ok, [[Home]]} = init:get_argument(home),
    DefaultPath = filename:join(Home, ".kube/config"),
    RelativeConfigPath = os:getenv("KUBECONFIG", DefaultPath),
    AbsConfigPath = filename:absname(RelativeConfigPath),
    case filelib:is_file(AbsConfigPath) of
        true -> load_file(AbsConfigPath);
        false -> load_in_cluster()
    end.

load_file(Path) ->
    % Load config
    [Config] = yamerl_constr:file(Path),

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
                  {keyfile, KeyPath},
                  {verify, verify_none}],
    HTTPOptions = [{ssl_options, SSLOptions}],
    Spec = swaggerl:load(SpecPath, HTTPOptions),

    % Set the API and return
    API = swaggerl:set_server(Spec, Server),
    API.

load_in_cluster() ->
    Host = os:getenv("KUBERNETES_SERVICE_HOST"),
    Port = os:getenv("KUBERNETES_SERVICE_PORT"),
    Server = "https://" ++ Host ++ ":" ++ Port,
    SpecPath = Server ++ "/" ++ "swagger.json",

    TokenFile = "/var/run/secrets/kubernetes.io/serviceaccount/token",
    {ok, Token} = file:read_file(TokenFile),

    AuthHeader = {<<"Authorization">>, << <<"Bearer: ">>/binary, Token/binary >>},
    DefaultHeaders = [AuthHeader],
    SSLOptions = [{verify, verify_none}],
    HTTPOptions = [{default_headers, DefaultHeaders}, {ssl_options, SSLOptions}],

    Spec = swaggerl:load(SpecPath, HTTPOptions),

    % Set the API and return
    API = swaggerl:set_server(Spec, Server),
    API.
