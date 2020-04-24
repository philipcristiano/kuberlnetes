-module(kuberlnetes_config_test).
-include_lib("eunit/include/eunit.hrl").

-define(MUT, kuberlnetes).

-define(TOKEN_PATH, "/var/run/secrets/kubernetes.io/serviceaccount/token").

exec_with_token_and_cluster_ca_test() ->
    meck:new(filelib, [unstick]),
    meck:new(base64, [unstick]),
    meck:new(yamerl_constr),
    meck:new(kuberlnetes_process),
    meck:new(swaggerl),
    meck:new(public_key),

    Server = "test_server",
    B64CAData = make_ref(),
    CAData = make_ref(),
    PemEntry = [{"type", "test_dir", "encrypt"}],
    KubeConfig = [
      {"clusters", [[
        {"cluster", [
          {"certificate-authority-data", B64CAData},
          {"server", Server}
        ]}
      ]]},
      {"users", [[
        {"user", [
          {"exec", [
            {"command", "aws"},
            {"args", [a, b, c]}
          ]}
        ]}
      ]]}
    ],

    TokenData = "test_token_data",
    ExecAuthData = [
      {"status", [{"token", TokenData}]}
    ],

    % Exec isn't in the cluster
    PortReturn = make_ref(),
    APIRef = make_ref(),
    meck:expect(filelib, is_file, fun (?TOKEN_PATH) -> false end),
    meck:expect(yamerl_constr, file, fun (_Path) -> [KubeConfig] end),
    meck:expect(base64, decode, fun(B64Data) ->
        ?assertEqual(B64CAData, B64Data),
        CAData
    end),
    meck:expect(public_key, pem_decode, fun(PemData) ->
        ?assertEqual(CAData, PemData),
        PemEntry
    end),
    meck:expect(kuberlnetes_process, run, fun (_C, _A, _E) -> {0, PortReturn} end),
    meck:expect(yamerl_constr, string, fun (Arg) ->
      PortReturn = Arg,
      [ExecAuthData] end),

    BToken = erlang:list_to_binary(TokenData),
    ExpectedAuthHeader = {<<"Authorization">>, << <<"Bearer ">>/binary, BToken/binary >>},
    ExpectedOptions = [
      {ssl_options, [{cacerts, ["test_dir"]}]},
      {default_headers, [ExpectedAuthHeader]}],

    meck:expect(swaggerl, load, fun (SpecPath, Options) ->
        ?assertEqual(Server ++ "/openapi/v2", SpecPath),
        ?assertEqual(ExpectedOptions, Options),
        APIRef
    end),

    meck:expect(swaggerl, set_server, fun(Spec, ServerArg) ->
        ?assertEqual(APIRef, Spec),
        ?assertEqual(Server, ServerArg)
    end),

    API = kuberlnetes:load(),

    ?assertEqual(ok, API),

    meck:validate(swaggerl),
    meck:unload(swaggerl),
    meck:validate(public_key),
    meck:unload(public_key),
    meck:validate(kuberlnetes_process),
    meck:unload(kuberlnetes_process),
    meck:validate(yamerl_constr),
    meck:unload(yamerl_constr),
    meck:validate(base64),
    meck:unload(base64),
    meck:validate(filelib),
    meck:unload(filelib),
    ok.

exec_with_token_test() ->
    meck:new(filelib, [unstick]),
    meck:new(base64, [unstick]),
    meck:new(yamerl_constr),
    meck:new(kuberlnetes_process),
    meck:new(swaggerl),
    meck:new(public_key),

    Server = "test_server",
    B64CAData = make_ref(),
    CAData = make_ref(),
    PemEntry = [{"type", "test_dir", "encrypt"}],
    KubeConfig = [
      {"clusters", [[
        {"cluster", [
          {"server", Server}
        ]}
      ]]},
      {"users", [[
        {"user", [
          {"exec", [
            {"command", "aws"},
            {"args", [a, b, c]}
          ]}
        ]}
      ]]}
    ],

    TokenData = "test_token_data",
    ExecAuthData = [
      {"status", [{"token", TokenData}]}
    ],

    % Exec isn't in the cluster
    PortReturn = make_ref(),
    APIRef = make_ref(),
    meck:expect(filelib, is_file, fun (?TOKEN_PATH) -> false end),
    meck:expect(yamerl_constr, file, fun (_Path) -> [KubeConfig] end),
    meck:expect(base64, decode, fun(B64Data) ->
        ?assertEqual(B64CAData, B64Data),
        CAData
    end),
    meck:expect(public_key, pem_decode, fun(PemData) ->
        ?assertEqual(CAData, PemData),
        PemEntry
    end),
    meck:expect(kuberlnetes_process, run, fun (_C, _A, _E) -> {0, PortReturn} end),
    meck:expect(yamerl_constr, string, fun (Arg) ->
      PortReturn = Arg,
      [ExecAuthData] end),

    BToken = erlang:list_to_binary(TokenData),
    ExpectedAuthHeader = {<<"Authorization">>, << <<"Bearer ">>/binary, BToken/binary >>},
    ExpectedOptions = [
      {ssl_options, []},
      {default_headers, [ExpectedAuthHeader]}],

    meck:expect(swaggerl, load, fun (SpecPath, Options) ->
        ?assertEqual(Server ++ "/openapi/v2", SpecPath),
        ?assertEqual(ExpectedOptions, Options),
        APIRef
    end),

    meck:expect(swaggerl, set_server, fun(Spec, ServerArg) ->
        ?assertEqual(APIRef, Spec),
        ?assertEqual(Server, ServerArg)
    end),

    API = kuberlnetes:load(),

    ?assertEqual(ok, API),

    meck:validate(swaggerl),
    meck:unload(swaggerl),
    meck:validate(public_key),
    meck:unload(public_key),
    meck:validate(kuberlnetes_process),
    meck:unload(kuberlnetes_process),
    meck:validate(yamerl_constr),
    meck:unload(yamerl_constr),
    meck:validate(base64),
    meck:unload(base64),
    meck:validate(filelib),
    meck:unload(filelib),
    ok.
