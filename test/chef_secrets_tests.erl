-module(chef_secrets_tests).
-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

get_returns_value_if_it_exists_test() ->
    application:set_env(chef_secrets, provider, chef_secrets_json_file),
    TestFilePath = filename:join(code:priv_dir(chef_secrets),
                                 "../test/json_secrets_file.json"),
    application:set_env(chef_secrets, provider_config, [{other, foo}, {secrets_file, TestFilePath}]),
    application:ensure_all_started(chef_secrets),
    ExpectedResponse = {ok, <<"Value">>},
    ActualResponse = chef_secrets:get(<<"Name">>),
    ?assertEqual(ExpectedResponse, ActualResponse).
