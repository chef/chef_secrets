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

get_returns_error_not_found_if_it_does_not_exists_test() ->
    application:set_env(chef_secrets, provider, chef_secrets_json_file),
    TestFilePath = filename:join(code:priv_dir(chef_secrets),
                                 "../test/json_secrets_file.json"),
    application:set_env(chef_secrets, provider_config, [{other, foo}, {secrets_file, TestFilePath}]),
    application:ensure_all_started(chef_secrets),
    ExpectedResponse = {error, not_found},
    ActualResponse = chef_secrets:get(<<"Not there">>),
    ?assertEqual(ExpectedResponse, ActualResponse).

get_returns_value_if_more_than_this_one_secret_exists_test() ->
    application:set_env(chef_secrets, provider, chef_secrets_json_file),
    TestFilePath = filename:join(code:priv_dir(chef_secrets),
                                 "../test/many_json_secrets_file.json"),
    application:set_env(chef_secrets, provider_config, [{other, foo}, {secrets_file, TestFilePath}]),
    application:ensure_all_started(chef_secrets),
    ExpectedResponse = {ok, <<"Value">>},
    ActualResponse = chef_secrets:get(<<"Name">>),
    ?assertEqual(ExpectedResponse, ActualResponse).
