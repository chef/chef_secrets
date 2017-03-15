%% ex: ts=4 sw=4 et
%%
-module(chef_secrets_tests).
-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

%%
%% Helper Functions
%%
secrets_file_path(Name) ->
    filename:join([code:priv_dir(chef_secrets), "../test/", Name]).

setup_application(SecretsFileName) ->
    Config = [ {other, foo},
               {secrets_file, secrets_file_path(SecretsFileName)} ],
    application:set_env(chef_secrets, provider, chef_secrets_json_file),
    application:set_env(chef_secrets, provider_config, Config),
    application:ensure_all_started(chef_secrets).

%%
%% Tests
%%
get_returns_a_nested_value_if_it_exists_test() ->
    setup_application("json_secrets_file.json"),
    ExpectedResponse = {ok, <<"37f5c43dd8bab089821e5a49fe0ac17128c6d1878fe632e687889eaaea1980b51b3e3df755d2610cffe6896c1df9f23bdda3">>},
    ActualResponse = chef_secrets:get(<<"postgresql">>, <<"db_superuser_password">>),
    ?assertEqual(ExpectedResponse, ActualResponse).

get_returns_a_single_level_value_if_it_exists_test() ->
    setup_application("json_secrets_file.json"),
    ExpectedResponse = {ok, <<"single_level_test_value">>},
    ActualResponse = chef_secrets:get(<<"single_level_key">>),
    ?assertEqual(ExpectedResponse, ActualResponse).

get_returns_error_not_found_if_it_does_not_exists_test() ->
    setup_application("json_secrets_file.json"),
    ExpectedResponse = {error, not_found},
    ActualResponse = chef_secrets:get(<<"postgresql">>, <<"not here">>),
    ?assertEqual(ExpectedResponse, ActualResponse).
