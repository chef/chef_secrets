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
get_returns_value_if_it_exists_test() ->
    setup_application("json_secrets_file.json"),
    ExpectedResponse = {ok, <<"Value">>},
    ActualResponse = chef_secrets:get(<<"Name">>),
    ?assertEqual(ExpectedResponse, ActualResponse).

get_returns_error_not_found_if_it_does_not_exists_test() ->
    setup_application("json_secrets_file.json"),
    ExpectedResponse = {error, not_found},
    ActualResponse = chef_secrets:get(<<"Not there">>),
    ?assertEqual(ExpectedResponse, ActualResponse).

get_returns_value_if_more_than_this_one_secret_exists_test() ->
    setup_application("many_json_secrets_file.json"),
    ExpectedResponse = {ok, <<"Value">>},
    ActualResponse = chef_secrets:get(<<"Name">>),
    ?assertEqual(ExpectedResponse, ActualResponse).
