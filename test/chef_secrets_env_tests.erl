%% ex: ts=4 sw=4 et
%%
-module(chef_secrets_env_tests).
-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

read_returns_an_error_if_var_does_exist_test() ->
    os:unsetenv("DOES_NOT_EXIST"),
    ActualReturn = chef_secrets_env:read([{var_name, "DOES_NOT_EXIST"}]),
    ?assertEqual({error, env_var_not_found}, ActualReturn).

read_returns_ejson_from_packed_envvar_test() ->
    Path = filename:join([code:priv_dir(chef_secrets), "../test/json_secrets_file.json"]),
    CredJson = os:cmd(io_lib:format("veil-dump-secrets ~s", [Path])),
    os:putenv("CHEF_SECRETS_TEST_DATA", CredJson),
    {ok, Ejson} = chef_secrets_env:read([{var_name, "CHEF_SECRETS_TEST_DATA"}]),
    Actual = ej:get([<<"single_level_key">>], Ejson),
    ?assertEqual(<<"single_level_test_value">>, Actual).
