-module(chef_secrets_fd_tests).
-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

read_returns_an_error_if_var_does_exist_test() ->
    os:unsetenv("CHEF_SECRETS_FD"),
    ActualReturn = chef_secrets_fd:read([{}]),
    ?assertEqual({error, env_var_not_found}, ActualReturn).

read_returns_ejson_read_from_fd_referenced_in_env_var_test() ->
    Path = filename:join([code:priv_dir(chef_secrets), "../test/json_secrets_file.json"]),
    CredJson = os:cmd(io_lib:format("veil-dump-secrets ~s", [Path])),
    Fd = get_fd_from_content(CredJson),
    os:putenv("CHEF_SECRETS_FD", erlang:integer_to_list(Fd)),
    {ok, Ejson} = chef_secrets_fd:read([]),
    Actual = ej:get([<<"postgresql">>, <<"db_superuser_password">>], Ejson),
    ?assertMatch(<<"37f5c43dd8b", _/binary>>, Actual),
    os:unsetenv("CHEF_SECRETS_FD").

get_fd_from_content(Data) ->
    {A, B, C} = erlang:timestamp(),
    TmpFile = io_lib:format("/tmp/chef_secrets~w~w~w", [A, B, C]),
    ok = file:write_file(TmpFile, Data),
    {ok, {file_descriptor, _, {_Port, Fd}}} = file:open(TmpFile, [read, raw]),
    Fd.
