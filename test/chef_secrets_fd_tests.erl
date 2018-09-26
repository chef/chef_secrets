-module(chef_secrets_fd_tests).
-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

read_returns_an_error_if_var_does_exist_test() ->
    os:unsetenv("CHEF_SECRETS_FD"),
    ActualReturn = chef_secrets_fd:read([{}]),
    ?assertEqual({error, env_var_not_found}, ActualReturn).

%%
%% Note: The workaround we use for the OTP21 changes only works on linux, because it uses /proc. Expect this test to fail
%% on other operating systems.
%%
read_returns_ejson_read_from_fd_referenced_in_env_var_test() ->
    Path = filename:join([code:priv_dir(chef_secrets), "../test/json_secrets_file.json"]),
    CredJson = os:cmd(io_lib:format("veil-dump-secrets ~s", [Path])),
    %% Note: OTP 21 detects leaked filehandles and GC's them fast, so we must hang onto the Fh for the duration of the test
    {Fh, Fd} = get_fd_from_content(CredJson),
    os:putenv("CHEF_SECRETS_FD", erlang:integer_to_list(Fd)),
    {ok, Ejson} = chef_secrets_fd:read([]),
    Actual = ej:get([<<"postgresql">>, <<"db_superuser_password">>], Ejson),
    ?assertMatch(<<"37f5c43dd8b", _/binary>>, Actual),
    os:unsetenv("CHEF_SECRETS_FD"),
    file:close(Fh).

get_fd_from_content(Data) ->
    {A, B, C} = erlang:timestamp(),
    TmpFile = io_lib:format("/tmp/chef_secrets~w~w~w", [A, B, C]),
    ok = file:write_file(TmpFile, Data),
    {ok, IoDevice} = file:open(TmpFile, [read, raw]),
    Fd = get_fd_from_io_device(TmpFile, IoDevice),
    {IoDevice, Fd}.


-ifdef(open_returns_map).
get_fd_from_io_device(Name, _) ->
    %% I am ashamed of this hack.  The problem is that OTP-21 rewrote
    %% raw files to use a nif. This has the effect of burying the file
    %% descriptor deep in an opaque reference, and I've not spotted a
    %% sane way to extract that info.
    %% So the hack relies on exploiting linux process info to find our fd.
    FName = lists:flatten(Name),
    Fd = find_fd_for_target(FName),
    Fd.
-else.
get_fd_from_io_device(_, IoDevice) ->
    %% Somewhat fragile parsing of erlang internal structures, works only in OTP < 21
    {file_descriptor, _, {_Port, Fd}} = IoDevice,
    Fd.
-endif.


-define(PROCDIR, "/proc/self/fd").
get_filedesc_target(Fd) ->
    case file:read_link(filename:join(?PROCDIR,Fd)) of
        {ok, Name} ->
            Name;
        {error, Error} ->
            Error
    end.

find_fd_for_target(Target) ->
    {ok, Files} = file:list_dir(?PROCDIR),
    %% lists:reverse is a hack to get the most recently opened
    %% filehandle assuming the numbers are increasing. Not really
    %% trustworthy, since the strategy is first-avail.
    [H|_] = lists:filter(fun(F) ->
                                 Target == get_filedesc_target(F)
                         end,
                         lists:reverse(Files)),
    erlang:list_to_integer(H).
