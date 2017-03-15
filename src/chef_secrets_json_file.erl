%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Chef Software, Inc
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chef_secrets_json_file).

%% API
-export([read/1, write/1]).

%%%===================================================================
%%% API
%%%===================================================================
read(Config) ->
    SecretsFile = proplists:get_value(secrets_file, Config),
    lager:info("Reading secrets from ~s via veil helper command", [SecretsFile]),
    VeilHelperOutput = os:cmd(io_lib:format("veil-dump-secrets ~s", [SecretsFile])),
    CredentialsHash = jiffy:decode(VeilHelperOutput),
    {ok, CredentialsHash}.

write(_) -> {error, todo}.
