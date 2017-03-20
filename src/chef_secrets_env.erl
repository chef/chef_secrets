%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Chef Software, Inc
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chef_secrets_env).

-export([read/1]).

read(Config) ->
    EnvVarName = proplists:get_value(var_name, Config, "CHEF_SECRETS_DATA"),
    case os:getenv(EnvVarName) of
        false ->
            lager:error("Could not find ~s in environment", [EnvVarName]),
            {error, env_var_not_found};
        Value ->
            lager:info("Reading secrets from environment variable ~s", [EnvVarName]),
            os:unsetenv(EnvVarName),
            {ok, jiffy:decode(Value)}
    end.
