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
    lager:info("Reading secrets from ~s", [SecretsFile]),
    {ok, ContentBin} = file:read_file(SecretsFile),
    SecretsEjson = jiffy:decode(ContentBin),
    {ok, SecretsEjson}.

write(_) -> {error, todo}.
