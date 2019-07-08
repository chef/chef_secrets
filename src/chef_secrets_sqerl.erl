%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Chef Software, Inc
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chef_secrets_sqerl).

-export([config/1]).

-spec config({binary(), binary()}) -> proplists:proplist().
config({Section, Keyname}) ->
    {ok, Password} = chef_secrets:get(Section, Keyname),
    %% We set the env here because applications that use chef-secrets
    %% often don't set a password field in the sqerl config, but sqerl
    %% expects it to be there. We should probably fix that at some point
    application:set_env(sqerl, db_pass, ""),
    Config = [{pass, Password} | sqerl_config_env:config()],
    lists:keyreplace(pass, 1, Config, {pass, Password}).
