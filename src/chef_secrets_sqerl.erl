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
    [{pass, Password} | sqerl_config_env:config()].
