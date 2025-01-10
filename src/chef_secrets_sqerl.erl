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
    IdleCheck = envy:get(sqerl, idle_check, 1000, non_neg_integer),
    Statements = sqerl_config_env:read_statements_from_config(),
    {ok, Password} = chef_secrets:get(Section, Keyname),
    DbOptions = envy:get(sqerl, db_options, [], list),
    DbOptions1 = sqerl_config_env:handle_custom_ssl_options(DbOptions),

    %% The ip_mode key in the sqerl clause determines if we parse db_host as IPv4 or IPv6
    [{host, envy_parse:host_to_ip(sqerl, db_host)},
     {port, envy:get(sqerl, db_port, pos_integer)},
     {user, envy:get(sqerl, db_user, string)},
     {pass, Password},
     {db, envy:get(sqerl, db_name, string)},
     {extra_options, DbOptions1},
     {timeout, envy:get(sqerl,db_timeout, 5000, pos_integer)},
     {idle_check, IdleCheck},
     {prepared_statements, Statements},
     {column_transforms, envy:get(sqerl, column_transforms, list)}].
