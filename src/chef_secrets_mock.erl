%%%-------------------------------------------------------------------
%%% @Copyright (c) 2017-2025 Progress Software Corporation and/or its subsidiaries or affiliates. All Rights Reserved.
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chef_secrets_mock).

%% API
-export([read/1, write/1]).

%%%===================================================================
%%% API
%%%===================================================================
read(_) ->
    {ok, {[]}}.

write(_) -> {error, todo}.
