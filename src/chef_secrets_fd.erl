%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Chef Software, Inc
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chef_secrets_fd).

-export([read/1]).

read(_Config) ->
    case os:getenv("CHEF_SECRETS_FD") of
        false ->
            lager:error("Could not find CHEF_SECRETS_FD in environment", []),
            {error, env_var_not_found};
        Value ->
            Fd = erlang:list_to_integer(Value),
            lager:info("Reading secrets from file descriptor ~B", [Fd]),
            {ok, Content} = read_content(Fd),
            {ok, jiffy:decode(Content)}
    end.

read_content(Fd) ->
    Port = erlang:open_port({fd, Fd, Fd}, [binary, in, eof]),
    gather_data(Port, []).

gather_data(Port, Acc) ->
    receive
        {Port, eof} ->
            erlang:port_close(Port),
            {ok, erlang:iolist_to_binary(lists:reverse(Acc))};
        {Port, {data, Data}} ->
            gather_data(Port, [Data | Acc])
    after 2000 ->
        lager:error("timed out reading from fd"),
        {error, timeout}
    end.
