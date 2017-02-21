%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Chef Software, Inc
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chef_secrets_json_file).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          secrets_file :: string(),
          secrets_data :: map()
         }).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Config) ->
    gen_server:start_link({local, chef_secrets_keyring_provider}, ?MODULE, Config, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Config) ->
    SecretsFile = proplists:get_value(secrets_file, Config),
    lager:info("chef_secrets: Reading secrets from ~s", [SecretsFile]),
    {ok, SecretsContentBin} = file:read_file(SecretsFile),
    Secrets = jiffy:decode(SecretsContentBin, [return_maps]),
    {ok, #state{secrets_file = SecretsFile,
                secrets_data = Secrets}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
