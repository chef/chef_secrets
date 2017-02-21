%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Chef Software, Inc
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chef_secrets_keyring).

-behaviour(gen_server).

%% API
-export([start_link/1, get/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {provider,
                secrets_data
               }).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

get(Name) when is_binary(Name) ->
    gen_server:call(?MODULE, {get, Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Config) ->
    Provider = proplists:get_value(provider, Config),
    {ok, Secrets} = Provider:read(Config),
    {ok, #state{provider = Provider,
                secrets_data = Secrets}}.

handle_call({get, Name}, _From, #state{secrets_data = Secrets} = State) ->
    Reply = case ej:get([Name], Secrets) of
              undefined ->
                lager:warning("Could not find secret ~s", [Name]),
                {error, not_found};
              Secret ->
                {ok, Secret}
            end,
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
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
