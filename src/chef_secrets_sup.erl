%%%-------------------------------------------------------------------
%% @doc chef_secrets top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chef_secrets_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, Provider} = application:get_env(chef_secrets, provider),
    {ok, ProviderOpts} = application:get_env(chef_secrets, provider_config),
    ChildSpec = {chef_secrets_keyring, {Provider, start_link, [ProviderOpts]},
                 permanent, brutal_kill, worker, [Provider]},
    {ok, { {one_for_one, 5, 10}, [ChildSpec]} }.

%%====================================================================
%% Internal functions
%%====================================================================
