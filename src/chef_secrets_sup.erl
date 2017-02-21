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

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% ChildSpec :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, Provider} = application:get_env(chef_secrets, provider),
    {ok, ProviderOpts} = application:get_env(chef_secrets, provider_config),
    Config = [{provider, Provider} | ProviderOpts],
    ChildSpec = {chef_secrets_keyring, {chef_secrets_keyring, start_link, [Config]},
                 permanent, brutal_kill, worker, [chef_secrets_keyring, chef_secrets_json_file]},
    {ok, { {one_for_one, 5, 10}, [ChildSpec]} }.

%%====================================================================
%% Internal functions
%%====================================================================
