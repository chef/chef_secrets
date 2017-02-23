-module(chef_secrets).
-export([dump/0,
         get/1,
         get/2]).

-type secret() :: ej:json_term().

-spec get(binary()) -> {ok, secret()} | {error, any()}.
get(ServiceName) ->
    chef_secrets_keyring:get(ServiceName).

-spec get(binary(), binary()) -> {ok, secret()} | {error, any()}.
get(ServiceName, ItemName) ->
    case chef_secrets_keyring:get(ServiceName) of
        {ok, ServiceData} ->
            case ej:get([ItemName], ServiceData) of
                undefined ->
                    {error, not_found};
                Secret ->
                    {ok, Secret}
            end;
        Error -> Error
    end.

-spec dump() -> ej:json_object().
dump() ->
    chef_secrets_keyring:dump().
