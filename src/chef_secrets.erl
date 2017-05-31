-module(chef_secrets).
-export([dump/0,
         get/1,
         get/2]).

-type secret_value() :: ej:json_term().
-type secret()       :: binary().

-spec get(binary()) -> {ok, secret_value()} | {error, not_found}.
get(ServiceName) ->
    chef_secrets_keyring:get(ServiceName).

-spec get(binary(), binary()) -> {ok, secret()} | {error, not_found}.
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
