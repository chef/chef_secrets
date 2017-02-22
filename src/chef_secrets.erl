-module(chef_secrets).
-export([dump/0,
         get/1,
         get/2]).

get(ServiceName) ->
    chef_secrets_keyring:get(ServiceName).

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

dump() ->
    chef_secrets_keyring:dump().
