-module(chef_secrets).
-export([get/1]).

get(Name) ->
    chef_secrets_keyring:get(Name).
