%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Chef Software, Inc
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(chef_secrets_json_file).

%% API
-export([read/1, write/1]).

%%%===================================================================
%%% API
%%%===================================================================
read(Config) ->
    SecretsFile = proplists:get_value(secrets_file, Config),
    lager:info("Reading secrets from ~s", [SecretsFile]),
    {ok, ContentBin} = file:read_file(SecretsFile),
    VeilEjson = jiffy:decode(ContentBin),
    CredentialsHash = process_veil_ejson(VeilEjson),
    {ok, CredentialsHash}.

process_veil_ejson(VeilEjson) ->
    case ej:get([<<"veil">>], VeilEjson) of
        undefined ->
            VeilEjson;
        V ->
            Version = ej:get([<<"version">>], V),
            Credentials = process_veil_ejson(Version, V),
            flatten_veil_creds(Credentials)
    end.

process_veil_ejson(1, Ejson) ->
    ej:get([<<"credentials">>], Ejson);
process_veil_ejson(2, Ejson) ->
    EncryptedCredentials = ej:get([<<"credentials">>], Ejson),
    EncryptionConfig = ej:get([<<"cipher">>], Ejson),
    decrypt_credentials(EncryptedCredentials, EncryptionConfig);
process_veil_ejson(_Unknown, _Ejson) ->
    {error, unknown_veil_version}.

decrypt_credentials(Ciphertext, Config) ->
    Key = base64:decode(ej:get([<<"key">>], Config)),
    Iv = base64:decode(ej:get([<<"iv">>], Config)),
    PlainText = crypto:block_decrypt(aes_cbc256, Key, Iv, base64:decode(Ciphertext)),
    jiffy:decode(unpad(PlainText)).

flatten_veil_creds({CredList}) ->
    flatten_veil_creds(CredList, {[]}).

flatten_veil_creds([], Acc) ->
    {Acc};
flatten_veil_creds([Head |Rest], Acc) ->
    case Head of
        %% Matches a group of credentials
        {_GroupName, {KeyList = [{_KeyName, {_VeilData}}|_MoreKeys]}} ->
            flatten_veil_creds(Rest, process_veil_data(KeyList, Acc));
        %% Matches a single-level credential
        Key = {_KeyName, {_VeilData}} ->
            flatten_veil_creds(Rest, process_veil_data([Key], Acc))
        end.

process_veil_data([], EJAcc) ->
    EJAcc;
process_veil_data([{_KeyName, VeilData}|Rest], EJAcc) ->
    Name = ej:get([<<"name">>], VeilData),
    Group = ej:get([<<"group">>], VeilData),
    Value = ej:get([<<"value">>], VeilData),
    case Group of
        null ->
            process_veil_data(Rest, ej:set_p([Name], EJAcc, Value));
        G->
            process_veil_data(Rest, ej:set_p([G, Name], EJAcc, Value))
    end.

%%
%% Erlang's crypto library doesn't unpad the data for us, so we do it
%% ourselves.
%%
-spec unpad(binary()) -> binary().
unpad(Binary) when is_binary(Binary) ->
    Last = binary:last(Binary),
    Length = byte_size(Binary),
    binary:part(Binary, 0, Length - Last).

write(_) -> {error, todo}.
