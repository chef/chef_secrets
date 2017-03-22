require "veil/credential_collection/base"
require "veil/credential_collection/chef_secrets_fd"
require "veil/credential_collection/chef_secrets_file"
require "veil/credential_collection/chef_secrets_env"

module Veil
  class CredentialCollection

    def self.from_config(opts)
      klass = case opts[:provider]
              when 'chef-secrets-file'
                ChefSecretsFile
              when 'chef-secrets-env'
                ChefSecretsEnv
              when 'chef-secrets-fd'
                ChefSecretsFd
              else
                raise UnknownProvider, "Unknown provider: #{opts[:provider]}"
              end

      klass.new(opts)
    end
  end
end
