require "veil/credential_collection/base"
require "veil/credential_collection/chef_secrets_file"

module Veil
  class CredentialCollection

    def self.from_config(opts)
      klass = case opts[:provider]
              when 'chef-secrets-file'
                ChefSecretsFile
              else
                raise UnknownProvider, "Unknown provider: #{opts[:provider]}"
              end

      klass.new(opts)
    end
  end
end
