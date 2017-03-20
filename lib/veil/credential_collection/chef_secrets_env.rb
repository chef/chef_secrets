require "veil/credential_collection/base"
require "json"

module Veil
  class CredentialCollection
    class ChefSecretsEnv < Base

      # Create a new ChefSecretsEnv
      #
      # @param [Hash] opts
      #   a hash of options to pass to the constructor
      def initialize(opts = {})
        var_name = opts[:var_name] || 'CHEF_SECRETS_DATA'

        @credentials = {}
        import_credentials_hash(read_env_var(var_name))
      end

      # Unsupported methods
      def rotate
        raise NotImplementedError
      end
      alias_method :rotate_credentials, :rotate
      alias_method :save, :rotate

      def read_env_var(var_name)
        value = ENV[var_name]
        unless value
          raise InvalidCredentialCollectionEnv.new("Env var #{var_name} does not exist")
        end

        JSON.parse(value)
      ensure
        ENV.delete(var_name)
      end
    end
  end
end
