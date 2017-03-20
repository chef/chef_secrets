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
        import_credentials_hash(inflate_secrets_from_environment(var_name))
      end

      # Unsupported methods
      def rotate
        raise NotImplementedError
      end
      alias_method :rotate_credentials, :rotate
      alias_method :save, :rotate

      def inflate_secrets_from_environment(var_name)
        value = ENV[var_name]
        unless value
          msg = "Env var #{var_name} has not been set. This should by done by "\
            "launching this application via veil-env-wrapper."
          raise InvalidCredentialCollectionEnv.new(msg)
        end

        begin
          JSON.parse(value)
        rescue JSON::ParserError => e
          msg = "Env var #{var_name} could not be parsed: #{e.message}"
          raise InvalidCredentialCollectionEnv.new(msg)
        end
      ensure
        ENV.delete(var_name)
      end
    end
  end
end
