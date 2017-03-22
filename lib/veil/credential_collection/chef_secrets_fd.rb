require "veil/exceptions"
require "veil/credential_collection/base"
require "json"

module Veil
  class CredentialCollection
    class ChefSecretsFd < Base

      # Create a new ChefSecretsFd
      #
      # @param [Hash] opts
      #   ignored
      def initialize(opts = {})
        @credentials = {}
        import_credentials_hash(inflate_secrets_from_fd)
      end

      # Unsupported methods
      def rotate
        raise NotImplementedError
      end
      alias_method :rotate_credentials, :rotate
      alias_method :save, :rotate

      def inflate_secrets_from_fd
        if ENV['CHEF_SECRETS_FD'].nil?
          raise InvalidCredentialCollectionFd.new("CHEF_SECRETS_FD not found in environment")
        end

        fd = ENV['CHEF_SECRETS_FD'].to_i
        value = nil

        begin
          file = IO.new(fd, "r")
          value = file.gets
        rescue StandardError => e
          msg = "A problem occured trying to read passed file descriptor: #{e}"
          raise InvalidCredentialCollectionFd.new(msg)
        ensure
          file.close if file
        end

        if !value
          msg = "File at CHEF_SECRETS_FD (#{fd}) did not contain any data!"
          raise InvalidCredentialCollectionFd.new(msg)
        end

        begin
          JSON.parse(value)
        rescue JSON::ParserError => e
          msg = "Chef secrets data could not be parsed: #{e.message}"
          raise InvalidCredentialCollectionFd.new(msg)
        end
      end
    end
  end
end
