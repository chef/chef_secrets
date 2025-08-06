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
        # Check if we already have cached data for this FD
        fd = ENV['CHEF_SECRETS_FD'].to_i
        cache_key = "chef_secrets_fd_#{fd}_#{Process.pid}"
        
        if defined?(@@fd_cache) && @@fd_cache && @@fd_cache[cache_key]
          STDERR.puts "DEBUG: Using cached data for FD #{fd}" if ENV['DEBUG']
          return @@fd_cache[cache_key]
        end
        
        if ENV['CHEF_SECRETS_FD'].nil?
          raise InvalidCredentialCollectionFd.new("CHEF_SECRETS_FD not found in environment")
        end

        value = nil

        # Add debugging
        STDERR.puts "DEBUG: Attempting to read from FD #{fd}" if ENV['DEBUG']
        STDERR.puts "DEBUG: Process PID: #{Process.pid}" if ENV['DEBUG']
        STDERR.puts "DEBUG: Available FDs: #{Dir.glob('/proc/self/fd/*').map{|f| File.basename(f)}.join(', ')}" if ENV['DEBUG'] && File.exist?('/proc/self/fd')

        begin
          file = IO.new(fd, "r")
          STDERR.puts "DEBUG: Successfully opened FD #{fd}" if ENV['DEBUG']
          value = file.gets
          STDERR.puts "DEBUG: Read #{value ? value.length : 'nil'} chars from FD #{fd}" if ENV['DEBUG']
        rescue StandardError => e
          STDERR.puts "DEBUG: Failed to read FD #{fd}: #{e.class} - #{e.message}" if ENV['DEBUG']
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
          parsed_data = JSON.parse(value)
          # Cache the parsed data
          @@fd_cache ||= {}
          @@fd_cache[cache_key] = parsed_data
          STDERR.puts "DEBUG: Cached data for FD #{fd}" if ENV['DEBUG']
          return parsed_data
        rescue JSON::ParserError => e
          msg = "Chef secrets data could not be parsed: #{e.message}"
          raise InvalidCredentialCollectionFd.new(msg)
        end
      end
    end
  end
end