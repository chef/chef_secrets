require "veil/hasher"
require "veil/credential"
require "forwardable"

module Veil
  class CredentialCollection
    class Base
      class << self
        def create(hash = {})
          new(hash)
        end
      end

      extend Forwardable

      attr_reader :credentials, :hasher, :version

      def_delegators :@credentials, :size, :length, :find, :map, :select, :each, :[], :keys

      def initialize(opts = {})
        @hasher = Veil::Hasher.create(opts[:hasher] || {})
        @credentials = expand_credentials_hash(opts[:credentials] || {})
        @version = opts[:version] || 1
      end

      def to_hash
        {
          type: self.class.name,
          version: version,
          hasher: hasher.to_h,
          credentials: credentials_as_hash
        }
      end
      alias_method :to_h, :to_hash

      def save
        raise "Save has not been implemented for this class"
      end

      def rotate(group_or_cred, cred = nil)
        if cred && credentials.key?(group_or_cred) && credentials[group_or_cred].key?(cred)
          credentials[group_or_cred][cred].rotate(hasher)
        elsif credentials.key?(group_or_cred)
          if credentials[group_or_cred].is_a?(Hash)
            credentials[group_or_cred].each { |_s, c| c.rotate(hasher) }
          else
            credentials[group_or_cred].rotate(hasher)
          end
        end
      end

      #
      # Retrieves a credential from the credential store:
      #
      #  get(name)
      #  get(group, name)
      #
      def get(*args)
        case args.length
        when 1
          cred_name = args[0]
          c = credentials[cred_name]
          if c.nil?
            raise Veil::CredentialNotFound, "Credential '#{cred_name}' not found."
          else
            c.value
          end
        when 2
          group_name = args[0]
          cred_name = args[1]

          g = credentials[group_name]
          if g.nil?
            raise Veil::GroupNotFound, "Credential group '#{group_name}' not found."
          else
            c = g[cred_name]
            if c.nil?
              raise Veil::CredentialNotFound, "Credential '#{cred_name}' not found in group '#{group_name}'."
            else
              c.value
            end
          end
        else
          raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 1 or 2)"
        end
      end

      # TODO mp 2017/03/01 replace usage to prefer get_credential
      alias_method :get_credential, :get

      #
      # Check to see if a given credential has been added.
      #
      def exist?(*args)
        get(*args)
        true
      rescue Veil::GroupNotFound, Veil::CredentialNotFound
        false
      end


      # Add a new credential to the credentials
      #
      # @param [Hash] args
      #
      def add(*args)
        params = { name: nil, group: nil, length: 128, value: nil, force: false }
        case args.length
        when 1
          # add('foo')
          params[:name] = args.first
        when 2
          if args.all? { |a| a.is_a?(String) }
            # add('my_app', 'foo')
            params[:group], params[:name] = args
          elsif args[1].is_a?(Hash)
            # add('my_app', value: 'something')
            # add('foo', length: 50)
            params[:name] = args.first
            params.merge!(args[1])
          end
        when 3
          # add('my_app', 'foo', value: 'something')
          # add('my_app', 'foo', length: 50)
          params[:group], params[:name] = args[0], args[1]
          params.merge!(args[2])
        else
          raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 1-3)"
        end

        params[:name] = params[:name].to_s

        if params[:value]
          params[:length] = params[:value].length
        else
          params[:value] = hasher.encrypt(params[:group], params[:name], 0)
        end

        if params[:group]
          credentials[params[:group]] ||= {}

          return credentials[params[:group]][params[:name]] if credentials[params[:group]].key?(params[:name]) && !params[:force]

          credentials[params[:group]][params[:name]] = Veil::Credential.new(params)
        else
          return credentials[params[:name]] if credentials.key?(params[:name]) && !params[:force]

          credentials[params[:name]] = Veil::Credential.new(params)
        end
      end
      alias_method :<<, :add

      # Add the contents of a file as a credential after
      # verifying that the file can be read.
      # Usage:
      #   add_from_file(filename, "secretname")
      #   add_from_file(filename, "groupname", "secretname")
      #
      #  Anything added from file will automatically be frozen.
      #`add`'s options are not supported.
      #
      def add_from_file(filepath, *args)
        unless File.readable?(filepath)
          raise Veil::FileNotReadable.new("Cannot read #{filepath}")
        end
        add(*args, value: File.read(filepath),
                   frozen: true)
      end

      def remove(group_or_cred, cred = nil)
        if group_or_cred && cred && credentials.key?(group_or_cred)
          credentials[group_or_cred].delete(cred)
        else
          credentials.delete(group_or_cred)
        end
      end
      alias_method :delete, :remove

      def rotate_hasher
        @hasher = Veil::Hasher.create
        rotate_credentials
      end

      def rotate_credentials
        credentials.each do |cred_or_group_name, cred_or_group|
          if cred_or_group.is_a?(Veil::Credential)
            cred_or_group.rotate(hasher)
          else
            cred_or_group.each { |_group, cred| cred.rotate(hasher) }
          end
        end
      end

      private

      def expand_credentials_hash(creds_hash)
        expanded = Hash.new

        creds_hash.each do |cred_or_group_name, cred_or_group_attrs|
          if cred_or_group_attrs.key?(:type) && cred_or_group_attrs[:type] == "Veil::Credential"
            expanded[cred_or_group_name.to_s] = Veil::Credential.create(cred_or_group_attrs)
          else
            cred_or_group_attrs.each do |name, opts|
              expanded[cred_or_group_name.to_s] ||= Hash.new
              expanded[cred_or_group_name.to_s][name.to_s] = Veil::Credential.create(opts)
            end
          end
        end

        expanded
      end

      def credentials_as_hash
        hash = Hash.new

        credentials.each do |cred_or_group_name, cred_or_group_attrs|
          if cred_or_group_attrs.is_a?(Hash)
            cred_or_group_attrs.each do |name, cred|
              hash[cred_or_group_name] ||= Hash.new
              hash[cred_or_group_name][name] = cred.to_hash
            end
          else
            hash[cred_or_group_name] = cred_or_group_attrs.to_hash
          end
        end

        hash
      end
    end
  end
end
