require "veil/cipher/v1"
require "veil/cipher/v2"

module Veil
  class Cipher
    DEFAULT_DECRYPTOR = Veil::Cipher::V1
    DEFAULT_ENCRYPTOR = Veil::Cipher::V2

    class << self
      #
      # Create a new Cipher instance
      #
      # Defaults to using v1 for decryption (noop), v2 for encryption.
      # If invoked as default, v2 will generate key and iv.
      #
      # @param opts Hash<Symbol> a hash of options to pass to the constructor
      #
      # @example Veil::Cipher.create(type: "V1")
      # @example Veil::Cipher.create(type: "V2", key: "blah", iv: "vi")
      #
      def create(opts = {})
        case opts
        when {}, nil
          [ DEFAULT_DECRYPTOR.new({}), DEFAULT_ENCRYPTOR.new({}) ]
        else
          cipher = const_get(opts[:type])
          [ cipher.new(opts), cipher.new(opts) ]
        end
      end
    end
  end
end
