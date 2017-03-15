require "base64"
require "openssl"

module Veil
  class Cipher
    class V2
      attr_reader :key, :iv, :cipher

      def initialize(opts = {})
        @cipher = OpenSSL::Cipher.new("aes-256-cbc")
        @key    = opts[:key] ? Base64.strict_decode64(opts[:key]) : cipher.random_key
        @iv     = opts[:iv] ? Base64.strict_decode64(opts[:iv]) : cipher.random_iv
      end

      def encrypt(plaintext)
        c = cipher
        c.encrypt
        c.key = key
        c.iv = iv
        Base64.strict_encode64(c.update(plaintext) + c.final)
      end

      def decrypt(ciphertext)
        c = cipher
        c.decrypt
        c.key = key
        c.iv = iv
        JSON.parse(c.update(Base64.strict_decode64(ciphertext)) + c.final, symbolize_names: true)
      end

      def to_hash
        {
          type: self.class.name,
          key: Base64.strict_encode64(key),
          iv: Base64.strict_encode64(iv)
        }
      end
      alias_method :to_h, :to_hash
    end
  end
end
