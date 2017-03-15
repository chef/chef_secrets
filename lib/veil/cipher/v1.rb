module Veil
  class Cipher
    class V1
      def initialize(_opts = {})
      end

      def encrypt(plaintext)
        raise RuntimeError, "Veil::Cipher::V1#encrypt should never be called"
      end

      def decrypt(anything)
        anything
      end
    end
  end
end
