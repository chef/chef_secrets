module Veil
  class InvalidSalt < StandardError; end
  class InvalidSecret < StandardError; end
  class InvalidParameter < StandardError; end
  class InvalidHasher < StandardError; end
  class InvalidCredentialCollection < StandardError; end
  class InvalidCredentialCollectionFile < InvalidCredentialCollection; end
  class InvalidCredentialCollectionEnv <  InvalidCredentialCollection; end
  class InvalidCredentialCollectionFd <  InvalidCredentialCollection; end
  class MissingParameter < StandardError; end
  class NotImplmented < StandardError; end
  class InvalidCredentialHash < StandardError; end
  class CredentialNotFound < StandardError; end
  class GroupNotFound < StandardError; end
  class FileNotFound < StandardError; end
  class FileNotReadable < StandardError; end
  class UnknownProvider < StandardError; end
end
