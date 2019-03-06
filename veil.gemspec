# coding: utf-8
lib = File.expand_path("../lib", __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require "veil/version"

Gem::Specification.new do |spec|
  spec.name          = "veil"
  spec.version       = Veil::VERSION
  spec.authors       = ["Chef Software, Inc."]
  spec.email         = ["partnereng@chef.io"]

  spec.summary       = %q{Veil is a Ruby Gem for generating secure secrets from a shared secret}
  spec.description   = spec.summary
  spec.license       = "Apache-2.0"
  spec.homepage      = "https://github.com/chef/chef_secrets/"

  spec.files         = Dir.glob("{bin,lib,spec}/**/*").reject { |f| File.directory?(f) } + ["LICENSE"]
  spec.executables   = spec.files.grep(/^bin/) { |f| File.basename(f) }
  spec.test_files    = spec.files.grep(/^(spec|features)/)
  spec.require_paths = ["lib"]

  spec.add_dependency "bcrypt", "~> 3.1"
  spec.add_dependency "pbkdf2"

  spec.add_development_dependency "bundler"
  spec.add_development_dependency "rake"
  spec.add_development_dependency "rspec", "~> 3.0"
end
