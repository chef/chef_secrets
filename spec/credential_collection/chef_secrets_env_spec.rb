require 'spec_helper'

describe Veil::CredentialCollection::ChefSecretsEnv do
  describe "#new" do
    context "env variable is set" do
      let(:var_name) { "CHEF_SECRETS_DATA" }

      before(:each) do
        ENV[var_name] = '{ "secret_service": { "secret_name": "secret_value" } }'
      end

      it 'reads the secret from the env var CHEF_SECRETS_DATA' do
        expect(subject.get("secret_service", "secret_name")).to eq("secret_value")
      end

      it 'removes the variable from env' do
        subject
        expect(ENV[var_name]).to eq(nil)
      end

      context "env variable name is passed" do
        let(:var_name) { "CHEF_SECRETS_DATA_2" }
        let(:subject) { described_class.new(var_name: var_name) }

        it 'reads the secret from the passed env var name' do
          expect(subject.get("secret_service", "secret_name")).to eq("secret_value")
        end

        it 'removes the variable from env' do
          subject
          expect(ENV[var_name]).to eq(nil)
        end
      end

      context "env var content cannot be parsed" do
        before(:each) do
          ENV[var_name] = '{ "secre '
        end

        it "re-raises the JSON parse error" do
          expect{ subject.get("secret_service", "secret_name") }.to raise_error(Veil::InvalidCredentialCollectionEnv)
        end
      end
    end

    context "env variable is not set" do
      let(:var_name) { "CHEF_SECRETS_DATA" }

      before(:each) do
        ENV.delete(var_name)
      end

      it 'raises an exception' do
        expect{ described_class.new }.to raise_error(Veil::InvalidCredentialCollectionEnv)
      end
    end

    context "unsupported methods" do
      let(:var_name) { "CHEF_SECRETS_DATA" }

      before(:each) do
        ENV[var_name] = '{}'
      end

      it 'does not support #rotate' do
        expect{ subject.rotate }.to raise_error(NotImplementedError)
      end

      it 'does not support #save' do
        expect{ subject.save }.to raise_error(NotImplementedError)
      end

      it 'does not support #rotate_hasher' do
        expect{ subject.rotate_hasher }.to raise_error(NotImplementedError)
      end
    end
  end
end
