require "spec_helper"

describe Veil::CredentialCollection do
  describe 'from_config' do
    context 'passing provider "chef-secrets-file"' do
      let(:opts) { { provider: 'chef-secrets-file', something_else: 'config' } }

      it 'instantiates ChefSecretsFile with all options' do
        expect(Veil::CredentialCollection::ChefSecretsFile).to receive(:new).with(opts)
        described_class.from_config(opts)
      end
    end

    context 'passing anything else as provider' do
      let(:opts) { { provider: 'vault' } }

      it 'raises an exception' do
        expect { described_class.from_config(opts) }.to raise_error(Veil::UnknownProvider)
      end
    end

    context 'passing an options hash that has no provider' do
      let(:opts) { { something: 'else' } }

      it 'raises an exception' do
        expect { described_class.from_config(opts) }.to raise_error(Veil::UnknownProvider)
      end
    end
  end
end
