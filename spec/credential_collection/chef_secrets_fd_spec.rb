require 'spec_helper'

describe Veil::CredentialCollection::ChefSecretsFd do
  describe "#new" do
    let(:content) { '{ "secret_service": { "secret_name": "secret_value" } }' }
    let(:content_file) {
      rd, wr = IO.pipe
      wr.puts content
      wr.close
      rd
    }

    context "env variable is set" do
      before(:each) do
        ENV['CHEF_SECRETS_FD'] = content_file.to_i.to_s
      end

      it 'reads the secret from the passed file descriptor' do
        expect(subject.get("secret_service", "secret_name")).to eq("secret_value")
      end

      # TODO(ssd) 2017-03-22: We wanted a test for closing the FD, but
      # didn't want to fight with ruby today.

      context "env var content cannot be parsed" do
        let(:content) { '{ "secre ' }

        it "re-raises the JSON parse error" do
          expect{ subject.get("secret_service", "secret_name") }.to raise_error(Veil::InvalidCredentialCollectionFd)
        end
      end
    end

    context "CHEF_SECRETS_FD is not set" do
      before(:each) do
        ENV.delete('CHEF_SECRETS_FD')
      end

      it 'raises an exception' do
        expect{ described_class.new }.to raise_error(Veil::InvalidCredentialCollectionFd)
      end
    end

    context "unsupported methods" do
      let(:content) { '{}' }

      before(:each) do
        ENV['CHEF_SECRETS_FD'] = content_file.to_i.to_s
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
