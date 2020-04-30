require "spec_helper"
require "openssl"

describe Veil::Cipher::V2 do
  let(:iv64) { Base64.strict_encode64("mondaytue16bytes") }
  let(:key64) { Base64.strict_encode64("thursdayfridaysaturdaysun32bytes") }
  let(:ciphertext64) { "qS6SRmgOgSta2jSdD60sJmu83cRgy4DUJ2nKZwStrqs=" }
  let(:plainhash) { { "chef-server": "test-value" } }

  describe "#new" do
    it "accepts passed key and iv base64-encoded data" do
      cipher = described_class.new(iv: iv64, key: key64)

      expect(cipher.iv).to eq("mondaytue16bytes")
      expect(cipher.key).to eq("thursdayfridaysaturdaysun32bytes")
    end

    it "generates key and iv data if none was passed" do
      openssl = double(OpenSSL::Cipher)
      expect(OpenSSL::Cipher).to receive(:new).and_return(openssl)
      expect(openssl).to receive(:random_iv).and_return("random iv")
      expect(openssl).to receive(:random_key).and_return("random key")
      cipher = described_class.new()

      expect(cipher.iv).to eq("random iv")
      expect(cipher.key).to eq("random key")
    end
  end

  describe "#decrypt" do
    it "parses decrypted json data" do
      expect(described_class.new(iv: iv64, key: key64).decrypt(ciphertext64)).to eq(plainhash)
    end
  end

  describe "#to_hash" do
    it "base64-encodes iv and key" do
      expected = {
        iv: "bW9uZGF5dHVlMTZieXRlcw==",
        key: "dGh1cnNkYXlmcmlkYXlzYXR1cmRheXN1bjMyYnl0ZXM=",
        type: "Veil::Cipher::V2"
      }
      expect(described_class.new(iv: iv64, key: key64).to_hash).to eq(expected)
    end
  end
end
