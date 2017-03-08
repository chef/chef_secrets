require "spec_helper"

describe Veil::Cipher do
  describe "#self.create" do
    it "defaults to noop decrypt, v2 encrypt" do
      dec, enc = described_class.create()
      expect(dec.class).to eq(Veil::Cipher::V1)
      expect(enc.class).to eq(Veil::Cipher::V2)
    end

    it "parses the non-namespaced type" do
      dec, enc = described_class.create(type: "V2")
      expect(dec.class).to eq(Veil::Cipher::V2)
      expect(enc.class).to eq(Veil::Cipher::V2)
    end

    it "parses the complete type" do
      dec, enc = described_class.create(type: "Veil::Cipher::V2")
      expect(dec.class).to eq(Veil::Cipher::V2)
      expect(enc.class).to eq(Veil::Cipher::V2)
    end
  end
end
