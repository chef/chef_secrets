require "spec_helper"

describe Veil::Cipher::V1 do
  describe "#decrypt" do
    it "does nothing" do
      expect(described_class.new().decrypt("hi")).to eq("hi")
    end
  end

  describe "#encrypt" do
    it "raises a RuntimeError" do
      expect { described_class.new().encrypt("hi") }.to raise_error(RuntimeError)
    end
  end
end
