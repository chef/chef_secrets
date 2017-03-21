require "spec_helper"
require "tempfile"
require 'stringio'

describe Veil::CredentialCollection::ChefSecretsFile do
  let(:hasher) { Veil::Hasher.create }
  let!(:file) { Tempfile.new("private_chef_secrets.json") }
  let(:user) { "opscode_user" }
  let(:group) { "opscode_group" }
  let(:version) { 1 }
  let(:content) do
    {
      "veil" => {
        "type" => "Veil::CredentialCollection::ChefSecretsFile",
        "version" => version,
        "hasher" => {},
        "credentials" => {}
      }
    }
  end
  let(:legacy_content) do
    {
      "redis_lb" => {
        "password" => "f1ad3e8b1e47bc81720742a2572b9ff"
      },
      "rabbitmq" => {
        "password" => "f5031e56ae018a7b71ce153086fc88f",
        "actions_password" => "92609a68d03b50afcc597d7"
      }
    }
  end

  describe "#self.from_file" do
    context "when the file exists" do
      context "when it's a valid credential store" do
        it "returns an instance" do
          file.write(JSON.pretty_generate(content))
          file.rewind
          expect(described_class.from_file(file.path)).to be_instance_of(described_class)
        end
      end

      context "when it's not a valid credential store" do
        it "raises an error" do
          file.write("not a json chef secrets file")
          file.rewind
          expect { described_class.from_file(file.path) }.to raise_error(Veil::InvalidCredentialCollectionFile)
        end
      end

      context "when it's a legacy secrets file" do
        it "imports the legacy file" do
          file.write(JSON.pretty_generate(legacy_content))
          file.rewind
          instance = described_class.from_file(file.path)
          expect(instance["redis_lb"]["password"].value).to eq("f1ad3e8b1e47bc81720742a2572b9ff")
          expect(instance["redis_lb"]["password"].version).to eq(0)
          expect(instance["redis_lb"]["password"].length).to eq(31)
        end
      end
    end

    context "when the file doesn't exist" do
      it "raises an error" do
        expect { described_class.from_file("not_a_file") }.to raise_error(Veil::InvalidCredentialCollectionFile)
      end
    end
  end

  describe "#save" do
    it "saves the content to a file it can read" do
      file.rewind
      creds = described_class.new(path: file.path)
      creds.add("redis_lb", "password")
      creds.add("postgresql", "sql_ro_password")
      creds.save

      file.rewind
      new_creds = described_class.from_file(file.path)

      expect(new_creds["redis_lb"]["password"].value).to eq(creds["redis_lb"]["password"].value)
      expect(new_creds["postgresql"]["sql_ro_password"].value).to eq(creds["postgresql"]["sql_ro_password"].value)
    end

    it "saves the content in an encrypted form" do
      creds = described_class.new(path: file.path)
      creds.add("postgresql", "sql_ro_password", value: "kneipenpathos")
      creds.save

      expect(IO.read(file.path)).to_not match(/kneipenpathos/)
    end

    context "when using ownership management" do
      let(:tmpfile) do
        s = StringIO.new
        allow(s).to receive(:path).and_return("/tmp/unguessable")
        s
      end

      context "when the target file does not exist" do

        let(:secrets_file) { double(File) }
        before(:each) do
          allow(File).to receive(:stat).with(file.path).and_raise(Errno::ENOENT)
        end

        context "when the user is not set" do
          it "does not change any permissions" do
            expect(Tempfile).to receive(:new).with("veil").and_return(tmpfile)
            expect(FileUtils).not_to receive(:chown)
            expect(FileUtils).to receive(:mv).with("/tmp/unguessable", file.path)

            creds = described_class.new(path: file.path)
            creds.add("redis_lb", "password")
            creds.save
          end
        end

        context "when the user is set" do
          it "gives the file proper permissions" do
            expect(Tempfile).to receive(:new).with("veil").and_return(tmpfile)
            expect(FileUtils).to receive(:chown).with(user, user, "/tmp/unguessable")
            expect(FileUtils).to receive(:mv).with("/tmp/unguessable", file.path)

            creds = described_class.new(path: file.path,
                                        user: user)
            creds.add("redis_lb", "password")
            creds.save
          end
        end
      end

      context "when the target file exists" do

        let(:secrets_file) { double(File) }
        let(:secrets_file_stat) { double(File, uid: 100, gid: 1000) }
        before(:each) do
          allow(File).to receive(:stat).with(file.path).and_return(secrets_file_stat)
        end

        context "when the user is not set" do
          it "keeps the existing file permissions" do
            expect(Tempfile).to receive(:new).with("veil").and_return(tmpfile)
            expect(FileUtils).to receive(:chown).with(100, 1000, "/tmp/unguessable")
            expect(FileUtils).to receive(:mv).with("/tmp/unguessable", file.path)

            creds = described_class.new(path: file.path)
            creds.add("redis_lb", "password")
            creds.save
          end
        end

        context "when the user is set" do
          it "gives the file proper permissions" do
            expect(Tempfile).to receive(:new).with("veil").and_return(tmpfile)
            expect(FileUtils).to receive(:chown).with(user, user, "/tmp/unguessable")
            expect(FileUtils).to receive(:mv).with("/tmp/unguessable", file.path)

            creds = described_class.new(path: file.path,
                                        user: user)
            creds.add("redis_lb", "password")
            creds.save
          end
        end

        context "when user and group are set" do
          it "gives the file proper permissions" do
            expect(Tempfile).to receive(:new).with("veil").and_return(tmpfile)
            expect(FileUtils).to receive(:chown).with(user, group, "/tmp/unguessable")
            expect(FileUtils).to receive(:mv).with("/tmp/unguessable", file.path)

            creds = described_class.new(path: file.path,
                                        user: user,
                                        group: group)
            creds.add("redis_lb", "password")
            creds.save
          end

          it "gives the file proper permission even when called from_file" do
            file.puts("{}"); file.rewind
            expect(Tempfile).to receive(:new).with("veil").and_return(tmpfile)
            expect(FileUtils).to receive(:chown).with(user, group, "/tmp/unguessable")
            expect(FileUtils).to receive(:mv).with("/tmp/unguessable", file.path)

            creds = described_class.from_file(file.path,
                                              user: user,
                                              group: group)
            creds.add("redis_lb", "password")
            creds.save
          end
        end
      end
    end

    it "saves the latest version number" do
      allow(FileUtils).to receive(:chown)
      file.rewind
      creds = described_class.new(path: file.path)
      creds.save

      file.rewind
      new_creds = described_class.from_file(file.path)
      expect(new_creds.version).to eq(2)
    end

    context "reading an unencrypted file" do
      let(:existing_content) { <<EOF
{
  "veil": {
    "type": "Veil::CredentialCollection::ChefSecretsFile",
    "version": 1,
    "hasher": {
      "type": "Veil::Hasher::PBKDF2",
      "secret": "5f6e76ae7f5b631a96f5142b2a4b837e23ab6d204dc9e635fc18783ae8d253596f9cd9b65e295c96cee0b9cd1171cae1ca2e897ca5419106785d99d4a42fe9897f2fc7f537a05d39f19b26aa05500e93d65621ed3cdbb718d3005f808fe04a2e2267c43f5100dfe1a6ab3b6462d7fe57bc3adced263fd8c2c55ddebc2f9067b475868e9fb950bf2ae37889e22c42fd686b168b25410a4fa7689a32686fab76e57cd64482ff411be69a4c9abefc5ed64227fb42db05f3b221ddc18c6bb7ca54625cd8a5426be0d9c2161f09f35a04bd9b07884f4319389801c123d0bd345d4218434d9aee87dbb115c5cc22d1c87ffc447c2a008e89edda7e25453076dd478df0f08d50206724cd055741b7521b889fbf6d12af7946ee069bdb60cf79b14e2dad910bab97ae0ff6a9c1f88556e493df26bff007728317683a14433b30a6b6537095c1a8906a08d8a067a1b2a0443ad6fcf007e6bd70c8a3bbfe571044652d3960fd200d23f046b2493463692b570f1c94dabb05933e8d6bcd7e59a708ac8b385034a32b5b1f20635cb10e8027376f496ee1e8496c3517e24bfab3a840f0994a7fde433dc942be781488e6ebbbc68872467ebd216c71e092b2adae600a50bc2ace312cc8cd7949ddb16b72555c9d511211e22383eb515dad22032e80c11f3193a1b357f1c073e4770e314a960bbeada64b36078cfc18e806c03743a0dcd1f77ef3",
      "salt": "78a8140351ecbfaee137655377aa15138655dc65a49c53ca5f6ee05c7b9c3db9dbc0e2bfdf14af3a062a1cc513e4e086126cf428890df3caf11d9714729027ac93fbf8b7bd9d8fc734b7b4a84c979b45e804e573f93f5cd6dab0b1cf7e5b6191c0924e61b84e8289065918fa991846e1a0f19f357c497c9fa4c420fda0578c14",
      "iterations": 10000,
      "hash_function": "OpenSSL::Digest::SHA512"
    },
    "credentials": {
      "postgresql": {
        "db_superuser_password": {
          "type": "Veil::Credential",
          "name": "db_superuser_password",
          "group": "postgresql",
          "value": "37f5c43dd8bab089821e5a49fe0ac17128c6d1878fe632e687889eaaea1980b51b3e3df755d2610cffe6896c1df9f23bdda3",
          "version": 1,
          "length": 100,
          "frozen": false
        }
      }
    }
  },
  "postgresql": {
    "db_superuser_password": "37f5c43dd8bab089821e5a49fe0ac17128c6d1878fe632e687889eaaea1980b51b3e3df755d2610cffe6896c1df9f23bdda3"
  }
}
EOF
}
      let(:tempfile) { Tempfile.new("private-chef-secrets").path }

      before(:each) do
        File.write(tempfile, existing_content)
      end

      it "reads the secrets" do
        creds = described_class.new(path: tempfile)
        expect(creds["postgresql"]["db_superuser_password"].value).to eq("37f5c43dd8bab089821e5a49fe0ac17128c6d1878fe632e687889eaaea1980b51b3e3df755d2610cffe6896c1df9f23bdda3")
      end

      it "saves the file encrypted" do
        creds = described_class.new(path: tempfile)
        creds.save

        expect(IO.read(tempfile)).to_not match("37f5c43dd8bab089821e5a49fe0ac17128c6d1878fe632e687889eaaea1980b51b3e3df755d2610cffe6896c1df9f23bdda3")
      end

      it "keeps only the veil key of the file's hash" do
        creds = described_class.new(path: tempfile)
        creds.save

        json = JSON.parse(IO.read(tempfile))
        expect(json.keys).to eq(["veil"])
      end
    end
  end
end
