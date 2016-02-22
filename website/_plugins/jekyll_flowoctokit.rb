require 'octokit'

module Jekyll
  class FlowGitHubGenerator < Jekyll::Generator
    def self.latest_release(repo)
      @releases ||= Hash.new do |hash, key|
        access_token = ENV['GH_BOT_TOKEN']
        authed = access_token.nil? ? "" : " (with auth)"
        Jekyll.logger.info "GitHub Query:", "latest release for #{key}#{authed}"
        client = Octokit::Client.new(:access_token => access_token)
        hash[key] = client.latest_release(key)
      end
      @releases[repo]
    end

    def generate(site)
      repo = site.config['repository']
      release = self.class.latest_release(repo)
      tag = release.tag_name

      # we generate these URLs manually instead of using
      # release.assets[...].browser_download_url because they may not be
      # uploaded yet when generating the site; travis uploads the mac and linux
      # binaries from separate workers, and the linux worker is the one that
      # generates the site.
      linux_binary = "https://github.com/#{repo}/releases/download/#{tag}/flow-linux64-#{tag}.zip"
      mac_binary = "https://github.com/#{repo}/releases/download/#{tag}/flow-osx-#{tag}.zip"

      site.config['latest_release'] = {
        'name' => release.name,
        'tag_name' => release.tag_name,
        'linux_binary' => {
          'url' => linux_binary
        },
        'mac_binary' => {
          'url' => mac_binary
        }
      }
    end
  end
end
