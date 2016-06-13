require 'octokit'

module Jekyll
  class FlowGitHubGenerator < Jekyll::Generator
    def self.releases(repo)
      @releases ||= Hash.new do |hash, key|
        access_token = ENV['DOC_BOT_TOKEN']
        authed = access_token.nil? ? "" : " (with auth)"
        Jekyll.logger.info "GitHub Query:", "releases for #{key}#{authed}"
        client = Octokit::Client.new(:access_token => access_token)
        hash[key] = client.releases(key)
      end
      @releases[repo]
    end

    def map_release(repo, release)
      tag = release.tag_name

      # we generate these URLs manually instead of using
      # release.assets[...].browser_download_url because they may not be
      # uploaded yet when generating the site; travis uploads the mac and linux
      # binaries from separate workers, and the linux worker is the one that
      # generates the site.
      linux_binary = "https://github.com/#{repo}/releases/download/#{tag}/flow-linux64-#{tag}.zip"
      mac_binary = "https://github.com/#{repo}/releases/download/#{tag}/flow-osx-#{tag}.zip"

      {
        'name' => release.name.nil? || release.name.empty? ?
          release.tag_name :
          release.name,
        'tag_name' => release.tag_name,
        'linux_binary' => {
          'url' => linux_binary
        },
        'mac_binary' => {
          'url' => mac_binary
        },
        'published_at' => release.published_at,
        'html_url' => release.html_url
      }
    end

    def generate(site)
      repo = site.config['repository']
      site.config['releases'] = self.class.releases(repo).reject {|release|
        release.draft
      }.map {|release|
        map_release(repo, release)
      }
      site.config['latest_release'] = site.config['releases'][0]
    end
  end
end
