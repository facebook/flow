require 'octokit'

module Jekyll
  class FlowGitHubGenerator < Jekyll::Generator
    def generate(site)
      repo = site.config['repository']
      release = Octokit.latest_release(repo)
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
