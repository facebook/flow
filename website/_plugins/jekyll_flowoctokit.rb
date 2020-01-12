# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

require 'octokit'
require 'ostruct'

module Jekyll
  class FlowGitHubGenerator < Jekyll::Generator
    def self.releases(repo)
      @releases ||= Hash.new do |hash, key|
        access_token = ENV['DOC_BOT_TOKEN']
        if access_token.nil?
          Jekyll.logger.info "GitHub Query:", "using fake releases for #{key} (no DOC_BOT_TOKEN)"
          hash[key] = [OpenStruct.new(
            :name => 'v0.0.0',
            :tag_name => 'v0.0.0',
            :published_at => DateTime.now,
            :html_url => "https://github.com/#{repo}/releases",
            :draft => false,
          )]
        else
          Jekyll.logger.info "GitHub Query:", "releases for #{key}"
          client = Octokit::Client.new(:access_token => access_token)
          hash[key] = client.releases(key)
        end
      end
      @releases[repo]
    end

    def map_release(repo, release)
      tag = release.tag_name

      # we generate these URLs manually instead of using
      # release.assets[...].browser_download_url because they may not be
      # uploaded yet when generating the site; Circle uploads the mac and linux
      # binaries from separate workers.
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
