# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

require 'fileutils'
require 'json'
require 'open3'
require 'set'
require 'tmpdir'

module Jekyll
  EXT = ".doc.js"
  EXT_REGEX = /\.doc\.js$/i
  YAML_REGEX = /^(---\s*\n.*?\n?)^((---|\.\.\.)\s*$\n?)/m

  class FlowdocGenerator < Generator
    priority :low

    def generate(site)
      site.static_files.delete_if do |sf|
        next if not sf.path =~ EXT_REGEX

        dirname = File.dirname(sf.path.gsub(site.source, ""))
        basename = File.basename(sf.path)

        page = FlowdocPage.new(site, site.source, dirname, basename)
        site.pages << page

        sprockets = site.sprockets
        asset = sprockets.find_asset('inlineErrors', {})
        raise AssetNotFoundError, 'inlineErrors' unless asset
        sprockets.manifest.add(asset)
        if page["path"] && sprockets.digest?
          site.regenerator.add_dependency(
            site.in_source_dir(page["path"]),
            site.in_source_dir(asset.logical_path)
          )
        end

        true
      end

      tags = site.data['flow_dot_js_versions']
      site.config['flow'] ||= {}
      if tags.nil? || tags.empty?
        version = ENV["CIRCLE_TAG"] || "master"
        site.config['flow']['version'] = version
        site.config['flow']['versions'] = [version]
      else
        versions = tags.map {|v| v['version'] }
        site.config['flow']['version'] = versions.first
        site.config['flow']['versions'] = ["master"] + versions
      end
    end
  end
end
