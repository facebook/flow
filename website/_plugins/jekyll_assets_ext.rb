# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

require 'jekyll-assets'

module Jekyll
  module Assets
    module Liquid
      # Custom tag: adds an `asset_name` liquid tag that returns the AMD module
      # name, and also sets up the dependency info so that Sprockets builds that
      # file.
      class AssetNameTag < Tag
        def render(context)
          site = context.registers[:site]
          args = @args.parse_liquid(context)
          sprockets = site.sprockets

          asset = find_asset(args, sprockets)
          sprockets.manifest.add(asset)
          File.basename(asset.logical_path, File.extname(asset.logical_path))

        rescue => error
          Jekyll.logger.error("", error.to_s)
          raise error
        end

        private
        def find_asset(args, sprockets)
          sprockets_, file = args[:sprockets] ||= {}, args[:file]
          if !(out = sprockets.find_asset(file, sprockets_))
            raise(
              AssetNotFoundError, args[:file]
            )

          else
            out.liquid_tags << self
            !args.proxies?? out : ProxiedAsset.new(
              out, args, sprockets, self
            )
          end
        end
      end
    end
  end
end

Liquid::Template.register_tag "asset_name", Jekyll::Assets::Liquid::AssetNameTag


module StripExtensionFilter
  def strip_extension(input)
    File.join(File.dirname(input), File.basename(input, File.extname(input)))
  end
end

Liquid::Template.register_filter(StripExtensionFilter)
