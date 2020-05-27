# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

require 'jekyll-assets'

module Jekyll
  module Assets
    class Tag
      def on_name(args, ctx:, asset:)
        return unless args[:name]
        env = ctx.registers[:site].sprockets
        File.basename(asset.logical_path, File.extname(asset.logical_path))
      end
    end
  end
end

module StripExtensionFilter
  def strip_extension(input)
    File.join(File.dirname(input), File.basename(input, File.extname(input)))
  end
end

Liquid::Template.register_filter(StripExtensionFilter)
