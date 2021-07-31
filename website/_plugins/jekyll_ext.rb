# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
#

class VersionTag < Liquid::Tag
  def initialize(tag_name, version, tokens)
     super
     @tag_name = tag_name
     @version = version
  end

  def render(context)
    if @tag_name == "since" then
      cls = "added"
      title = "Added in #{@version}"
      symbol = "&ge;"
    else
      cls = "removed"
      title = "Removed after #{@version}"
      symbol = "&le;"
    end
    "<span class=\"version #{cls}\" title=\"#{title}\">"\
      "#{symbol}#{@version}"\
    "</span>"
  end
end

Liquid::Template.register_tag('since', VersionTag)
Liquid::Template.register_tag('until', VersionTag)

module StripTagsFilter
  def strip_liquid_tags(input)
    empty = ''.freeze
    input.to_s.gsub(/{%.*?%}/m, empty)
  end
end

Liquid::Template.register_filter(StripTagsFilter)
