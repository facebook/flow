# Copyright (c) 2013-present, Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the "flow" directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.
#
# This library contains portions based on third party software provided under
# this license:
#
#   kramdown - fast, pure-Ruby Markdown-superset converter
#   Copyright (C) 2009-2013 Thomas Leitner <t_leitner@gmx.at>
#
#   Permission is hereby granted, free of charge, to any person obtaining a
#   copy of this software and associated documentation files (the
#   "Software"), to deal in the Software without restriction, including
#   without limitation the rights to use, copy, modify, merge, publish,
#   distribute, sublicense, and/or sell copies of the Software, and to
#   permit persons to whom the Software is furnished to do so, subject to
#   the following conditions:
#
#   The above copyright notice and this permission notice shall be included
#   in all copies or substantial portions of the Software.
#
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
#   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
#   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
#   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
#   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
#   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#

module Kramdown
  module Converter
    # Custom Kramdown converter
    #
    # Features:
    #
    # - customizes the header output to render anchor IDs as children instead
    #   of on the header element itself (necessary to position the anchor to
    #   account for our fixed header).
    #
    class FlowHtml < Html
      def convert_header(el, indent)
        attr = el.attr.dup
        if @options[:auto_ids] && !attr['id']
          attr['id'] = generate_id(el.options[:raw_text])
        end
        id = attr.delete('id')
        @toc << [el.options[:level], id, el.children] if id && in_toc?(el)
        level = output_header_level(el.options[:level])
        child_content = inner(el, indent)
        child_content << %Q{<a id="#{id}" class="hashref"></a>}
        child_content << %Q{<a href="##{id}" class="hash">#</a>}
        format_as_block_html("h#{level}", attr, child_content, indent)
      end
    end
  end
end

module Jekyll
  module Converters
    class Markdown::FlowMarkdownParser < Markdown::KramdownParser
      def convert(content)
        Kramdown::Document.new(content, @config).to_flow_html
      end
    end
  end

  module Tags
    # Fixes the {% highlight %} tag to generate markup that matches the output
    # of fenced code blocks (```).
    class HighlightBlock
      def add_code_tag(code)
        code_attributes = [
          "class=\"language-#{@lang.to_s.tr("+", "-")} highlighter-rouge\""
        ].join(" ")
        if @highlight_options[:linenos]
          "<div #{code_attributes}><div class=\"highlight\">"\
          "#{code.chomp}</div></div>"
        else
          "<div #{code_attributes}><pre class=\"highlight\"><code>"\
          "#{code.chomp}</code></pre></div>"
        end
      end
    end
  end
end
