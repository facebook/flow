# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.
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

require 'kramdown/parser'
require 'kramdown/converter/syntax_highlighter/rouge'
require 'rouge'

module Kramdown
  module Parser
    # Custom Kramdown parser
    #
    # Features:
    #
    # - add `+line_numbers` flag to fenced code blocks, e.g.
    #
    #   ```js +line_numbers
    #   foo;
    #   ```
    #
    class FlowKramdown < Kramdown::Parser::Kramdown
      def initialize(source, options)
        super
        {
          :codeblock_fenced => :codeblock_fenced_flow
        }.each do |current, replacement|
          i = @block_parsers.index(current)
          @block_parsers.delete(current)
          @block_parsers.insert(i, replacement)
        end
      end

      FENCED_CODEBLOCK_MATCH = /^(([~`]){3,})\s*?((\S+?)(?:\?\S*)?)?(\s*\+line_numbers)?\s*?\n(.*?)^\1\2*\s*?\n/m
      def parse_codeblock_fenced_flow
        if @src.check(self.class::FENCED_CODEBLOCK_MATCH)
          start_line_number = @src.current_line_number
          @src.pos += @src.matched_size
          el = new_block_el(:codeblock, @src[6], nil, :location => start_line_number)
          lang = @src[3].to_s.strip
          unless lang.empty?
            el.options[:lang] = lang
            el.attr['class'] = "language-#{@src[4]}"
          end
          line_numbers = @src[5].to_s.strip
          unless line_numbers.empty?
            el.options[:line_numbers] = true
          end
          @tree.children << el
          true
        else
          false
        end
      end
      define_parser(:codeblock_fenced_flow, /^[~`]{3,}/)
    end
  end

  module Converter
    # Custom Kramdown converter
    #
    # Features:
    #
    # - customizes the header output to render anchor IDs as children instead
    #   of on the header element itself (necessary to position the anchor to
    #   account for our fixed header).
    #
    # - customizes codeblock output to pass the :line_numbers options
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

      def convert_codeblock(el, indent)
        attr = el.attr.dup
        lang = extract_code_language!(attr)

        # THIS LINE IS THE ONLY THING DIFFERENT FROM UPSTREAM
        hl_opts = { :line_numbers => el.options[:line_numbers] }

        highlighted_code = highlight_code(el.value, el.options[:lang] || lang, :block, hl_opts)

        if highlighted_code
          add_syntax_highlighter_to_class_attr(attr, lang || hl_opts[:default_lang])
          "#{' '*indent}<div#{html_attributes(attr)}>#{highlighted_code}#{' '*indent}</div>\n"
        else
          result = escape_html(el.value)
          result.chomp!
          if el.attr['class'].to_s =~ /\bshow-whitespaces\b/
            result.gsub!(/(?:(^[ \t]+)|([ \t]+$)|([ \t]+))/) do |m|
              suffix = ($1 ? '-l' : ($2 ? '-r' : ''))
              m.scan(/./).map do |c|
                case c
                when "\t" then "<span class=\"ws-tab#{suffix}\">\t</span>"
                when " " then "<span class=\"ws-space#{suffix}\">&#8901;</span>"
                end
              end.join('')
            end
          end
          code_attr = {}
          code_attr['class'] = "language-#{lang}" if lang
          "#{' '*indent}<pre#{html_attributes(attr)}><code#{html_attributes(code_attr)}>#{result}\n</code></pre>\n"
        end
      end
    end


    module SyntaxHighlighter
      module Rouge
        def self.call(converter, text, lang, type, call_opts)
          opts = options(converter, type).dup

          # THIS LINE IS THE ONLY THING DIFFERENT FROM UPSTREAM
          opts[:line_numbers] = true if call_opts[:line_numbers]

          call_opts[:default_lang] = opts[:default_lang]
          lexer = ::Rouge::Lexer.find_fancy(lang || opts[:default_lang], text)
          return nil if opts[:disable] || !lexer

          formatter = (opts.delete(:formatter) || ::Rouge::Formatters::HTML).new(opts)
          formatter.format(lexer.lex(text))
        end
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
