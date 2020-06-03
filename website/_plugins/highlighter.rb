# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

require 'open3'
require 'json'
require 'cgi'

module Jekyll
  module Converters
    class Markdown::FlowMarkdown
      class << self
        attr_accessor :tempdir
      end

      def initialize(config)
        @config = config
        @flow_cache = {}
        @markdown = Jekyll::Converters::Markdown::KramdownParser.new config
      end

      def convert(content)
        @markdown.convert(render(content))
      end

      def render(content)
        content.gsub(Kramdown::Parser::GFM::FENCED_CODEBLOCK_MATCH) {|match|
          _, _, lang, _, code = Regexp.last_match.captures

          if lang == 'js'
            render_block(code)
          else
            match
          end
        }
      end

      def render_block(content)
        unless @flow_cache[content]
          tokens = serialize_tokens(get_tokens(content))
          errors = serialize_errors(get_errors(content))
          html_ranges = create_html_ranges(tokens, errors)

          printed_code = print_code(content, html_ranges)
          hover_errors = print_hover_errors(errors)

          json = JSON.generate({
            'value' => content,
            'tokens' => tokens,
            'errors' => errors,
          })

          json = CGI.escapeHTML(json)

          @flow_cache[content] = print_code_section(printed_code, hover_errors, json, { 'line_numbers' => true })
        end

        @flow_cache[content]
      end

      def match_pragma(content)
        content.match(/^(\/\/ *@flow|\/\*\s*@flow)/)
      end

      def run_flow(command, stdin, success_codes)
        out, err, status = Open3.capture3(
          "#{@config['flow']['path'] || 'flow'} #{command}",
          :stdin_data => stdin
        )
        if status.exited? and success_codes.include?(status.exitstatus)
          response = JSON.parse(out)
        else
          raise "flow ast failed (exit #{status.exitstatus}): #{err}"
        end
      end

      def get_tokens(content)
        response = run_flow("ast --tokens", content, [0])
        response['comments'].each do |comment|
          normalize_comment comment
        end
        tokens = response['tokens']
        tokens += response['comments']
        tokens.sort_by do |token|
          token['range'][0]
        end
      end

      def normalize_comment(comment)
        comment['context'] = 'comment'
        if comment['type'] == 'Line'
          comment['value'] = '//' + comment['value']
        elsif comment['type'] == 'Block'
          comment['value'] = '/*' + comment['value'] + "*/"
        end
      end

      def get_errors(content)
        return [] unless match_pragma(content)
        response = run_flow(
          "check-contents --json --root #{self.class.tempdir}",
          content,
          [0, 2]
        )
        response['errors']
      end

      def serialize_tokens(tokens)
        result = []
        tokens.each do |token|
          next if token['type'] == 'T_EOF'
          result.push({
            'type' => token['type'],
            'context' => token['context'],
            'value' => token['value'],
            'line' => token['loc']['start']['line'],
            'start' => token['range'][0],
            'end' => token['range'][1],
          })
        end
        result
      end

      def serialize_errors(errors)
        errors.each_with_index.map do |error, error_index|
          error_id = error_index + 1

          messages = error['message'].each_with_index.map do |message, message_index|
            message_id = message_index + 1
            message_loc = message['loc'] || {}
            {
              'id' => "E#{error_id}M#{message_id}",
              'description' => message['descr'],
              'context' => message['context'],
              'source' => message_loc['source'],
              'start' => message_loc['start'],
              'end' => message_loc['end'],
            }
          end

          operation = nil

          if !error['operation'].nil?
            op = error['operation']
            operation = {
              'description' => "#{op['descr']}\n",
              'context' => op['context'],
              'source' => op['loc']['source'],
              'start' => op['loc']['start'],
              'end' => op['loc']['end'],
            }
          end

          {
            'id' => "E#{error_id}",
            'messages' => messages,
            'operation' => operation,
          }
        end
      end

      def create_html_ranges(tokens, errors)
        before = {}
        after = {}

        tokens.each do |token|
          start_pos = token['start']
          end_pos   = token['end']

          open  = "<span class=\"#{token['context']} #{token['type']}\">"
          close = '</span>'

          before[start_pos] = (before[start_pos] || '') + open
          after[end_pos]    = close + (after[end_pos] || '')
        end

        errors.each do |error|
          error['messages'].each do |message|
            next if message['start'] == nil || message['end'] == nil

            start_pos = message['start']['offset']
            end_pos = message['end']['offset']

            open  = '<span class="flow-error-target" data-error-id="' + error['id'] + '" data-message-id="' + message['id'] + '">'
            close = '</span>'

            before[start_pos] = (before[start_pos] || '') + open
            after[end_pos]    = close + (after[end_pos] || '')
          end
        end

        { 'before' => before, 'after' => after }
      end

      def print_code(source, html_ranges)
        chars = source.chars
        printed = ""

        before = html_ranges['before']
        after = html_ranges['after']

        chars.each_with_index do |char, index|
          if before[index] != nil
            printed += before[index]
          end

          printed += CGI.escapeHTML(char)

          if after[index + 1] != nil
            printed += after[index + 1]
          end
        end

        printed
      end

      def print_code_section(content, hover_errors, json, options)
          <<-CODE
<div class="editor highlight">
  <div class="editor-content">
    #{print_editor_code(content, options['line_numbers'])}
  </div>
  <div class="editor-messages">#{hover_errors}</div>
  <div hidden class="editor-data">#{json}</div>
</div>
    CODE
      end

      def print_editor_code(content, line_numbers)
        if line_numbers
          lines = content.lines.count
          <<-CODE
<table>
  <tbody>
    <tr>
      <td class="editor-gutter">
        <pre class="editor-line">#{(1..lines).to_a.join("\n")}</pre>
      </td>
      <td class="editor-code"><pre><code>#{content}</code></pre></td>
    </tr>
  </tbody>
</table>
    CODE
        else
          <<-CODE
<pre><code>#{content}</code></pre>
    CODE
        end
      end

      def print_hover_errors(errors)
        errors = errors.each.map do |error|
          messages = error['messages'].each.map do |message|
            if message['context'] != nil
              '<span data-message-id="' + message['id'] + '">' + message['description'] + '</span>'
            else
              message['description']
            end
          end

          '<span class="flow-error" data-error-id="' + error['id'] + '">' + messages.join(' ') + '</span>'
        end

        errors.join("\n")
      end
    end
  end

  Hooks.register :site, :pre_render do |site|
    tempdir = Dir.mktmpdir
    Converters::Markdown::FlowMarkdown.tempdir = tempdir
    File.open(File.join(tempdir, '.flowconfig'), 'w') {|f|
      f.write(
        <<-EOF.gsub(/^ {10}/, '')
          [options]
          max_header_tokens=1
        EOF
      )
    }
  end

  Hooks.register :site, :post_render do |site|
    tempdir = Converters::Markdown::FlowMarkdown.tempdir
    flow = site.config['flow']['path'] || 'flow'
    `#{flow} stop #{tempdir}`
    FileUtils.remove_entry_secure tempdir
  end
end
