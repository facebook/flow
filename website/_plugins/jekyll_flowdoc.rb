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
        version = ENV["TRAVIS_TAG"] || "master"
        site.config['flow']['version'] = version
        site.config['flow']['versions'] = [version]
      else
        versions = tags.map {|v| v['version'] }
        site.config['flow']['version'] = versions.first
        site.config['flow']['versions'] = ["master"] + versions
      end
    end
  end

  class FlowdocPage < Page
    def process(name)
      self.ext = EXT
      self.basename = name[0..-ext.length - 1]
    end

    def read_yaml(base, name, opts = {})
      filename = File.join(base, name)

      begin
        self.content = File.read(site.in_source_dir(base, name),
                                 Utils.merged_file_read_opts(site, opts))
        if content =~ YAML_REGEX
          self.data = SafeYAML.load(Regexp.last_match(1))
        end
      rescue Exception => e
        Jekyll.logger.warn "Error reading file #{filename}: #{e.message}"
      end

      self.data ||= {}

      validate_data! filename
      validate_permalink! filename

      self.data
    end
  end

  class FlowdocConverter < Converter
    class << self
      attr_accessor :tempdir

      def valid_pragma?(str)
        @pragmas ||= Set.new ['$WithLineNums', '$NoCliOutput']
        @pragmas.include? str
      end
    end

    def initialize(config = {})
      super(config)
      @markdown = Converters::Markdown.new @config
    end

    def matches(ext)
      ext =~ EXT_REGEX
    end

    def output_ext(ext)
      '.html'
    end

    def convert_token(token)
      type, range, context = token.values_at('type', 'range', 'context')
      line = token['loc']['start']['line']
      if type == 'T_EOF'
        {
          :value => "",
          :range => range,
          :line => line
        }
      else
        {
          :start => "<span class=\"#{context} #{type}\">",
          :end => "</span>",
          :range => range,
          :line => line
        }
      end
    end

    def is_block_comment(comment)
      comment['type'] == 'Block' && comment['loc']['start']['column'] == 0
    end

    def is_ignored_comment(comment)
      loc, value = comment.values_at('loc', 'value')
      start = loc['start']
      is_block_comment(comment) &&
        ((start['line'] == 1 && value.strip == "@flow") || value =~ YAML_REGEX)
    end

    def is_pragma(comment)
      comment['loc']['start']['column'] == 0 &&
        self.class.valid_pragma?(comment['value'].strip)
    end

    def unindent(str)
      str = str.gsub(/\A\n|\n\z/m, '')
      indent = str
        .gsub(/\A\n+|\n+\z/m, '')
        .gsub(/\n+/m, "\n")
        .scan(/^[ \t]*/)
        .min_by { |l| l.length }
      str.gsub!(/^#{indent}/, "")
    end

    def convert_comment(comment)
      type, loc, value, range = comment.values_at('type', 'loc', 'value', 'range')
      line = loc['start']['line']
      if is_ignored_comment(comment)
        {
          :value => "",
          :range => range,
          :line => line
        }
      elsif is_pragma(comment)
        {
          :pragma => value.strip,
          :range => range,
          :line => line
        }
      elsif is_block_comment(comment)
        {
          :value => unindent(value),
          :range => range,
          :line => line
        }
      else
        {
          :start => '<span class="comment %s">' % [type.downcase],
          :end => '</span>',
          :range => range,
          :line => line
        }
      end
    end

    def flow_exe
      @config['flow']['path'] || 'flow'
    end

    def get_tokens(content)
      token_json = Open3.popen3("#{flow_exe} ast --tokens") {|stdin, stdout|
        stdin.puts(content)
        stdin.close
        JSON.parse(stdout.gets)
      }
      tokens, errors, comments = token_json.values_at('tokens', 'errors', 'comments')
      tokens = tokens.map {|token| convert_token token }
      items = comments.reduce(tokens) do |comments, comment|
        comments.push(convert_comment(comment))
        comments
      end
    end

    def get_error_json(content)
      cmd = "#{flow_exe} check-contents --json --root #{self.class.tempdir}"
      response = Open3.popen3(cmd) {|stdin, stdout|
        stdin.puts(content)
        stdin.close
        JSON.parse(stdout.gets)
      }

      errors_json = response['errors']

      errors = []
      errors_json.each_with_index do |error, e_index|
        e_id = e_index + 1
        messages = []
        error['message'].each_with_index do |message, m_index|
          m_id = e_id * 1000 + (m_index + 1)
          msg_json = {
            'id' => "M#{m_id}",
            'description' => message['descr'],
            'context' => message['context'],
          }

          m_loc = message['loc']
          if m_loc != nil
            msg_json['source'] = m_loc['source']
            msg_json['start'] = m_loc['start']
            msg_json['end'] = m_loc['end']
          end

          messages.push(msg_json)
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
        errors.push({
          'id' => "E#{e_id}",
          'messages' => messages,
          'operation' => operation
        })
      end
      errors
    end

    def get_error_output(error, line_offset)
      chunks = []
      start = error['messages'][0]['start']['offset']
      operations = error['operation'].nil? ? [] : [error['operation']]
      (operations + error['messages']).each do |message|
        if message.has_key?('source') &&
           message.has_key?('start') &&
           message.has_key?('end') &&
           !message['context'].nil? &&
           !message['context'].empty?

          context = message['context']
          start_col = message['start']['column'] - 1
          end_col = message['start']['line'] == message['end']['line'] ?
            message['end']['column'] - 1 :
            start_col

          filename = message['source'] && message['source'] != '-' ?
            message['source'] :
            ''

          line = message['start']['line'] - (line_offset - 1)

          prefix = "#{filename}#{line}: "
          before = start_col >= 1 ? context.slice(0..(start_col - 1)) : ''
          highlight = context.slice(start_col..end_col)
          after = context.slice((end_col + 1)..(context.length - 1))

          line = "#{prefix}#{escape(before)}" +
            "<strong>#{escape(highlight)}</strong>" +
            "#{escape(after)}\n"
          chunks.push(line)

          offset = start_col + prefix.length - 1
          whitespace = "#{prefix}#{before}".gsub(/\S/, ' ')
          arrows = '^' * highlight.length

          chunks.push("#{whitespace}#{arrows} #{escape(message['description'])}")
        else
          chunks.push(". #{escape(message['description'])}\n")
        end
      end
      chunks.join('')
    end

    def get_errors(errors_json)
      errors = []
      outputs = []
      errors_json.each do |error|
        e_id = error['id']
        first_offset = nil
        if !error['operation'].nil?
          first_offset = error['operation']['start']['offset']
        end
        error['messages'].each do |message|
          next unless message.has_key?('start') && message.has_key?('end')

          start_offset = message['start']['offset']
          end_offset = message['end']['offset']

          first_offset = start_offset if first_offset.nil?

          errors.push({
            :start =>
              "<span " +
                "class=\"error\" " +
                "data-error-id=\"#{e_id}\" " +
                "data-message-id=\"#{message['id']}\">",
            :end => '',
            :range => [start_offset, start_offset],
            :length => end_offset - start_offset,
            :line => message['start']['line']
          })
          errors.push({
            :start => '',
            :end => '</span>',
            :range => [end_offset, end_offset],
            :line => message['start']['line']
          })
        end
        outputs.push({
          :range => [first_offset, first_offset],
          :error => error
        })
      end
      [errors, outputs]
    end

    def escape(str)
      CGI.escapeHTML(str).untaint
    end

    def print_code_section(content, options)
      if options[:line_numbers]
        lines = content.lines.count
        <<-EOF

<div class="language-javascript highlighter-flow">
  <div class="highlight">
    <table style="border-spacing: 0"><tbody>
      <tr>
        <td class="gutter gl" style="text-align: right">
          <pre class="lineno">#{(1..lines).to_a.join("\n")}</pre>
        </td>
        <td class="code"><pre>#{content}</pre></td>
      </tr>
    </tbody></table>
  </div>
</div>

EOF
      else
        <<-EOF

<div class="language-javascript highlighter-flow">
  <pre class="highlight"><code>#{content}</code></pre>
</div>

EOF
      end
    end

    def print_cli_output(content)
      <<-EOF

<div class="language-bash highlighter-flow">
  <pre class="highlight"><code>$&gt; flow</code></pre>
</div>
<div class="language-text cli-error highlighter-flow">
  <pre class="highlight"><code>#{content}</code></pre>
</div>

EOF
    end

    def convert(content)
      content = content.lines.reject { |l|
        l =~ /^\s*\/\/\s*\$ExpectError(\([^)]*\))?$/
      }.join
      tokens = get_tokens(content)
      errors_json = get_error_json(content)
      errors, outputs = get_errors(errors_json)
      items = (tokens + errors + outputs).sort_by {|item|
        # sort by position, then by :length (longest to shortest), which sorts
        # the opening <span> tags on errors.
        [item[:range], -1 * (item[:length] || 0)
      ]}
      sections = []
      out = ""
      cli_output = []
      pragmas = Set.new
      last_loc = 0
      last_was_code = false
      section_start_line = 0
      items.each do |item|
        start_loc, end_loc = item[:range]
        is_code = !item.has_key?(:value) # code or cli output
        if last_was_code && !is_code
          with_output = !cli_output.empty?
          with_nums = pragmas.include?('$WithLineNums') || with_output
          html = print_code_section(
            out.sub(/\A\n+|\n+\z/m, ''),
            { :line_numbers => with_nums }
          )
          if with_output
            html = <<-EOF

<div class="code-sample">
#{html}
<a class="show" href="#" onclick="this.parentNode.className = 'code-sample shown'; return false">show Flow output</a>
<a class="hide" href="#" onclick="this.parentNode.className = 'code-sample'; return false">hide Flow output</a>
#{print_cli_output(cli_output.join("\n\n"))}
</div>

EOF
          end
          sections.push(html)
          out = ""
          cli_output = []
          section_start_line = item[:line]
          pragmas.clear()
        elsif !last_was_code && is_code
          sections.push(out)
          out = ""
          cli_output = []
          section_start_line = item[:line]
          pragmas.clear()
        end

        if item.has_key?(:pragma)
          pragmas.add(item[:pragma])
          last_loc = end_loc # skip the pragma
          last_was_code = true
          section_start_line = item[:line] + 1
          next
        end

        # Catch up over whitespace
        out += escape(content.slice(last_loc, start_loc - last_loc))

        if item.has_key?(:error)
          cli_output.push(
            get_error_output(item[:error], section_start_line)
          ) unless pragmas.include?('$NoCliOutput')
        elsif is_code
          value = content.slice(start_loc, end_loc - start_loc)
          if value =~ /\/\/\s*\$Doc(Issue|Hide)/
            section_start_line += 1 # skip the line
          else
            out += item[:start]
            out += escape(value)
            out += item[:end]
          end
          last_was_code = true
        else
          out += item[:value]
          last_was_code = false
        end
        last_loc = end_loc
      end
      out += escape(content.slice(last_loc, content.length - last_loc))
      sections.push(last_was_code ? print_code_section(out, { :line_numbers => false }) : out)

      out = @markdown.convert(sections.join)
      out += "<script>require(['inlineErrors'], function(inlineErrors) {\n  inlineErrors.highlight(#{JSON.generate(errors_json)});\n});</script>"
      out
    end
  end

  Hooks.register :site, :pre_render do |site|
    FlowdocConverter.tempdir = Dir.mktmpdir
    File.open(File.join(FlowdocConverter.tempdir, '.flowconfig'), 'w') {|f|
      f.write(
        <<-EOF.gsub(/^ {10}/, '')
          [options]
          suppress_comment=\\\\(.\\\\|\\n\\\\)*\\\\$DocIssue
          max_header_tokens=1
        EOF
      )
    }
  end

  Hooks.register :site, :post_render do |site|
    flow = site.config['flow']['path'] || 'flow'
    `#{flow} stop #{FlowdocConverter.tempdir}`
    FileUtils.remove_entry_secure FlowdocConverter.tempdir
  end

end
