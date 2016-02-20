require 'fileutils'
require 'json'
require 'open3'
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

        site.pages << FlowdocPage.new(site, site.source, dirname, basename)

        true
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
      {
        :start => "<span class=\"#{context} #{type}\">",
        :end => "</span>",
        :range => range
      }
    end

    def convert_comment(comment)
      type, loc, value, range = comment.values_at('type', 'loc', 'value', 'range')
      if type == 'Block' && loc['start']['column'] == 0
        if value == " @flow " || value =~ YAML_REGEX
          {
            :value => "",
            :range => range
          }
        else
          {
            :value => @markdown.convert(value.strip.gsub(/^[ \t]+/, '')),
            :range => range
          }
        end
      else
        {
          :start => '<span class="comment">',
          :end => '</span>',
          :range => range
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

      line_lengths = content.lines.map { |line| line.length }
      line_offsets = [0]
      line_lengths.each_with_index do |line_length, index|
        line_offsets[index + 1] = line_offsets[index] + line_length
      end

      errors = []
      errors_json.each_with_index do |error, e_index|
        e_id = e_index + 1
        messages = []
        error['message'].each_with_index do |message, m_index|
          m_id = e_id * 1000 + (m_index + 1)
          start_line, start_col = message.values_at('line', 'start')
          end_line, end_col = message.values_at('endline', 'end')

          msg_json = {
            'id' => "M#{m_id}",
            'description' => message['descr']
          }

          if start_line && start_line > 0 && end_line && end_line > 0
            start_offset = line_offsets[start_line - 1] + start_col - 1
            end_offset = line_offsets[end_line - 1] + end_col

            msg_json['start'] = {
              'line' => start_line,
              'column' => start_col,
              'offset' => start_offset
            }
            msg_json['end'] = {
              'line' => end_line,
              'column' => end_col + 1,
              'offset' => end_offset
            }
          end

          messages.push(msg_json)
        end
        errors.push({
          'id' => "E#{e_id}",
          'messages' => messages
        })
      end
      errors
    end

    def get_errors(errors_json)
      errors_json.reduce([]) do |errors, error|
        e_id = error['id']
        error['messages'].each do |message|
          next unless message.has_key?('start') && message.has_key?('end')

          start_offset = message['start']['offset']
          end_offset = message['end']['offset']
          errors.push({
            :start =>
              "<span " +
                "class=\"error\"" +
                "data-error-id=\"#{e_id}\" " +
                "data-message-id=\"#{message['id']}\">",
            :end => '',
            :range => [start_offset, start_offset]
          })
          errors.push({
            :start => '',
            :end => '</span>',
            :range => [end_offset, end_offset]
          })
        end
        errors
      end
    end

    def convert(content)
      content = content.lines.reject { |l| l =~ /^\s*\/\/\s*\$ExpectError/ }.join
      tokens = get_tokens(content)
      errors_json = get_error_json(content)
      errors = get_errors(errors_json)
      items = (tokens + errors).sort_by {|item| item[:range]}
      out = ""
      last_loc = 0
      in_code = false
      items.each do |item|
        start_loc, end_loc = item[:range]
        is_markdown = item.has_key?(:value)
        out += '</pre>' if in_code && is_markdown
        out += content.slice(last_loc, start_loc - last_loc)
        if is_markdown
          out += item[:value]
          in_code = false
        else
          out += '<pre class="code">' if not in_code
          out += item[:start]
          out += content.slice(start_loc, end_loc - start_loc)
          out += item[:end]
          in_code = true
        end
        last_loc = end_loc
      end
      out += content.slice(last_loc, content.length - last_loc)
      out += '</pre>' if in_code
      out += '<script src="//code.jquery.com/jquery-git2.min.js"></script>'
      out += '<script src="/static/dhtml.js"></script>'
      out += "<script>highlightErrors(#{JSON.generate(errors_json)})</script>"
      out
    end
  end

  Hooks.register :site, :pre_render do |site|
    FlowdocConverter.tempdir = Dir.mktmpdir
    FileUtils.touch(File.join(FlowdocConverter.tempdir, '.flowconfig'))
  end

  Hooks.register :site, :post_render do |site|
    flow = site.config['flow']['path'] || 'flow'
    `#{flow} stop #{FlowdocConverter.tempdir}`
    FileUtils.remove_entry_secure FlowdocConverter.tempdir
  end

end
