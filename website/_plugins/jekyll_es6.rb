require 'execjs'
require 'babel/transpiler'
require 'sprockets/es6'
require 'sprockets/uri_utils'
require 'ostruct'

Sprockets::ES6.configuration ||= OpenStruct.new
Sprockets::ES6.configuration.modules = 'amd'
Sprockets::ES6.configuration.moduleIds = true

# Babel uses console, which doesn't exist in ExecJS. This stubs it out so that
# it doesn't error. It'd be cool to capture these messages and log them via
# Jekyll.logger, but that would be really invasive. For an example, see
# sprockets-babel: https://github.com/70mainstreet/sprockets-babel/blob/f49a9d83be207af4f08433c8e62c107eb6f420ee/lib/sprockets/babel.rb#L63
module Babel
  module Transpiler
    PREFACE = <<-JS
      var self = this;
      var console = {
        log: function() {},
        warn: function() {},
        error: function() {}
      };
    JS

    def self.context
      @context ||= ExecJS.compile(PREFACE + File.read(script_path))
    end
  end
end

module Sprockets
  # Patch sprockets-es6 to automatically set up dependencies on `import`'d
  # modules.
  class ES6
    MODULE_MAP = {
      "jquery" => "jquery/dist/jquery.slim.min.js",
      "docsearch" => nil
    }.freeze

    alias _preflow_call call
    def call(input)
      Jekyll.logger.debug "sprockets-es6:", "babelifying #{input[:filename]}"

      # Run babel on the source data (same as upstream)
      data = input[:data]
      result = input[:cache].fetch(@cache_key + [input[:filename]] + [data]) do
        transform(data, transformation_options(input))
      end

      # Extract all the imported modules and add them as linked Sprockets deps,
      # as if you'd written `//= link moduleName` at the top of the file.
      links = Set.new(input[:metadata][:links])
      result
        .fetch("metadata", {})
        .fetch("modules", {})
        .fetch("imports", [])
        .each { |import|
          source = MODULE_MAP.fetch(import["source"], import["source"])
          links.add resolve_import(input, source) if not source.nil?
        }

      { data: result['code'], links: links, amd: true }
    end

    private
    def resolve_import(input, source)
      env = input[:environment]
      asset = env.find_asset(source, accept: @content_type)

      raise FileNotFound, "could not find file: #{source}" if asset.nil?

      # if the imported dependency is also required via `//= require source`,
      # then don't mark it as "used", so that we don't build a separate asset
      # for it.
      env.parent.manifest.add(asset) unless asset_is_required?(input, asset)

      asset.uri
    end

    def asset_is_required?(input, asset)
      loaded_asset = input[:environment].find_asset(asset.logical_path, {
        accept: @content_type,
        pipeline: :self
      })
      filename, params = URIUtils.parse_asset_uri(loaded_asset.uri)
      unloaded_uri = URIUtils.build_asset_uri(filename, params.merge(id: nil))
      input[:metadata][:required].include?(unloaded_uri)
    end
  end

  # Wrap JS files with define()
  class AMD
    IGNORE = %W(
      requirejs/require.js
      prelude.js
    )

    def self.call(input)
      env = input[:environment]
      relative_path = env.split_subpath(input[:load_path], input[:filename])

      # Skip require.js, we don't want to AMD-ify it.
      return nil if IGNORE.include? relative_path

      # Skip if already AMD
      return nil if input[:metadata][:amd]

      Jekyll.logger.debug "sprockets-amd:", "AMDifying #{input[:filename]}"

      if relative_path =~ %r(jquery/dist/jquery(\.slim)?(\.min)?\.js)
        name = 'jquery'
      else
        name = input[:name]
      end

      code = <<-JS
define("#{name}", ["require", "exports", "module"], function(require, exports, module) {
#{input[:data]}
});
      JS
      { data: code }
    end
  end
 register_postprocessor 'application/javascript', AMD

end
