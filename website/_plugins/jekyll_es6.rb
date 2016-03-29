require 'sprockets/es6'
require 'sprockets/uri_utils'
require 'ostruct'

Sprockets::ES6.configuration ||= OpenStruct.new
Sprockets::ES6.configuration.modules = 'amd'
Sprockets::ES6.configuration.moduleIds = true

module Sprockets
  class ES6
    MODULE_MAP = {
      "jquery" => "jquery/dist/jquery.slim.min.js",
      "docsearch" => nil
    }.freeze

    alias _preflow_call call
    def call(input)
      # Run babel on the source data (same as upstream)
      data = input[:data]
      result = input[:cache].fetch(@cache_key + [input[:filename]] + [data]) do
        transform(data, transformation_options(input))
      end

      # Extract all the imported modules and add them as linked Sprockets deps,
      # as if you'd written `//= link moduleName` at the top of the file.
      imports = result
        .fetch("metadata", {})
        .fetch("modules", {})
        .fetch("imports", [])
        .map { |import|
          source = MODULE_MAP.fetch(import["source"], import["source"])
          source.nil? ? nil : resolve_import(input, source)
        }
        .compact
      links = Set.new(input[:metadata][:links])
      links.merge imports

      { data: result['code'], links: links }
    end

    private
    def resolve_import(input, source)
      env = input[:environment]
      asset = env.find_asset(source, { accept: @content_type, pipeline: :self })

      # if the imported dependency is also required via `//= require source`,
      # then don't mark it as "used", so that we don't build a separate asset
      # for it.
      filename, params = URIUtils.parse_asset_uri(asset.uri)
      unloaded_uri = URIUtils.build_asset_uri(filename, params.merge(id: nil))
      is_required = input[:metadata][:required].include?(unloaded_uri)
      env.parent.used.add(asset) unless is_required

      asset.uri
    end
  end
end
