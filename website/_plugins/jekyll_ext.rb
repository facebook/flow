module Jekyll
  module Converters
    class Markdown::FlowMarkdownParser < Markdown::RedcarpetParser
      def initialize(config)
        super(config)

        @renderer = Class.new(@renderer) do
          def header(text, level)
            if level <= 1 || level >= 5
              "<h#{level}>#{text}</h#{level}>"
            else
              hash = text
                .downcase
                .gsub(/<[^>]+>/, '')      # strip HTML tags
                .gsub(/[^a-z0-9]/, '-')   # strip non-alphanumeric
                .gsub(/-+/, '-')          # collapse multiple -'s
                .gsub(/^-|-$/, '')        # trim -'s
              <<-EOF
<h#{level}>
  #{text}
  <a id="#{hash}" class="hashref"></a>
  <a href="##{hash}" class="hash">#</a>
</h#{level}>
              EOF
            end
          end
        end
      end
    end
  end
end
