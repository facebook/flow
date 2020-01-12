# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

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
