require 'sprockets/es6'
require 'ostruct'

Sprockets::ES6.configuration ||= OpenStruct.new
Sprockets::ES6.configuration.modules = 'amd'
Sprockets::ES6.configuration.moduleIds = true
