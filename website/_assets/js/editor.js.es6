/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

function toArray(list) {
  return Array.prototype.slice.call(list);
}

var EDITOR_CLASS = '.editor';
var EDITOR_CONTENT_CLASS = '.editor-content';
var EDITOR_GUTTER_CLASS = '.editor-gutter';
var EDITOR_CODE_CLASS = '.editor-code';
var EDITOR_DATA_CLASS = '.editor-data';
var EDITOR_MESSAGES_CLASS = '.editor-messages';

var FLOW_ERROR_CLASS = '.flow-error';
var FLOW_ERROR_TARGET_CLASS = '.flow-error-target';
var FLOW_ERROR_TARGET_ACTIVE_CLASS = 'flow-error-target-active';
var FLOW_ERROR_MESSAGE_ACTIVE_CLASS = 'flow-error-active';
var FLOW_ERROR_MESSAGE_HIGHLIGHTED_CLASS = 'flow-error-highlighted';

function Editor(block) {
  this.block = block;
  this.data = JSON.parse(block.querySelector(EDITOR_DATA_CLASS).textContent);

  this.overlay = this.block.querySelector(EDITOR_MESSAGES_CLASS);
  this.errors = toArray(block.querySelectorAll(FLOW_ERROR_CLASS));
  this.targets = toArray(block.querySelectorAll(FLOW_ERROR_TARGET_CLASS));

  this.onMouseEnter = this.onMouseEnter.bind(this);
  this.onMouseLeave = this.onMouseLeave.bind(this);

  this.bindEvents();
}

Editor.prototype.getError = function(errorId) {
  return this.errors.find(function(error) {
    return error.dataset.errorId === errorId;
  });
};

Editor.prototype.getErrorMessage = function(error, messageId) {
  return error.querySelector('[data-message-id="' + messageId + '"]');
};

Editor.prototype.getErrorTargets = function(errorId) {
  return this.targets.filter(function(target) {
    return target.dataset.errorId === errorId;
  });
};

Editor.prototype.focusError = function(target) {
  var data = target.dataset;

  var error = this.getError(data.errorId);
  var message = this.getErrorMessage(error, data.messageId);
  var targets = this.getErrorTargets(data.errorId);

  error.classList.add(FLOW_ERROR_MESSAGE_ACTIVE_CLASS);
  message.classList.add(FLOW_ERROR_MESSAGE_HIGHLIGHTED_CLASS);

  targets.forEach(function(target) {
    target.classList.add(FLOW_ERROR_TARGET_ACTIVE_CLASS);
  });

  if (this.overlay.style.top === '') {
    this.overlay.style.top = (target.offsetTop + target.offsetHeight) + 'px';
  }
};

Editor.prototype.unfocusError = function(target) {
  var data = target.dataset;

  var error = this.getError(data.errorId);
  var message = this.getErrorMessage(error, data.messageId);
  var targets = this.getErrorTargets(data.errorId);

  error.classList.remove(FLOW_ERROR_MESSAGE_ACTIVE_CLASS);
  message.classList.remove(FLOW_ERROR_MESSAGE_HIGHLIGHTED_CLASS);

  targets.forEach(function(target) {
    target.classList.remove(FLOW_ERROR_TARGET_ACTIVE_CLASS);
  });

  this.overlay.style.top = '';
};

Editor.prototype.onMouseEnter = function(event) {
  this.focusError(event.currentTarget);
};

Editor.prototype.onMouseLeave = function(event) {
  this.unfocusError(event.currentTarget);
};

Editor.prototype.bindEvents = function() {
  var onMouseEnter = this.onMouseEnter;
  var onMouseLeave = this.onMouseLeave;

  this.targets.forEach(function(target) {
    target.addEventListener('mouseenter', onMouseEnter);
    target.addEventListener('mouseleave', onMouseLeave);
  });
};

Editor.prototype.unbindEvents = function() {
  var onMouseEnter = this.onMouseEnter;
  var onMouseLeave = this.onMouseLeave;

  this.targets.forEach(function(target) {
    target.removeEventListener('mouseenter', onMouseEnter);
    target.removeEventListener('mouseleave', onMouseLeave);
  });
};

Editor.getEditors = function() {
  return toArray(document.querySelectorAll(EDITOR_CLASS));
};

Editor.init = function() {
  var editors = Editor.getEditors().map(function(block) {
    return new Editor(block);
  });
};

export default Editor;
