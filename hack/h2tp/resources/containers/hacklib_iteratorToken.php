<?php
// Copyright 2004-present Facebook. All Rights Reserved.

namespace HH {

  class HACKLIB_IteratorToken {
    public function __construct() {
      $this->expired = false;
    }
    public function isExpired() {
      return $this->expired;
    }
    public function isNotExpired() {
      return !$this->expired;
    }
    public function expire() {
      $this->expired = true;
    }
  }
}
