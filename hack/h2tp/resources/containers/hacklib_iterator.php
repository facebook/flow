<?php

namespace HH {
  require_once(__DIR__.SEP.'hacklib_iteratorToken.php');

  /**
   * This serves as a baseclass for iterators for all the containers. It works by
   * making use of an integer index which is incremented within the range
   * 0 - containersize.
   */
  class HACKLIB_iterator implements KeyedIterator {
    // constructor params
    private $count, $token, $getKeyAndValue;
    // current state
    private $index, $keyAndValue;

    /**
     * @param count : The total size of the container.
     * @param token : This is used to make sure that the iterator stops
     *                   working once the container has been modified.
     * @param getKeyAndValue : This is a function that, given the current index,
     *                            returns the current key and value.
     */
    public function hacklib_init($count, $token, $getKeyAndValue) {
      $this->count = $count;
      $this->token = $token;
      $this->getKeyAndValue = $getKeyAndValue;
      $this->rewind();
    }

    public function rewind() {
      if ($this->token->isNotExpired()) {
        $this->index = 0;
        $this->keyAndValue = null;
      }
    }

    public function next() {
      $this->index++;
      $this->keyAndValue = null;
    }

    private function currentKeyAndValue_UNSAFE() {
      if (!$this->keyAndValue) {
        $fn = $this->getKeyAndValue;
        $this->keyAndValue = $fn($this->index);
      }
      return $this->keyAndValue;
    }

    private function validate() {
      if ($this->token->isExpired()) {
        throw new \InvalidOperationException(
          'Collection was modified during iteration');
      }
      if (!$this->valid()) {
        throw new \InvalidOperationException('Iterator is not valid');
      }
    }

    public function current() {
      $this->validate();
      return $this->currentKeyAndValue_UNSAFE()[1];
    }

    public function key() {
      $this->validate();
      return $this->currentKeyAndValue_UNSAFE()[0];
    }

    public function valid() {
      return $this->index < $this->count;
    }
  }

  /**
   * adds a few simple methods to all containers that implement
   * iterators, mainly implemented so we can consolidate all
   * the logic dealing with iterators in one place.
   */
  trait HACKLIB_iteratable {
    //this is a token passed to all iterators so it can be invalidated.
    private $iteratorToken;

    /**
     * All iterators share a token that becomes invalidated the minute the
     * container is mutated. This creates a new token incase the current
     * one does not exist.
     */
    private function getCurrentToken() {
      if (!$this->iteratorToken) {
        $this->iteratorToken = new HACKLIB_IteratorToken();
      }
      return $this->iteratorToken;
    }

    protected function hacklib_expireAllIterators() {
      if ($this->iteratorToken) {
        $this->iteratorToken->expire();
        $this->iteratorToken = null;
      }
    }

    /**
     *  return the key and value at index i for this container
     */
    protected abstract function hacklib_getKeyAndValue($i);

    /**
     *  return the size of the given container
     */
    public abstract function count();

    /**
     *  create a new iterator of the required type.
     */
    protected abstract function hacklib_createNewIterator();

    /**
     * Returns an iterator that points to the beginning of this Container.
     */
    public function getIterator() {
      $iterator = $this->hacklib_createNewIterator();
      $iterator->hacklib_init(
        $this->count(),
        $this->getCurrentToken(),
        function ($i) {
          return $this->hacklib_getKeyAndValue($i);
        }
      );
      return $iterator;
    }
  }
}
