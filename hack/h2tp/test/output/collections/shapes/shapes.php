<?php
namespace StringsNS {
  require_once ($GLOBALS['HACKLIB_ROOT']);
  class Strings {
    const URL = 'url';
  }
}
namespace {
  function foo($d) {
    $d[\hacklib_id('hits')]++;
    echo ("{$d['name']} : {$d['hits']}\n");
  }
  $s = array(
    \StringsNS\Strings::URL => "http://www.snopes.com",
    'name' => "Snopes",
    'hits' => 5
  );
  echo ($s[\hacklib_id('hits')]."\n");
  foo($s);
  echo ($s[\hacklib_id('hits')]."\n");
  echo ($s[\hacklib_id('url')]."\n");
}
