<?hh
namespace StringsNS {
  class Strings {
    const URL = 'url';
  }
}
namespace {
  type UrlData = shape(\StringsNS\Strings::URL => string, 'name' => string, 'hits' => int);
  function foo(UrlData $d) {
    $d['hits']++;
    echo("{$d['name']} : {$d['hits']}\n");
  }
  $s = shape(\StringsNS\Strings::URL => "http://www.snopes.com", 'name' => "Snopes", 'hits' => 5);
  echo($s['hits']."\n");
  foo($s);
  echo($s['hits']."\n");
  echo($s['url']."\n");
}
