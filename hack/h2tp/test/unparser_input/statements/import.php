<?hh
echo(require_once dirname(__FILE__).'/../../foo.php');
include_once dirname(__FILE__) . '/../../foo.php';
function foo() {
  $x = require(dirname(__FILE__) . '/../../foo.php');
  $y = $x.(include(dirname(__FILE__) . '/../../foo.php'));
  echo($y);
}
