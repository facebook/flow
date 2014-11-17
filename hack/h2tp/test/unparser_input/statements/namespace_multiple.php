<?hh
namespace my\name;

const CONNECT_OK = 1;

namespace my\name\foo;

const CONNECT_OK = 2;

namespace somethingelse;

const CONNECT_OK = 3;

$x = \my\name\CONNECT_OK;
echo($x);
$x = \my\name\foo\CONNECT_OK;
echo($x);
$x = \somethingelse\CONNECT_OK;
echo($x);
