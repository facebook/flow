<?hh
class Alliteration {
  public $firstAllit = "cast vicariously as both victim and villain";
  public $clause = array(
    'part1' => 'no mere veneer of vanity'
  );
  public $nested = array(
    array("is a vestige of the", "vox populi", "now vacant")
  );
}
class Foo {
  public function bar($bag_of_stuff) {
    echo('some
    embedded
    newlines');
    echo "\n";
    echo 'Arnold once said: "I\'ll be back"';
    echo "\n";
    $v = "Voila! ";
    $all = new Alliteration();
    echo "$v In view humble vaudevillian veteran".
      "$all->firstAllit $bag_of_stuff[0].\n";
    echo "This visage, {$all->clause['part1']}, ".
      "{$all->nested[0][0]} \"{$all->nested[0][1]}\"".
      " {$all->nested[0][2]}\n";
  }
}
$f = new Foo();
$f->bar(array("by the vicissitudes of fate"));
