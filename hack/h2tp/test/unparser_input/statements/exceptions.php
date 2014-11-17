<?hh
class MyException extends Exception {}

class Foo {
  public static function bar() {
    try {
      try {
        $error = 'Always throw this error';
        throw new MyException($error);
        echo ('Never executed');
      } catch (MyException $e) {
        echo('Caught MyException: '.$e->getMessage()."\n");
        throw $e;
      } catch (Exception $e) {
        echo('Will not execute');
      }
    } catch (Exception $e) {
      echo('Caught Exception: '.$e->getMessage()."\n");
    } finally {
      echo("This always executes\n");
    }
  }
}
Foo::bar();
