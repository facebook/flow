Running the tests
To run the tests, first compile flow using make. Then run bash ./runtests.sh bin/flow

There is a make test target that compiles and runs tests.

To run a subset of the tests you can pass a second argument to the runtests.sh file.

For example: bash runtests.sh bin/flow class | grep -v 'SKIP'
