# sbt
The Simple Binary Tester.

Each test has a shell command, a name and a list of conditions. sbt runs the shell 
command and tests the conditions on it's output. 
If one condition returns false, the test is considered to be a failure 
otherwise the test passes.
## Syntax
The syntax generally looks like this
```
("test_name", "command arg0 arg1 argn") should {condition_name(arg0, arg1, argn) condition_name()}
```
a script can have multiple tests.
example:
```
("breaks", "echo -n does this test work") should {contain("byeeeee") contain("nope")}
("works", "echo -n does this test work") should {
	contain("does")
	contain("work")
}
```
The script above contains two tests one that breaks and one that works. 
The output should be something like
```
Test 'breaks' failed with output 'does this test work'
Test 'works' passed
```
