utils-experimental
====

**compiler** package contains 

* *SourceCodePrinter*, based on [scala-refactoring](http://scala-refactoring.org/)
* experiments with interpreter
 
**shell** package contains **TODO**

* [Test reports](shell/test-reports) 
* *ProcessWrappers* advanced process execution utils
* *ProcessReaderWrappers* utils for reading asynchronously a process's outputs

---

### SBT Keys
*tests run only on Linux and OsX systems*

sbt `copy-test-reports` task copies specs2 markdown reports to `test-reports-copy-dir`

sbt `clean-test-reports` task cleans `test-reports-copy-dir`

set `clean-test-reports-on-clean-auto := true` to clean `test-reports-copy-dir` on `clean`

set `autoAddReportsToGit := true` to add reports to git repo on `copy-test-reports`

"test-reports" directory is set by `copyTestReportsDir <<= baseDirectory(base => Some(base / "test-reports"))`

to disable test reports copying, set `copyTestReportsDir := None`
