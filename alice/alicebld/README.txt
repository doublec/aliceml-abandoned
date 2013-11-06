Environment:
* Linux:
  * Started from cron at 2am
  * Run as user alicebld on taifun
  * Set limits: 384MB virtual memory
  * Niced 5
* Windows:
  * Run on heaven
  * Rest to be determined

Procedure:
* Create a fresh build directory within /local/alicebld
  (remove eventual leftovers from last build, after `du'ing them).
* Retrieve the latest Mozart version (trunk) from CVS.
* Build Mozart.  If build fails, stop.
* Extend PATH environment variable.
* Retrieve the latest version (trunk) from CVS.
* Run install.sh for the Mozart backend,
  piping the output into a make.log file.
  Test exit code to check whether the build succeeded.
  * If yes:
    * Package up install directory
      (as tgz on Linux, as an installer on Windows).
    * Build examples and test cases;
      run regression test suite.
    * Tag latest version as last-successful-build.
  * Else:
    * Attempt four retries (to determine whether error was transient).
    * Save make.log file.
    * Remove the whole build directory.
    * Reattempt with revision tagged last-successful-build
      (to see whether it broke due to changes in the environment).
    * If last-successful-build was successful, generate
      diffs between last-successful-build and HEAD.
* Run build for the SEAM backend.

Outcome:
* Total time and disk space taken.
* Whether the build was successful (without transient errors).
  * If yes, include timings for build1, build2, build3, libraries.
  * If not, whether the rebuild of last-successful-build was successful.
* Attach make.log file.
* Generate HTML overview.
* Send summary mail to ps.alice.build newsgroup.
  If the summary contains error messages, also send
  to stockhausen@ps.uni-sb.de.

Precautions:
* Guard against crash of build script (using a sub-shell).
* Check all subprocesses' exit codes.

Additional Wishlist:
* Run clean.sh after build, and diff the working directory
  with a fresh checkout (to test whether clean really cleans
  and that no checked-in files are modified).
* Before and after clean, run cvs up to check that no `?' entries
  appear (to check that .cvsignore always contains all generated files).
