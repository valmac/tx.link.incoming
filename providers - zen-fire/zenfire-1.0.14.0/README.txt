
Zen-Fire .NET API
================================================================================

Use
================================================================================

One thing to note is the Zen-Fire library itself is not thread safe, so any
calls to it in a multithread environment should be synchronized.  It does
spawn a thread on login() to use in the background for network and callbacks,
so any function hooked will be called from a different thread than the one
Zen-Fire is initialized with.

Logging in again will invalidate any objects from the connection.

Compiling and Linking
================================================================================

Programs using zenfire.dll will need access to:

 - zlibwapi.dll (provided)
 - Visual C++ 2008 SP1 Runtime (downloadable from http://www.microsoft.com/downloads/details.aspx?familyid=A5C84275-3B97-4AB7-A40D-3802B2AF5FC2)


Contact
================================================================================

Please contact matt@zen-fire.com with any questions, comments, bugs, etc.
Thanks.

================================================================================
================================================================================

