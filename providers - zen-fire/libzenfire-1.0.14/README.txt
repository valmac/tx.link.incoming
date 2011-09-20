
Zen-Fire API
================================================================================

This is the pre-release of our new API.  While the underlying functional
layer is stable and well tested, the interface has completely changed and
should still be considered beta.

Please see CHANGES.txt for release specific notes.

Use
================================================================================

Full documentation is not yet complete, but the API functions are all
documented in docs/zenfire/index.html and there are examples in the examples
directory.

One thing to note is the Zen-Fire library itself is not thread safe, so any
calls to it in a multithread environment should be synchronized.  It does
spawn a thread on login() to use in the background for network and callbacks,
so any function hooked will be called from a different thread than the one
Zen-Fire is initialized with.


Compiling and Linking
================================================================================

To compile under windows, it requires the VC++ Feature Pack containing tr1
(ISO/IEC TR 19768, C++ Library Extensions).  For VC++ Standard and above, it
can be downloaded from
http://www.microsoft.com/downloads/details.aspx?FamilyID=d466226b-8dab-445f-a7b4-448b326c48e7
and is included in VC++ Service Pack 1 for Express and above.

Binaries compiled against Zen-Fire will need access to zlibwapi.dll (provided)
for internal compression.

Caveats / Known bugs
================================================================================


Contact
================================================================================

Please contact matt@zen-fire.com with any questions, comments, bugs, etc.
Thanks.

================================================================================
================================================================================

