# LPMud.c

This is the original 3.1.2 driver for LPMud.  It has been fixed so that it compiles on Ubuntu 20.04.

I am currently working to clean up any warnings.

The Makefile now works with Eclipse.  Initially eclipse couldn't handle the pipe to efun_defs and would crash the build. I changed it to create a temporary file and then read the file, which seems to work fine.

A lib folder is included.  The default 2.4.5 mudlib.  It currently points to my home directory.  This will need to be changed in your own build (the variable is in the Makefile.)
