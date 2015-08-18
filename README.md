nustar-idl
==========

Repository for NuSTAR IDL-based analysis and support scripts. The individual directories will have their own README files.

To report an issue please use the "Issues" button at top right.

To contribute to this repository, please fork off your own version, add you changes, and then institue a pull request. The admins for this repo will then merge these requests as they arise.

*** Requirements ***

The nustar-idl repo is heavily dependent on the AstroLib and Coyote libraries. You MUST have these libraries in your IDL !path. 

Fortunately, these libraries are also now on GitHub here:

The IDL AstroLib: https://github.com/wlandsman/IDLAstro

The Coyote IDL Libraries: https://github.com/davidwfanning/idl-coyote

A simple way to do this is:

(1) Make some local directory for your IDL scripts:

mkdir $HOME/local-idl

(2) Check out the various repositories:

cd $HOME/local-idl

git clone https://github.com/NuSTAR/nustar-idl.git nustar-idl

git clone https://github.com/wlandsman/IDLAstro.git astrolib-idl

git clone  https://github.com/davidwfanning/idl-coyote coyote-idl

(3) Modify your IDL_STARTUP script to include the above in your !path

You should already have an IDL_STARTUP script defined somewhere in your .bash_profile or equivalent. It should be called out like this:

  export IDL_STARTUP=$HOME/idl_startup.pro

Add the following line to that script:

base = getenv('HOME')

!path = expand_path('+'+base+'/local-idl/')+':'+ !path

where you should modify the path to "local-idl" to where ever you cloned the repositories.
