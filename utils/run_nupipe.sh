#!/bin/sh
# usage: run_pipe.sh PATH/TO/OBSID

# syntax: run_pipe.sh INDIR
if [ $# != 1 ] ; then
    echo Syntax:  run_pipe.sh PATH_TO_OBSID
    exit 1
fi

# Note, PATH_TO_OBSID is assumed to be relative to the current
# directory:

INDIR=$1


# Set up your local NuSTAR science environment here:
if [ -z "$HEADAS" ]; then
    echo "Need to set the HEASoft environment!"
    exit
fi

OUTDIR=$INDIR/event_cl
if [ ! -d $OUTDIR ]; then

    mkdir -m 750 $OUTDIR
fi


# Set the pfiles to point to $INDIR/PID_pfiles
# Assumes that INDIR is relative to the current directory
LOCPFILES=${OUTDIR}/$$_pfiles
if [ ! -d $LOCPFILES ]; then
    mkdir $LOCPFILES
fi
export PFILES="$LOCPFILES;$HEADAS/syspfiles"


# Assume that INDIR will be the complete path, and we only want the last bit
# for the stem inputs:
STEMINPUTS=nu`basename ${1}`
logfile=$OUTDIR/$$_pipe.log

# Set the entry/exit stages here if you want to 
# change it from the default of 1 and 2, respectively.
# Only set EXISTAGE=3 if you actually know what you're doing and have
# added correct keywords for effective area, grprmf, vignetting, etc below.
ENTRYSTAGE=1
EXITSTAGE=2





echo Running pipeline with command:

cmd="nupipeline \
clobber=yes \
indir=$INDIR steminput=$STEMINPUTS \
outdir=$OUTDIR \
entrystage=$ENTRYSTAGE exitstage=$EXITSTAGE \
pntra=OBJECT pntdec=OBJECT"
echo $cmd

echo $cmd > $logfile
$cmd >> $logfile 2>&1 





