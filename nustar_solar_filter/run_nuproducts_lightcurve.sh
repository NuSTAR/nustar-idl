#!/bin/bash

# usage: run_products.sh OBSID A/B source.reg
# e.g.:
# run_products.sh 60001043006 A source.reg
# Dumps data products into a new ./OBSID/source directory

# Setup a local pfiles directory for this run.
function headas_locpfiles { export PFILES="$1;$HEADAS/syspfiles"; }


# Check to see if you've already initialized the HEADAS:
if [ -X "$HEADAS" ]; then
    echo "Need to initialize HEASoft first."
    exit 1
fi

# Set up your local NuSTAR science environment here:
if [ -z "$NUSTARSETUP" ]; then 
    echo "Need to set the NUSTARSETUP environment variable!"
    exit
fi
source $NUSTARSETUP

#OBSID=$1
#REGNAME=$2

OBSID=30001040_Sco_X1/30001040002
REGNAME=src.reg
REGSTEM=`basename $REGNAME .reg`
clockfile=nuCclock20100101v044.fits

if [ ! -d gti_spec ]; then
    mkdir gti_spec
fi




OUTDIR=ave_spec/
if [ ! -d $OUTDIR ]; then
    mkdir $OUTDIR
fi

for MOD in A B
do
    
    LOCPFILES=${OUTDIR}/$$_pfiles${MOD}
    if [ ! -d $LOCPFILES ]; then
	mkdir $LOCPFILES
    fi
    headas_locpfiles $LOCPFILES
    
    SRCREGIONFILE=${OBSID}/event_cl/${REGSTEM}${MOD}.reg
        
    STEMOUT=${REGSTEM}${MOD}
    
    INSTRUMENT=FPM${MOD}
    DATPATH=${OBSID}/event_cl
    

    STEM=nu`basename $OBSID`
    infile=${DATPATH}/${STEM}${MOD}01_cl.evt
    LOGFILE=$OUTDIR/${STEMOUT}.log
    ORBFILE=$OBSID/auxil/${STEM}_orb.fits
    
    runmkarf=yes
    runmkrmf=yes
    clobber=yes
    bkgextract=no
    cleanup=yes
    rungrppha=yes
    
    echo $LOCPFILES > $LOGFILE
    cmd="nuproducts \
        indir=$DATPATH \
        infile=$infile \
        instrument=$INSTRUMENT \
        steminputs=$STEM \
        stemout=$STEMOUT \
        srcregionfile=$SRCREGIONFILE \
        bkgextract=$bkgextract \
        outdir=$OUTDIR \
        runmkarf=$runmkarf runmkrmf=$runmkrmf \
        clobber=$clobber \
        cleanup=$cleanup \
        rungrppha=$rungrppha \
        barycorr=yes clockfile=$clockfile \
        lcfile=NONE \
        orbitfile=$ORBFILE \
        srcra_barycorr=244.9794458 srcdec_barycorr=-15.6402833 \
        binsize=128 pixbin=2 \
        imagefile=NONE"
    echo ${cmd}
    ${cmd} >> $LOGFILE

done
