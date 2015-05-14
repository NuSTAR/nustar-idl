
PRO run_nuproducts_lightcurve, infile, $
                               binsize=binsize, erange=erange, $
                               usrgti=usrgti, barycorr=barycorr, $
                               region=region, $
                               outdir=outdir, no_lc_corr=no_lc_corr, $
                               outsuffix=outsuffix

; Purpose:
; Easy IDL wrapper for making lighturves using nuproducts.
; syntax: 
;
; run_nuproducts_lightcurve, infile, <opts>
;
; Example:
; run_nuproducts_lightcurve, '80001085002/event_cl/nu80001085002A01_cl.evt.gz', erange=[3 20], region="80001085002/event_cl/src.reg", binsize=100

; Optional inputs (defaults):
; binsize:
; The bin size (in seconds) that you want to use. (100 s)-> binsize=100
; erange:
; Energy range for the lighcurve (3 to 20 keV) -> erange=[3, 20]
; region:
; Full path to a ds9 region file that you want to use for input. If no
; region is supplied then the code automatically uses a large regin that
; should cover the entire focal plane.
; barycorr:
; Apply the barycorr based on the position of the source in the FITS header. 
; no_lc_corr (boolean):
; Don't apply the PSF and vignetting corrections to the lightcurve.
; Note: Turning off these corrections should decrease the run time dramatically
; but should not be used for science analysese (e.g. use this only for checking
; for solar flare increases in the background.

; Optional outputs (defaults):
; outdir
; Full path where you want the lightcurve to go.
; outsiffix
; Optional tag to add to the end of the lightcurve filename. Example: outsuffix='_gtifiltered'

; Author: Brian Grefenstette (bwgref@srl.caltech.edu)
; Last Update: 2015/05/06
 
syntax='syntax: run_nuproducts_lightcurve, infile, <opts> '

; Keyword checks:
 IF ~keyword_set(binsize) THEN binsize = 100.
 IF ~keyword_set(erange) THEN erange = [3, 20]
 IF ~keyword_set(usrgti) THEN usrgti = 'NONE'
 IF ~keyword_set(barycorr) THEN barycorr='no' ELSE barycorr='yes'
 IF ~keyword_set(outdir) THEN outdir = './'
 IF ~keyword_set(no_lc_corr) THEN lc_corr='yes' ELSE lc_corr = 'no'
 IF ~keyword_set(outsuffix) THEN outsuffix = ''
 
 ; Check for HEASoft install
 heasoft = getenv('HEADAS')
 IF strcmp(heasoft, '') THEN message, 'Init heasoft first.'

 ; Check to see if infile exists:
 IF n_elements(infile) EQ 0 THEN message, syntax
 IF ~file_test(infile) THEN message, 'Input file: '+infile+' does not exist.'


 ; Parse input path and filename
 datpath = file_dirname(infile)
 ; Test to see if your file is gzipps:
 IF stregex(infile, 'gz', /boolean) THEN begin
    evtstem = file_basename(infile, '.evt.gz')
 ENDIF ELSE BEGIN
    evtstem = file_basename(infile, '.evt')
 ENDELSE

 ; Change energy range to PI channels:
 pi_range = (erange - 1.6) / 0.04 
 e_low_str = strtrim(string(erange[0], format = '(d8.2)'), 2)
 e_high_str = strtrim(string(erange[1], format = '(d8.2)'), 2)
 pi_low_str = strtrim(string(pi_range[0], format = '(i0)'), 2)
 pi_high_str = strtrim(string(pi_range[1], format = '(i0)'), 2)

 bin_str = strtrim(string(binsize, format = '(d8.2)'), 2)

 ; See which FPM you're using:
 module = 'fail'
 IF stregex(infile, 'A', /boolean) THEN module = 'FPMA'
 IF stregex(infile, 'B', /boolean) THEN module = 'FPMB'
 IF strcmp(module, 'fail') THEN message, 'Cannot determine FPM from the name, please add A or B to filename.'


 

cmd = "nuproducts"
cmd += " indir="+datpath+" infile="+infile
cmd += " instrument="+module

cmd += ' lcpsfflag='+lc_corr+' lcvignflag='+lc_corr
cmd += ' bkgextract=no runmkarf=no runmkrmf=no clobber=yes'
cmd += ' phafile=NONE imagefile=NONE plotdevice=ps'
cmd += ' binsize='+bin_str
cmd += ' pilow='+pi_low_str+' pihigh='+pi_high_str
cmd += ' usrgtifile='+usrgti
cmd += ' outdir='+outdir

; Load the FITS header to get the RA/DEC
header = headfits(infile, exten='EVENTS')
ra = strtrim(string(fxpar(header, 'RA_OBJ')), 2)
dec = strtrim(string(fxpar(header, 'DEC_OBJ')), 2)

; Get paths for below.
auxpath = file_dirname(datpath)+'/../auxil/'
hkpath = file_dirname(datpath)+'/../hk/'

; Get the seqid from the filename:
seqid = strmid(evtstem, 2, 11) 

cmd += " steminputs=nu"+seqid

; Construct the output stem:
stemout=evtstem+'_'+e_low_str+'to'+e_high_str+'keV_'+bin_str+'s'+outsuffix
cmd += ' stemout='+stemout



; Find attitude file:
; Check event_cl directory first:

attfile = (file_search(datpath, '*att.fits'))[0]
IF ~file_test(attfile) THEN BEGIN
   attfile = (file_search(auxpath, '*att.fits*'))[0]
ENDIF
cmd += ' attfile='+attfile


fpm = strmid(module, 3)


hkfile = (file_search(datpath, '*'+fpm+'_fpm.hk*'))[0]
IF ~file_test(hkfile) THEN BEGIN
   hkfile = (file_search(hkpath, '*'+fpm+'_fpm.hk*'))[0]
ENDIF
cmd +=' hkfile='+hkfile


IF strcmp(barycorr, 'yes') THEN BEGIN
   cmd += ' barycorr=yes'
     
   IF ~file_test(orbfile) THEN BEGIN
      orbfile +='.gz'
      IF ~file_test(orbfile) THEN message, 'Missing orbit file: '+orbfile
   ENDIF
   
   cmd += ' orbitfile=$orbfile'
   cmd += ' srcra_barycorr='+ra+' srcdec_barycorr='+dec

ENDIF ELSE cmd +=' barycorr=no'


 ; Check and see if you want to use a region file:
IF ~keyword_set(region) THEN BEGIN
   cmd += ' srcra='+ra+' srcdec='+dec+' srcradius=300'
ENDIF ELSE cmd += 'srcregionfile='+region



;   orbfile = file_search(auxpath, 'orb')

print, 'Constructing lightcurve using nuproducts:'
print, cmd

spawn, cmd







END

