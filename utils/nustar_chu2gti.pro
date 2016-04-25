
; Purpose:
; IDL script for generating GTIs based on which startracker (CHU)
; combination the spacecraft is using to point the telescope. This
; will mostly be useful when using SCIECE_SC (Mode 06) data as in this
; case the pipeline uses the spacecraft bus solution to project
; photons on the sky. See the NuSTAR software users guide for more
; information.

; While the graphical output is included here to guide the eye, we
; recommend reading the intput image into XSELECT and applying the
; GTI filters produced by this script when attempting to make regions
; files for each of the CHU combinations.

; Note that this may only be feasible if there is a significant amount
; of exposure in the SCIENCE_SC mode and/or the source is bright.

; Syntax
; nustar_chu2gti, infile, <opts>

; Inputs
; infile:
; Full path the input event file that you want to filter.


; Outputs:
; GTI FITS files containing each combination of CHUs. These can be
; used as "usrgti" inputs to the NuSTARDAS pipeline. The filename
; convention is nu80001085002A06_cl_chuXX_gti.fits where XX is the
; CHU combinaton.


; Options (defaults):
; outdir:
; Optional output directory. Default is the same location as the
; infile.
; show:
; Show a summary plot that indicate the momvement per CHU combination.
; ps:
; Save a PostScript version of the above summary plot.
;
; Example:
; nustar_chu2gti, '80001085002/event_cl/nu80001085002A01_cl.evt.gz', /show


; Dependencies:
;
; Relies on the AstroLib for MRDFITS/MWRFITS
; Relies on Coyote library for cgPS_Open/Close and cgColor
;  
; See the readme file in the nustar-idl repo for instructions on how
; to install these libraries.



; History:
; 2015/05/14 - Converted to "public" version by Brian Grefenstette
; 2015/05/13 - Original development by Kristin Madsen


PRO nustar_write_chu_gti, mask, time, combo, name, outdir

  starttime = 0
  stoptime = 0
  if mask[0] eq combo then  starttime=[starttime,time[0]]
  for i=1, n_elements(mask)-1 do begin
    if (mask[i] eq combo) and (mask[i-1] ne combo) then starttime=[starttime,time[i]]
    if (mask[i] ne combo) and (mask[i-1] eq combo) then stoptime=[stoptime,time[i]]
  endfor
  nstart = n_elements(starttime)
  nstop = n_elements(stoptime)
  if nstart ne nstop then stoptime=[stoptime,time[i-1]]

  gti = replicate({start:0.0,stop:0.0},nstart-1)

  gti.start = starttime[1:*]
  gti.stop = stoptime[1:*]

  hh = [$
        "EXTNAME = 'GTI     '           / name of this binary table extension",$            
        "MJDREFI = 55197                / MJD reference day",$                              
        "MJDREFF = 7.660185200000000E-04/ MJD reference day",$                              
        "TSTART  =  "+string(min(gti.start)),$               
        "TSTOP   =  "+string(max(gti.stop)),$                 
        "TIMESYS = 'TT      '           / Time system",$                                    
        "TIMEUNIT= 's       '           / Time unit",$
        " "]             
                         
  mwrfits,gti, outdir+'/'+name+'_gti.fits',hh,/CREATE

END



PRO nustar_chu2gti, infile, $
                       outdir=outdir, show = show, ps=ps, save = save


  myname='nustar_chu2gti'
  syntax='syntax: '+myname+', infile, <opts> '


                                ; Error checking...
                                ; Check for HEASoft install
  heasoft = getenv('HEADAS')
  IF strcmp(heasoft, '') THEN message, myname+': Init heasoft first.'

                                ; Check to see if infile exists:
  IF n_elements(infile) EQ 0 THEN message, syntax
  IF ~file_test(infile) THEN message, myname+': Input file: '+infile+' does not exist.'
  

                                ; Parse input path and filename
  datpath = file_dirname(infile)

                                ; Get paths for below.
  
  auxpath = datpath+'/../auxil/'
  hkpath = datpath+'/../hk/'

                                ; Set the outut directory if not given as a keyword:
  IF ~keyword_set(outdir) THEN outdir = datpath 


                                ; Get the CHU file. This should catch
                                ; both .fits and .fits.gz
  chufile = file_search(hkpath, '*_chu123.fits*')

  ; Check to make sure that the chufile exists:
  IF ~file_test(chufile) THEN message, 'nustar_chu2gti: File not found: '+chufile+' '+hkpath


                                ; Check to see if the event file is
                                ; gzipped or not and get the stem:
  IF stregex(infile, 'gz', /boolean) THEN begin
     evtstem = file_basename(infile, '.evt.gz')
  ENDIF ELSE BEGIN
     evtstem = file_basename(infile, '.evt')
  ENDELSE
  

  if keyword_set(ps) THEN BEGIN
     outfile = outdir+'/'+evtstem+'_chucombos.ps'
     cgPS_Open, outfile, /quiet
     show = 1
  ENDIF


  ; Read in the events that you want to filter. The CHU combinations
  ; will be interpolated onto the event times, which will be used to
  ; make the GTI below.
  evt = mrdfits(infile, 'EVENTS', hh)

 
                                ; Loop over CHU combinations to get the mask value:
  mask = 0
  for chunum=1,3 do BEGIN

                                ; Read in CHU information
    chu = mrdfits(chufile,chunum,hh)

    maxres = 20 ;; [arcsec] maximum solution residual
    if chunum EQ 4 then qind = 2 else qind = 1

    mask += (chu.valid EQ 1 AND $          ;; Valid solution from CHU
         chu.residual LT maxres AND $  ;; CHU solution has low residuals
         chu.starsfail LT chu.objects AND $ ;; Tracking enough objects
         chu.(qind)(3) NE 1)*chunum^2             ;; Not the "default" solution
    ;; mask = 1, chu1 only
    ;; mask = 4, chu2 only
    ;; mask = 9, chu3 only
    ;; mask = 5, chu12
    ;; mask = 10 chu13
    ;; mask = 13 chu23
    ;; mask = 14 chu123
  endfor
  evtmask = interpol(mask, chu.time, evt.time)


  
  options = [1,4,9,5,10,13,14] ; Possible CHU combinations
  names = ['chu1','chu2','chu3','chu12','chu13','chu23','chu123']

                                ; Generate output GTI files:
  for i=0, n_elements(options)-1 do BEGIN
     sel = where(evtmask eq options[i],nn) ; Confirm that there is time spent with this mask.
     if nn gt 0 then $
        nustar_write_chu_gti, mask, chu.time, options[i], evtstem+'_'+names[i], outdir
  ENDFOR


                                ; Generate graphical output
  IF keyword_set(show) THEN BEGIN
 
     IF ~keyword_set(ps) THEN !p.multi = [0,1,2]

     plot, chu.time, mask, psym = 3, xtitle = 'NuSTAR Epoch Seconds', $
                                ytitle = 'CHU Combination' ; Plot CHU combination vs time.

     color = ['red','green','blue','magenta','brown','pink','lightblue']
     
     image = hist_2d(evt.x,evt.y,min1=0,max1=800,min2=0,max2=800) 
;     cgimage, image, /keep, /scale
     contour, image/max(image),/nodata, yr=[200,700],xr=[200,700], /iso, charsize=1
;     cgcontour, image/max(image),/nodata, yr=[400,700],xr=[400,700], /keep


     for i=0, n_elements(options)-1 do begin
        sel = where(evtmask eq options[i],nn)
        if nn gt 0 then BEGIN
           image = hist_2d(evt[sel].x,evt[sel].y,min1=0,max1=800,min2=0,max2=800) 
           contour, 5*image/max(image), /overplot, color=cgcolor(color[i])
           xyouts, 0.8, 0.3-0.03*i,names[i], color=cgcolor(color[i]), charsize = 2, /normal
        endif
     endfor
     if keyword_set(ps) then cgPs_close
  ENDIF


  
END
  

