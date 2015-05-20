; Purpose:
; Easy IDL wrapper for making images using nuproducts.
; syntax: 
;
; nustar_make_image, infile, <opts>
;
; Example:
; nustar_make_image, '80001085002/event_cl/nu80001085002A01_cl.evt.gz', erange=[3 20]

; Optional inputs (defaults):
; erange:
; Energy range for the lighcurve (3 to 79 keV) -> erange=[3, 79]

; Optional outputs (defaults):
; outdir
; Full path where you want the image to go. The default is wherever
; the input file is.
; outsuffix
; Optional tag to add to the end of the image filename. Example: outsuffix='_gtifiltered'


; History:

; 2015/05/14: Merged from previous version and brought the input
; keywrods in line with other nustar scripts.


PRO nustar_make_image, infile, $
                       erange = erange, outdir = outdir, $
                       usrgti=usrgti, outsuffix=outsuffix


  syntax='syntax: nustar_make_image, infile, <opts> '


  myname = 'nustar_make_image'

                                ; Check for HEASoft install
 heasoft = getenv('HEADAS')
 IF strcmp(heasoft, '') THEN BEGIN
    print, myname+': Init heasoft first.'
    return
 ENDIF
 

                                ; Check to see if infile exists:
 IF n_elements(infile) EQ 0 THEN BEGIN
    print, syntax
    return
 ENDIF

 IF ~file_test(infile) THEN BEGIN
    print, myname+': Input file: '+infile+' does not exist.'
    return
 endif

                                ; Default keywords
 IF ~keyword_set(usrgti) THEN usrgti = 'NONE'
 IF ~keyword_set(erange) THEN erange = [3, 79]
 IF ~keyword_set(outsuffix) THEN outsuffix = '' 


                                ; Parse input path and filename
 datpath = file_dirname(infile)
 IF ~keyword_set(outdir) THEN outdir = datpath+'/'





                                ; Test to see if your file is gzipps:
 IF stregex(infile, 'gz', /boolean) THEN begin
    evtstem = file_basename(infile, '.evt.gz')
 ENDIF ELSE BEGIN
    evtstem = file_basename(infile, '.evt')
 ENDELSE
                                ; Get the seqid from the filename:
 seqid = strmid(evtstem, 2, 11) 


                                ; Change energy range to PI channels:
 pi_range = (erange - 1.6) / 0.04 
 e_low_str = strtrim(string(erange[0], format = '(d8.2)'), 2)
 e_high_str = strtrim(string(erange[1], format = '(d8.2)'), 2)
 pi_low_str = strtrim(string(pi_range[0], format = '(i0)'), 2)
 pi_high_str = strtrim(string(pi_range[1], format = '(i0)'), 2)

                                ; See which FPM you're using:
 module = 'fail'
 IF stregex(infile, 'A', /boolean) THEN module = 'FPMA'
 IF stregex(infile, 'B', /boolean) THEN module = 'FPMB'
 IF strcmp(module, 'fail') THEN message, 'Cannot determine FPM from the name, please add A or B to filename.'

 
 cmd = "nuproducts"
 cmd += " indir="+datpath+" infile="+infile
 cmd += " instrument="+module
 cmd += ' bkgextract=no runmkarf=no runmkrmf=no clobber=yes'
 cmd += ' phafile=NONE lcfile=NONE plotdevice=ps'
 cmd += ' pilow='+pi_low_str+' pihigh='+pi_high_str
 cmd += ' usrgtifile='+usrgti
 cmd += ' outdir='+outdir

                                ; Load the FITS header to get the RA/DEC
 header = headfits(infile, exten='EVENTS')
 ra_obj = strtrim(string(fxpar(header, 'RA_OBJ')), 2)
 dec_obj = strtrim(string(fxpar(header, 'DEC_OBJ')), 2)

 cmd += ' steminputs=nu'+seqid
 cmd += ' srcra='+ra_obj+ ' srcdec='+dec_obj

                                ; Construct the output stem:
 stemout=evtstem+'_'+e_low_str+'to'+e_high_str+'keV'+outsuffix
 cmd += ' stemout='+stemout
 


 print, cmd
 spawn, cmd

end
