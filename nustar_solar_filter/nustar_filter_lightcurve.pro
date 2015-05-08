PRO nustar_filter_lightcurve, infile, $
                              rate = rate, show = show

; Generates a set of GTIs based on an input lightcurve and the
; supplied rate.

IF ~keyword_set(rate) THEN rate = 0.5 ; counts / second 

lc = mrdfits(infile, 'RATE', rate_header)
lc.time += fxpar(rate_header, 'TIMEZERO') ; Now in NuSTAR epoch time
dt = double(fxpar(rate_header, 'TIMEDEL'))

old_gtis = mrdfits(infile, 'GTI', gti_header)

; Okay, step 1: filter the lighturve

goodones = where(lc.rate LT rate, ngood)
IF ngood EQ 0 THEN message, 'nustar_filter_lightcurve: Something is wrong, filtering too strict.'


; Set up the "new" GTIs:
gti_stub = {start:0., stop:0.}



badones = where(lc.rate GE rate, nbad)
IF nbad EQ 0 THEN BEGIN
   print, 'Nothing to be done...'
ENDIF ELSE begin

   i = 0
   
   gti_stub.start = min(lc.time) - dt
   FOR  i =0, nbad - 1 DO begin
      
;   print, i
      gti_stub.stop = lc[badones[i]].time - dt
      push, rate_gtis, gti_stub
      
      
      IF i+1 LT nbad -1 THEN begin
         WHILE (badones[i+1] - badones[i] EQ 1) DO BEGIN
            
            i++
            IF i+1 Ge nbad -1 THEN BREAK
         ENDWHILE
      ENDIF
      gti_stub.start = lc[badones[i]+1].time + dt
      
   endfor
   gti_stub.stop = max(lc.time) + dt
   push, rate_gtis, gti_stub
   

; Write out temp GTIs:

   mwrfits, rate_gtis, 'rate_filter_gtis.fits', gti_header, /create
ENdelse

IF keyword_set(show) THEN begin 
   plot, lc.time, lc.rate, psym = 4, /xsty, xrange = [min(lc.time) - 100, max(lc.time)+100], yrange = [0, 1.1*rate], /yst

   IF nbad GT 0 THEN begin 
      oplot, lc[badones].time, lc[badones].rate, color = cgColor('Red'), psym =4
   ENDIF 

   oplot, !x.crange, rate * [1, 1], linestyle = 2, color = cgColor('Red')

   FOR i = 0, n_elements(rate_gtis) - 1 DO BEGIN
      oplot, rate_gtis[i].start * [1, 1], !y.crange, color = cgColor('Green')
      oplot, rate_gtis[i].stop * [1, 1], !y.crange, color = cgColor('Green'), linestyle = 2
   ENDFOR



ENDIF






END


