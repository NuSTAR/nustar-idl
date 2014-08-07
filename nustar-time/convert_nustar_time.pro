FUNCTION convert_nustar_time, time, $
                              from_ut = from_ut, $
                              from_mjd = from_mjd , $
                              from_nustar =from_nustar, $
                              from_aft=from_aft, $
                              nustar=nustar, $
                              mjd = mjd, $
                              fits=fits, $
                              ut = ut, $
                              aft = aft

; Last Updated: 2012-08-17
; Added support for time formats from the AFT, e.g.:
;               2012:220:00:59:59
; Brian Grefenstette
; Note: uses cmsystime (Craig Markwardt) and date_conv (astrolib) extensively

; Routine for converting times to/from NuSTAR epoch times.
; Inputs: time
; Output: time in new standard
; Input params:
;  from_ut, from_mjd, from_aft, or from_nustar (default)
; Output params:
;  nustar, mjd, fits, aft, utc extended (default)


mjdref = 55197. ; reference time for NuSTAR. NuSTAR time is seconds
               ; since this epoch

; Get offset for leap seconds:
set = 0
ntime = n_elements(time) 
IF keyword_set(from_ut) THEN BEGIN
   ; Converting from UTC string to NuSTAR epoch time:
                                ; Format for time string should be;
;               DD-MON-YEAR HH:MM:SS.SS
;               (eg.  14-JUL-2005 15:25:44.23)
;            OR
;               YYYY-MM-DD HH:MM:SS.SS  (ISO standard)

   ; Convert this to MJD:
   mjd_out_time = dblarr(ntime)
   FOR i = 0, ntime - 1 DO begin
      mjd_out_time[i] = date_conv(time[i], 'M')
   ENDFOR

   set = 1
ENDIF
IF keyword_set(from_mjd) THEN BEGIN
   mjd_out_time = time
   IF set EQ 1 THEN message, 'Only give me one output!'
   set = 1
ENDIF
IF keyword_set(from_aft) THEN BEGIN
   ; Converting from AFT string to NuSTAR epoch time:
                                ; Format for time string should be;
;                YEAR:DOY:HR:MN:SS.SS
;                2012:220:00:59:59
   mjd_out_time = dblarr(ntime)

   FOR i = 0, ntime - 1 DO BEGIN
      temp = float(strsplit(time[i], ':', /extract))
      mjd_out_time[i] = date_conv(temp, 'M')
   ENDFOR
   IF set EQ 1 THEN message, 'Only give me one output!'
   set = 1
ENDIF

IF ~set THEN BEGIN
   ; Convert NuSTAR time to MJD:
   mjd_out_time = (time / 86400.) + mjdref
ENDIF

; Now format output:
set = 0
IF keyword_set(mjd) THEN BEGIN
   output = mjd_out_time
   set = 1
ENDIF 
IF keyword_set(fits) THEN BEGIN
   jd_out_time = cmsystime(mjd_out_time, /from_mjd, /julian)
   output = strarr(ntime)
   FOR i = 0, ntime - 1 DO begin
      output[i] =  date_conv(jd_out_time[i], 'F')
   ENDFOR
   set = 1
ENDIF
IF keyword_set(ut) THEN BEGIN
   jd_out_time = cmsystime(mjd_out_time, /from_mjd, /julian)
   output = strarr(ntime)
;   print, ntime
   FOR i = 0, ntime - 1 DO begin
      tmp =  date_conv(jd_out_time[i], 'F')

      tmp = strsplit(tmp, 'T', /extract)
;      print, tmp
      output[i] = tmp[0] + ' '+tmp[1]
   ENDFOR
   set = 1
ENDIF
IF keyword_set(aft) THEN BEGIN
   jd_out_time = cmsystime(mjd_out_time, /from_mjd, /julian)
      output = strarr(ntime)
;   print, ntime
   FOR i = 0, ntime - 1 DO begin
      tmp =  date_conv(jd_out_time[i], 'V')
      output[i] = string(tmp[0], format = '(i0)')+':'+$
            string(tmp[1], format = '(i0)')+':'+$
            string(tmp[2], format = '(i0)')+':'+$
            string(tmp[3], format = '(i0)')+':'+$
            string(tmp[4], format = '(i0)')
   ENDFOR
   set = 1
ENDIF



IF ~set THEN BEGIN
   output = (mjd_out_time - mjdref) * 86400.

ENDIF


return, output


END

