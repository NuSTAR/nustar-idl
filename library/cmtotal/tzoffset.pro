;+
; NAME:
;   TZOFFSET
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   Craig.Markwardt@nasa.gov
;
; PURPOSE:
;   Compute timezone offset from GMT for any date
;
; CALLING SEQUENCE:
;   DT = TZOFFSET(T, [/JULIAN,] [/LOCAL,] [IS_DST=is_dst])
;   DT = TZOFFSET(/NOW)
;
; DESCRIPTION: 
;
;  The function TZOFFSET computes the time zone offset between the
;  local time zone and GMT for any date.
;
;  The time zone offset is defined here as the number of seconds of
;  time West of the Greenwich Meridian.  Equivalently, it is the
;  number of seconds that must be *added* to local time in order to
;  transform it to GMT.
;
;  Here are some examples for different time zones,
;
;       Time zone     TZOFFSET()
;         UTC               0     ;; Britain
;         GMT               0
;         GMT-5        +18000     ;; United States
;         GMT+10       -36000     ;; Australia
;
;  The user may input the date, T, as either seconds elapsed since
;  1970-01-01T00:00:00, or in Julian days (if /JULIAN is set).  The
;  input time may be either expressed in the user's local time zone
;  (if /LOCAL is set) or in UTC.
;
;
; METHODS:
;
;  Since IDL does not provide a way to compute the time zone directly,
;  TZOFFSET uses indirect methods.
;  
;  Essentially, it parses the output of SYSTIME(1) and
;  SYSTIME(1,/UTC), and computes the time difference between the local
;  system and UTC.  There is a search algorithm that finds Summer-time
;  transitions.
;
;  For speed, TZOFFSET() pre-computes time zone offsets and saves them
;  for future use as a table lookup.  On a relatively modern computer
;  in 2009, a century's worth of timezone data can be pre-computed in
;  less than one second.  If the time range of interest is smaller,
;  then the pre-computations will occur more quickly than that.  Once
;  the table has been pre-computed, interpolation of the resulting
;  table is extremely fast.
;
;  The IS_DST output parameter is estimated using a heuristic.
;  Basically, if TZOFFSET() increases, that is considered to be a
;  summer-time transition, and if TZOFFSET() decreases, that is
;  considered a transition to standard time.
;
; CAVEATS:
;
;  The results of TZOFFSET are only as good as your operating system's
;  timezone information.  If your system's timezone tables are
;  incomplete or erroneous, then so will be TZOFFSET's output.
;
;  TZOFFSET computes the timezone offsets for your system's current
;  time-zone.  To compute the offset for another different time zone,
;  you will need to reset your system's notion of the timezone.  On
;  Unix and Mac OS X systems, this can be done by setting the "TZ"
;  environment variable with SETENV.
;
;  For 32-bit Unix systems, timezone tables apparently run out in
;  2038.
;
;  Pre-computed timezone tables document Summer-time transitions to
;  within one second.  Users should avoid calling TZOFFSET() with
;  times exactly on the transition boundaries.
;
;  The IS_DST heuristic may not be perfect.  It is better to rely on
;  the actual timezone offset than to assume that IS_DST means
;  something.
;
; PARAMETERS:
;
;   T - input times, either array or scalar.  The times may be
;       in Julian days (if /JULIAN is set) or in seconds from
;       1970-01-01T00:00:00.  The times should be expressed in
;       the UTC timezone, or the local time zone if /LOCAL is set.
;
;
; RETURNS:
;
;   The resulting timezone offsets.  The return value will
;   have the same number of elements as the input T parameter.
;   See CAVEATS above.
;
; KEYWORD PARAMETERS:
;
;   IS_DST - upon return, IS_DST is set to an array containing a
;            boolean flag for each input time.  If the flag equals 1,
;            the corresponding input time is probably during "summer
;            time."  A flag value of 0 indicates probable
;            standard time.  See CAVEATS above.
;
;   JULIAN - if set, then the input times must be in Julian days.
;            DEFAULT: not set, i.e. input times are in seconds from 1970.
;
;   LOCAL - if set, then the input times must be measured in the local
;           timezone.
;           DEFAULT: not set, i.e. input times are in UTC timezone.
;
;   NOW - if set, then compute the timezone offset at the current
;         moment.  The values of T, JULIAN and LOCAL are ignored.
;
; SEE ALSO:
;
;   SYSTIME
;
; MODIFICATION HISTORY:
;   Written, CM, 14 Sep 2009
;   Documentation typos, CM, 25 Sep 2012
;
;  $Id: tzoffset.pro,v 1.6 2012/09/27 23:17:28 cmarkwar Exp $
;
;-
; Copyright (C) 2009, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-


pro tzoffset_init, tlimits, tgrid, toff
  COMPILE_OPT strictarr
  tutc = systime(1, /julian, /utc)  ;; [day]
  tloc = systime(1, /julian)        ;; [day]

  ;; We round because since the two SYSTIME()'s are called a
  ;; few microseconds apart, they will not correspond to exactly the
  ;; same time, although it will be close.
  toff1 = round((tutc - tloc)*86400d)+0d ;; [sec]

  tlimits = [tutc, tutc]   ;; [day]
  tgrid   = tlimits        ;; [day]
  toff    = [toff1, toff1] ;; [sec]

  return
end

;; Convert IDL time-string to YMDhms array
function tzoffset_str2jd, str
  COMPILE_OPT strictarr
  n = n_elements(str)
  monstr = strupcase(strmid(str,4,3))
  mon = intarr(n)
  
  for i = 0L, n-1 do begin
     case monstr[i] of
        'JAN': mon[i] =  1
        'FEB': mon[i] =  2
        'MAR': mon[i] =  3
        'APR': mon[i] =  4
        'MAY': mon[i] =  5
        'JUN': mon[i] =  6
        'JUL': mon[i] =  7
        'AUG': mon[i] =  8
        'SEP': mon[i] =  9
        'OCT': mon[i] = 10
        'NOV': mon[i] = 11
        'DEC': mon[i] = 12
        else: message, 'ERROR: unrecognized month "'+monstr+'"'
     endcase
  endfor

  yr  = fix(strmid(str,20,4))
  day = fix(strmid(str,8,2))

  hr  = fix(strmid(str,11,2))
  mi  = fix(strmid(str,14,2))
  sec = fix(strmid(str,17,2))

  jd = julday(mon, day, yr, hr, mi, sec)
;  print, yr, mon, day, hr, mi, sec, jd, $
;         format='(%"%04d-%02d-%02dT%02d:%02d:%02d = JD%25.6f")'
  return, jd
end

function tzoffset_calc, t
  COMPILE_OPT strictarr
  jd0 = 2440587.5D

  t1 = (t-jd0) * 86400d     ;; [sec] from 1970
  ;; Vectorize for slight speed
  t_str = [ systime(0,t1,/utc), systime(0,t1) ]
  tt   = tzoffset_str2jd(t_str)
  tutc = tt[0]
  tloc = tt[1]

  tzoff = round((tutc - tloc)*86400d)+0d
;  print, 'TZOFF = ', tzoff
  return, tzoff
end

pro tzoffset_extendp, tstop, tlimits, tgrid, toff, tstep=tstep
  COMPILE_OPT strictarr

  n = n_elements(tgrid)
  while tstop GE tgrid[n-1] do begin
     tnext = tgrid[n-1] + tstep
     tzoff1 = tzoffset_calc(tnext)

     if tzoff1 NE toff[n-1] then begin
        ;; We found a new transition
        tgrid = [tgrid, tnext]
        toff  = [toff,  tzoff1]
        n = n + 1

        ;; Binary search for actual transition time
        t1 = tgrid[n-2]   & t2 = tgrid[n-1]
        toff1 = toff[n-2] & toff2 = toff[n-1]
        while t2 GT t1 + 1d/86400d do begin
           tnext = (t1 + t2)/2d
           tzoffx = tzoffset_calc(tnext)
           if tzoffx EQ toff1 then begin
              t1 = tnext
           endif else begin
              t2 = tnext
           endelse
        endwhile

        tgrid[n-1] = t2
     endif else begin

        if toff[n-2] EQ toff[n-1] then begin
           tgrid[n-1] = tnext
        endif else begin
           tgrid = [tgrid, tnext]
           toff  = [toff, tzoff1]
           n = n + 1
        endelse
     endelse
  endwhile

  tlimits[1] = tgrid[n-1]
end

pro tzoffset_extendm, tstop, tlimits, tgrid, toff, tstep=tstep
  COMPILE_OPT strictarr

  n = n_elements(tgrid)
  while tstop LE tgrid[0] do begin
     tnext = tgrid[0] - tstep
     tzoff1 = tzoffset_calc(tnext)

     if tzoff1 NE toff[0] then begin
        ;; We found a new transition
        tgrid = [tnext, tgrid]
        toff  = [tzoff1, toff ]
        n = n + 1

        ;; Binary search for actual transition time
        t1 = tgrid[0]   & t2 = tgrid[1]
        toff1 = toff[0] & toff2 = toff[1]
        while t2 GT t1 + 1d/86400d do begin
           tnext = (t1 + t2)/2d
           tzoffx = tzoffset_calc(tnext)
           if tzoffx EQ toff1 then begin
              t1 = tnext
           endif else begin
              t2 = tnext
           endelse
        endwhile

        tgrid[1] = t2
     endif else begin

;        print, 'A', toff
;        print, toff[0] - toff[1]
        if toff[0] EQ toff[1] then begin
           tgrid[0] = tnext
        endif else begin
           tgrid = [tnext, tgrid]
           toff  = [tzoff1, toff]
           n = n + 1
        endelse
;        print, 'B', toff
     endelse

;print, '####'
;caldat, tgrid, mon, day, yr, hr, mi, sec & for i = 0, n_elements(tgrid)-1 do begin & print, yr(i), mon(i), day(i), hr(i), mi(i), sec(i), tgrid(i), toff(i), format='(%"%04d-%02d-%02dT%02d:%02d:%02d = JD%25.6f   --> %f")' & end
;print, '===='

  endwhile

  tlimits[0] = tgrid[0]
end

pro tzoffset_dst, tgrid, toff, dst
  COMPILE_OPT strictarr
  if n_elements(toff) EQ 0 then begin
     dst = [0]
     return
  endif

  dst = intarr(n_elements(toff)) - 1
  iprev = 0L
  for i = 1, n_elements(toff)-1 do begin
     if toff[i] EQ toff[i-1] then begin
        dst[i] = dst[iprev]
     endif else if toff[i] GT toff[i-1] then begin
        dst[i] = 0
        iprev = i
     endif else if toff[i] LT toff[i-1] then begin
        dst[i] = 1
        iprev = i
     endif
  endfor
  
  wh = where(dst LT 0, ct)
  if ct EQ 0 then return
  if ct EQ n_elements(dst) then begin
     dst[*] = 0
     return
  endif
  dst[wh] = 1-dst[max(wh)+1]

  return
end

function tzoffset, tt, julian=julian, now=now, local=local, is_dst=dst, $
                   reset=reset

  COMPILE_OPT strictarr
  common tzoffset, tlimits, tgrid, toff, is_dst

  if keyword_set(reset) then begin
     if n_elements(tlimits) GT 0 then begin
        dummy = temporary(tlimits)
        dummy = temporary(tgrid)
        dummy = temporary(toff)
     endif
     return, !values.d_nan
 endif

 if n_params() EQ 0 AND NOT keyword_set(now) then begin
     message, 'USAGE: OFF = TZOFFSET(T, [/JULIAN,] [/LOCAL])', /INFO
     message, '       OFF = TZOFFSET(/NOW)',                   /INFO
     return, !values.d_nan
 endif

  tstep = 30d  ;; [day] - timezone changes occur less frequently than TSTEP

  ;; If NOW is set, then retrieve the current Julian date in UTC time zone.
  if keyword_set(now) then begin
     tt = systime(1, /julian)
     julian = 1 & local = 0
  endif

  if keyword_set(julian) then begin
     t1 = tt
  endif else begin
     t1 = 2440587.5D + tt/86400d
  endelse

  mint = min(tt, max=maxt)

  if n_elements(tlimits) EQ 0 then begin
     ;; Initialize the look-up table...
     tzoffset_init, tlimits, tgrid, toff
     ;; ... with at least a year on either side
     tzoffset_extendp, tlimits[1]+365d, tlimits, tgrid, toff, tstep=tstep
     tzoffset_extendm, tlimits[0]-365d, tlimits, tgrid, toff, tstep=tstep
     ;; ... and estimate summer time flag
     tzoffset_dst, tgrid, toff, is_dst
  endif

  ;; Extend the range of the look-up table if the input range
  ;; exceeds the table range
  extended = 0
  if mint LE tlimits[0] then begin
     tzoffset_extendm, mint, tlimits, tgrid, toff, tstep=tstep
     extended = 1
  endif
  if maxt GE tlimits[1] then begin
     tzoffset_extendp, maxt, tlimits, tgrid, toff, tstep=tstep
     extended = 1
  endif
  if extended OR n_elements(is_dst) then begin
     tzoffset_dst, tgrid, toff, is_dst
  endif

  ii = value_locate(tgrid, t1)

  tzoff = toff[ii]
  dst   = is_dst[ii]
  
  if keyword_set(local) then $
     return, tzoffset(t1+tzoff/86400d, /julian, is_dst=dst)

  return, tzoff
end
