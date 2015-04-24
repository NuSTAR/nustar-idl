;+
; NAME:
;   GAPNAN
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 661, Greenbelt, MD 20770
;   Craig.Markwardt@nasa.gov
;
; PURPOSE:
;   Insert NANs in time series gaps to facilitate plotting
;
; MAJOR TOPICS:
;   Time series
;
; CALLING SEQUENCE:
;   GAPNAN, TT, Y1, [Y2,] [Y3,] [Y4,] [Y5], MAXGAP=, GTI=
;
; DESCRIPTION:
;   This procedure is an covenience procedure for plotting time series
;   which may have gaps.  In other words, a time series where there
;   will be time segments of data, and periods of no data which are
;   considered "gaps."  Sometimes it is desireable to plot the data
;   with lines connecting the data, but no lines across gaps.
;
;   GAPNAN will insert NAN values in time series between gaps.
;   Because an IDL line plot will not connect points with NAN values
;   between them, inserting a NAN value is an effective way of
;   suppressing connecting lines between gaps.
;
;   The user can specify gaps in one of two ways, using either the
;   MAXGAP or the GTI keyword.  The user must specify one of these
;   keywords, but not both.  
;
;   The user can specify the maximum allowable gap size between time
;   series samples using the MAXGAP keyword.  If the time step between
;   samples is larger than MAXGAP then a gap is declared.  (This
;   functionality uses the Markwardt library routine GTISEG.)
;
;   The GTI keyword explicitly designates "good" time intervals.  The
;   user should pass a 2xn array using the GTI keyword, which indicate
;   the start/stop time of each good-time.   If the time samples cross
;   between good time intervals (or if a time sample is noth within a
;   good interval at all), then a gap is declared.  (This
;   functionality uses the Markwardt library routine GTIWHERE.)
;
;   The values Y1, Y2, etc. are the dependent variables.  Up to five
;   dependent variables can be adjusted in one call.
;
; INPUTS:
;   TIME - time variable, used to find gaps.  Upon return, TIME will
;          be modified in-place.  Whereever gaps occur, a new time
;          value will be inserted with the value of NAN.
;
; OPTIONAL INPUTS:
;   Y1, Y2, Y3, Y4, Y5 - the optional dependent variable.  Must have
;          the same number of elements as TIME.  Wherever NANs were
;          inserted in the TIME array, NANs will also be inserted at
;          the corresponding positions of Y1, Y2, etc.  Upon return,
;          these parameters will be modified in-place.  The user may
;          pass up to five dependent variables in one call.
; 
; INPUT KEYWORD PARAMETERS:
;   MAXGAP - maximum gap size between segments, in the same units as
;            the TIME variable.   The user must specify either MAXGAP
;            or GTI, but not both.
;
;   GTI - a 2xN array, in the same units as the TIME variable,
;         indicating "good" time intervals.  The user must specify
;         either MAXGAP or GTI, but not both.
;
; EXAMPLE:
;   ;; Sample data with gap between 3 and 10
;   tt = [1,2,3,    10, 11, 12d]
;   yy = [1,1,1,    2,  2,  2d ]
;   gapnan, tt, yy  maxgap=5

;   ;; Note that a NaN is inserted between 3 and 10, since the actual gap of
;   ;; 7 is larger than the maximum of 5.
;
;
;   ;; Sample data with gap between 3 and 10
;   tt = [1,2,3,    10, 11, 12d]
;   yy = [1,1,1,    2,  2,  2d ]
;
;   ;; Good times from 0.5-2.5 and 10.5-13.0
;   gti = [[0.5,2.5], [10.5,13]]
;   gapnan, tt, yy, gti=gti
;
;   ;; Note that a Nan is inserted between 2 and 3 because the good
;   ;; interval stops at 2.5; a Nan is inserted between 3 and 10, and
;   ;; 10 and 11 because neither 3 nor 10 are within a good interval.
;
;
; MODIFICATION HISTORY:
;   Written and documented, 2010-04-27 CM
;   Added MAXGAP and GTI keywords, 2010-11-13 CM
;   Square bracket array notation, 2011-12-21 CM
;   Bug fix for more than one input array (was ignored), 2012-02-20 CM
;   Bug fix when input GTI is set; defend against array bounds error;
;     add USAGE message, 2013-03-16 CM
;
;-
pro gapnan_i, ii, yy
  COMPILE_OPT strictarr
  ngti = n_elements(ii)/2
  ny = n_elements(yy)
  y1 = dblarr(ny + ngti - 1)
  y1[*] = !values.d_nan
  for i = 0, ngti-1 do begin
      i0 = ii[0,i] > 0 & i1 = ii[1,i] < (ny-1)
      if i0 LT ny AND i1 GE 0 AND i1 GE i0 then y1[i0+i:i1+i] = yy[i0:i1]
  endfor
  yy = y1
end

pro gapnan, tt, y1, y2, y3, y4, y5, maxgap=maxgap, gti=gti, include=include, $
            indices=indices
  ;; INDICES is index locations, gap occurs after location
  COMPILE_OPT strictarr

  nt = n_elements(tt)

  if n_params() EQ 0 then begin
     message, 'USAGE:', /info
     message, '   GAPNAN, TT, Y1, [Y2,] [Y3,] [Y4,] [Y5], MAXGAP=, GTI=', /info
     return
  endif

  if n_elements(maxgap) GT 0 then begin
      gti = gtiseg(tt, count=ct, indices=ii, maxgap=maxgap[0], mingti=0d)
  endif else if n_elements(indices) GT 0 then begin
      ngti = n_elements(indices)+1
      ii = lonarr(2,ngti)
      ii[0,0]        = 0
      ii[1,0:ngti-2] = indices
      ii[0,1:*]      = indices+1
      ii[1,ngti-1]   = nt - 1

      wh = where(ii[0,*] GE 0 AND ii[0,*] LE (nt-1) AND $
                 ii[1,*] GE 0 AND ii[1,*] LE (nt-1) AND $
                 ii[0,*] LE ii[1,*], ngti)
      if ngti LE 0 then return
      ii = ii[*,wh]
      
  endif else if n_elements(gti) NE 0 then begin

      ;; Make an array which indicates the GTI segment that each time
      ;; sample falls into, or -1 if it doesn't fall into an interval
      wh1 = gtiwhere(tt, gti, include=include, intervals=intv1, count=ct1)
      intv = lonarr(nt) - 1
      if ct1 GT 0 then intv[wh1] = intv1
      
      ;; Now look for positions where the GTI interval # changes, or 
      ;; transition to bad interval.
      wh = where((intv[1:*] NE intv) OR (intv EQ -1) OR (intv[1:*] EQ -1), ct)
      if ct EQ 0 then begin
          ii = [[0, nt-1]]
      endif else begin
          ii = lonarr(2,ct+1)
          ii[1,0:ct-1] = wh
          ii[0,1:ct]   = wh+1
          ii[1,ct]     = nt-1
      endelse
  endif else begin

      ;; Neither selector was requested, so return the data unchanged
      return
  endelse

  gapnan_i, ii, tt
  if n_params() GE 2 then gapnan_i, ii, y1
  if n_params() GE 3 then gapnan_i, ii, y2
  if n_params() GE 4 then gapnan_i, ii, y3
  if n_params() GE 5 then gapnan_i, ii, y4
  if n_params() GE 6 then gapnan_i, ii, y5

  return
end
