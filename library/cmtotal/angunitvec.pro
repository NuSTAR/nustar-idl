;+
; NAME:
;   ANGUNITVEC
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Convert longitude and (co)latitude (RA/Dec) to unit vector
;
; MAJOR TOPICS:
;   Geometry
;
; CALLING SEQUENCE:
;   U = ANGUNITVEC(LON, LAT, [/DEC])
;
; DESCRIPTION:
;
;   The function ANGUNITVEC converts spherical polar angles into a
;   unit vector.
;
;   The inputs, LON and LAT, describes a point in spherical polar
;   coordinates on the unit sphere.  The output is that point as a
;   unit 3-vector.
;
;   LON is the longitude angle, measured in degrees from +X, with
;   positive angles rotating through +Y.  The range of LON is
;   0 (+X) through 90 (+Y) through 360.
;
;   If DEC=0, LAT represents a colatitude angle, measured in
;   degrees from +Z.  The range of LAT is 0 (+Z) through 
;   180 (-Z).
;
;   If DEC=1, LAT represents a latitude angle ("declination"
;   in astronomy), measured in degrees from the XY equator (positive
;   toward +Z).  The range of LAT is -90 (-Z) through +90 (+Z).
;
;   ANGUNITVEC and UNITVECANG are functional inverses.
;   UNITVECANG(ANGUNITVEC(LON,LAT)) should produce the same LONLAT
;   pairs.
;
; INPUTS:
;
;  LON - input longitude values, scalar or vector.  See above.
;  LAT - input (co)latitude, scalar or vector. See above.
;
; RETURNS:
;
;  The resulting unit vector, either a 3-vector or a 3xN array for N
;  unit vectors.
;
;
; KEYWORD PARAMETERS:
;
;   DEC - if set, then the input LONLAT(1,*) component is latitude;
;         if not set then LONLAT(1,*) is a colatitude.
;
; EXAMPLE:
;
;   print, angunitvec([157.65924,80.052155])
;     ==> [-9.1103345E-01,3.7439942E-01,1.7275169E-01]
;     (compare to example in UNITVECANG)
;
; SEE ALSO
;   UNITVECANG, ANGUNITVEC, CROSSP, QTNORMALIZE
;
; MODIFICATION HISTORY:
;   Written, 1999, CM
;   Documented, 2012-10-02, CM
;
;  $Id: angunitvec.pro,v 1.2 2012/10/02 12:28:13 cmarkwar Exp $
;
;-
; Copyright (C) 1999, 2012, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
function angunitvec, a0, d0, declination=declination

  dtor = !dpi/180D
  n = min([n_elements(a0), n_elements(d0)])
  unit = dblarr(3,n)
  unit = reform(unit, 3, n, /overwrite)
  if keyword_set(declination) then $
    d = double(90D - d0)*dtor else d = double(d0)*dtor
  a = a0*dtor

  unit(0,*) = cos(a)*sin(d)
  unit(1,*) = sin(a)*sin(d)
  unit(2,*) = cos(d)
  if n_elements(unit) EQ 3 then unit = unit(*) ;; Allow dimensions to relax

  return, unit
end

