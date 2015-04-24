;+
; NAME:
;   UNITVECANG
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Convert unit vector to longitude and (co)latitude (RA/Dec)
;
; MAJOR TOPICS:
;   Geometry
;
; CALLING SEQUENCE:
;   LONLAT = UNITVECANG(U, [/DEC])
;
; DESCRIPTION:
;
;   The function UNITVECANG converts a unit vector into two polar
;   angles.
;
;   The input should be one or more unit 3-vector describing points on
;   the unit sphere.  The input is either a 3-vector, or a 3xN array
;   representing N unit vectors (i.e. DBLARR(3,N)).
;
;   The returned array, LONLAT, describes that point in spherical
;   polar coordinates.
;   
;   LONLAT(0,*) is the longitude angle, measured in degrees from +X,
;   with positive angles rotating through +Y.  The range of
;   LONLAT(0,*) is 0 (+X) through 90 (+Y) through 360.
;
;   If DEC=0, LONLAT(1,*) represents a colatitude angle, measured in
;   degrees from +Z.  The range of LONLAT(1,*) is 0 (+Z) through 
;   180 (-Z).
;
;   If DEC=1, LONLAT(1,*) represents a latitude angle ("declination"
;   in astronomy), measured in degrees from the XY equator (positive
;   toward +Z).  The range of LONLAT(1,*) is -90 (-Z) through +90 (+Z).
;
;   The input U may either be a single 3-vector (i.e. DBLARR(3)) or it
;   may be an array of N 3-vectors, (i.e. DBLARR(3,N)).  It is the
;   responsibility of the user for U to be a unit vector.
;
;   ANGUNITVEC and UNITVECANG are functional inverses.
;   ANGUNITVEC(UNITVECANG(U)) should produce the same unit vector(s).

; INPUTS:
;
;  U - input unit vector, either a 3-vector or a 3xN array for N unit
;      vectors.
;
;
; RETURNS:
;
;   The resulting LONLAT array as described above.
;
;
; KEYWORD PARAMETERS:
;
;   DEC - if set, then the returned LONLAT(1,*) component is latitude;
;         if not set then LONLAT(1,*) is a colatitude.
;
; EXAMPLE:
;
;   ; Sample unit vector
;   U = [-9.1103345E-01,3.7439942E-01,1.7275169E-01]
;   print, unitvecang(U)
;     ==> [157.65924,80.052155]
;
; SEE ALSO
;   UNITVECANG, ANGUNITVEC, CROSSP, QTNORMALIZE
;
; MODIFICATION HISTORY:
;   Written, 1999, CM
;   Documented, 2012-10-02, CM
;
;  $Id: unitvecang.pro,v 1.4 2012/10/02 12:28:13 cmarkwar Exp $
;
;-
; Copyright (C) 1999, 2012, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
function unitvecang, vecs, declination=declination

  n = n_elements(vecs)/3
  ang = make_array(2, n, value=vecs(0)*0)
  ang = reform(ang, 2, n, /overwrite)

  v0 = vecs(0,*) & v1 = vecs(1,*)
  ang(0,*) = (atan(v1,v0)*180d/!dpi + 360D) MOD 360D
  r = sqrt(v0*v0+v1*v1)
  ang(1,*) = atan(r, vecs(2,*))*180d/!dpi
  if keyword_set(declination) then ang(1,*) = 90D - ang(1,*)
  if n_elements(ang) EQ 3 then ang = ang(*) ;; Allow dimensions to relax here

  return, ang
end
