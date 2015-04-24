;+
; NAME:
;   UNITIZE
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Construct a unit vector from a vector
;
; MAJOR TOPICS:
;   Geometry
;
; CALLING SEQUENCE:
;   U = UNITIZE(V)
;
; DESCRIPTION:
;
;   The function UNITIZE accepts any vector as input, and returns a
;   unit vector.  The returned vector has the same direction as V but
;   a unit magnitude. (using L2 norm)
;
;   Mostly commonly UNITIZE will be used on 3-vectors.  The input V
;   may either be a single 3-vector (i.e. DBLARR(3)) or it may be an
;   array of N 3-vectors, (i.e. DBLARR(3,N)).  The returned array will
;   have the same structure as V.
;
;   UNITIZE also works on vectors with arbitrary numbers of
;   components.  All that is required is that the vector components be
;   the first dimension of the input array V.
;
; INPUTS:
;
;  V - input array, commonly a 3-vector for a single vector, or a 3xN
;      array for N vectors.  It is also possible to pass any array
;      DBLARR(M,n1,n2,n3,...).
;
;
; RETURNS:
;
;   The resulting unitized vector or vectors.  The output has the same
;   dimension as the input, V.
;
;
; KEYWORD PARAMETERS:
;
;   NONE
;
; EXAMPLE:
;
;   print, unitize([3d,4d,0d])
;     ==> [0.6, 0.8, 0.0]
;
; SEE ALSO
;   UNITVECANG, ANGUNITVEC, CROSSP, QTNORMALIZE
;
; MODIFICATION HISTORY:
;   Written, 1999, CM
;   Converted to more general dimension, 2012-10-02, CM
;   Documented, 2012-10-02, CM
;
;  $Id: unitize.pro,v 1.5 2012/10/02 12:28:47 cmarkwar Exp $
;
;-
; Copyright (C) 1999, 2012, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
function unitize, u, magnitude=uu
  uu = sqrt(total(u^2,1))
  nu = n_elements(uu)
  if nu EQ 1 then return, u/uu(0)

  nv = n_elements(u)/nu
  uu3 = u
  for i = 0, nv-1 do uu3(i,*) = uu
  return, u / uu3
end
