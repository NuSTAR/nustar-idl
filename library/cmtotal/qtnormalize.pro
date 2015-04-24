;+
; NAME:
;   QTNORMALIZE
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Normalize a quaternion (unit quaternion and/or sign conventions)
;
; MAJOR TOPICS:
;   Geometry
;
; CALLING SEQUENCE:
;   QNEW = QTNORMALIZE(QOLD, UNITIZE=1, POS3=1)
;
; DESCRIPTION:
;
;   The function QTNORMALIZE performs normalization operations upon a
;   quaternion.  The two operations are: UNITIZE and POS3.
;
;   The UNITIZE operation occurs if /UNITIZE is set (this is the
;   default).  If set, then the returned quaternion is a unit
;   quaternion.  Non-unit input quaternions will be adjusted so that
;   the components have the same ratios, but unit magnitude.
;
;   The POS3 operation occurs if /POS3 is set (this is the default).
;   If set, then the returned quaternion is ensured to be positive in
;   its third component.  In other words, QNEW[3] GE 0.  Since the
;   same quaternion can have two different representations, differing
;   only in the signs of the components, the POS3 operation ensures
;   that one sign convention is used for all quaternions.  This is
;   useful for comparing quaternion component values.
;
;   By default /UNITIZE and /POS3 are both set.  In order to disable
;   them, set UNITIZE=0 or POS3=0 explicitly.
;
; INPUTS:
;
;  Q - array of one or more unit quaternions.  For a single
;      quaternion, Q should be a 4-vector.  For N quaternions, Q
;      should be a 4xN array.
;
;
; RETURNS:
;
;   The resulting normalized quaternions.  For single inputs, returns
;   a 4-vector.  For N inputs, returns N quaternions as a 4xN array.
;
;
; KEYWORD PARAMETERS:
;
;   UNITIZE - if set, then perform UNITIZE operation as described
;             above.  By default, UNITIZE=1.
;
;   POS3 - if set, then perform POS3 operation as described above.  By
;          default, POS3=1.
;
;
; EXAMPLE:
;
;
; SEE ALSO
;   QTANG, QTAXIS, QTCOMPOSE, QTERP, QTEXP, QTFIND, QTINV, QTLOG,
;   QTMAT, QTMULT, QTPOW, QTVROT
;
; MODIFICATION HISTORY:
;   Written, 2012-08-01, CM
;
;  $Id: qtnormalize.pro,v 1.2 2012/10/02 12:29:33 cmarkwar Exp $
;
;-
; Copyright (C) 2012, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

function qtnormalize, q0, unitize=unitize0, pos3=pos30

  if n_elements(unitize0) EQ 0 then unitize = 1 $
  else unitize = keyword_set(unitize0)
  if n_elements(pos30) EQ 0 then pos3 = 1 $
  else pos3 = keyword_set(pos30)

  q = q0

  ;; Standardize so that the final element is always non-negative
  if keyword_set(pos3) then begin
      wh = where(q(3,*) LT 0, ct)
      if ct GT 0 then q(*,wh) = -q(*,wh)
  endif

  if NOT keyword_set(unitize) then return, q

  qnorm = sqrt(total(q^2,1))
  
  nu = n_elements(qnorm)
  if nu EQ 1 then return, q/qnorm(0)
  
  return, q / rebin(reform(qnorm,1,nu),4,nu)
end
