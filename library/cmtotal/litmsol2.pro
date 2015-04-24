;+
; NAME:
;   LITMSOL2
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Solve the light-time equation between two moving bodies
;
; MAJOR TOPICS:
;   Geometry, Physics, Dynamics
;
; CALLING SEQUENCE:
;   LITMSOL2, T1, X1, Y1, Z1, T2, $
;            FUNC2, INFO2, RAW2, FUNCTARGS=, FUNCTSAVE=, $
;            /RECEIVER, TBASE=, TOLERANCE=, POSUNITS=, MAXITER=, $
;            LIGHT_TIME=, TGUESS=, ERROR=, NITER=, $
;            VX1=, VY1=, VZ1=, $
;            X2=, Y2=, Z2=, VX2=, VY2=, VZ2=, $
;            METHOD=, $
;            DELAY_FUNCTION=, DELAY_ARG1=, DELAY_ARG2=, $
;            DELAY_FUNCTARGS=
;
; DESCRIPTION:
;
;  The procedure LITMSOL2 solves the light time equation between two
;  moving bodies, A and B, in the solar system.  Given the time and
;  position of reception or transmission of a photon at A, this
;  equation determines the time of transmission or reception at the
;  other solar system body B.  Since both bodies may be moving, the
;  equation must be solved iteratively.
;
;  The user must know the "A" endpoint of the ray, with time T1 and
;  position X1,Y1,Z1.  LITMSOL2 solves for the "B" endpoint time and
;  position T2 and X2,Y2,Z2 by propagating a light ray from one to the
;  other.
;
;  The position of the "B" body must be described as an interpolatable
;  function.  The user function FUNC2 must calculate the position (and
;  velocity) of the body at any applicable time T2, in the requested
;  units.
;
;  By default the body "A" is considered the transmitter and LITMSOL2
;  calculates the time at which body "B" receives the ray.   However,
;  if /RECEIVER is set, then body "A" is considered the receiver, and
;  LITMSOL2 calculates the time T2 in the past at which the ray must
;  have been transmitted by body "B" in order to be received by "A" at
;  time T1.
;
;  LITMSOL2 is able to estimate the T2 knowing only the time and
;  position at body "A".  However, convergence may be faster if the
;  TGUESS, METHOD and/or VX1,VY1,VZ1 keywords are used.  By default,
;  the initial guess for T2 is simply the same as T1.  A better
;  estimate can be passed in the TGUESS keyword.
;
;  If velocity information is available, then LITMSOL2 can use a
;  simple linear corrector method in order to speed convergence
;  (i.e. Newton's method).  The user should pass the velocity
;  at time T1 in the VX1,VY1,VZ1 keywords, and METHOD='CORRECTOR'.
; 
;  The user may also specify a "delay" function which estimates any
;  additional light propagation delays along the path based on the
;  current estimates of the two ray endpoints.  One such delay might
;  be the "Shapiro" delay due to general relativity.
;
;  Since the solution is iterative, the user may specify a solution
;  tolerance, and a maximum number of iterations.  An estimate of the
;  solution error is returned in the ERROR keyword.
;
; USER FUNCTIONS
;
;  The user must supply a function to interpolate the position of the
;  body at time T, which is passed in parameter FUNC2.  FUNC2, a
;  scalar string, is the name of subroutine to call which must compute
;  position of body at time T2.  The calling convention is the same as
;  JPLEPHINTERP, namely,
;
;     PRO FUNC2, INFO2, RAW2, T2, X2, Y2, Z2, VX2, VY2, VZ2, $
;       VELOCITY=, POSUNITS=, VELUNITS=, SAVE=, ...
;
;  The variables INFO2 and RAW2 are described below.  The variable T2
;  is the requested time (TDB), and the position and velocity must be
;  returned in X2,Y2,Z2, VX2,VY2,VZ2, with the requested units.  The
;  SAVE keyword can designate one keyword whose value will be returned
;  to the calling routine.  Any other keywords can be passed using the
;  _EXTRA calling convention using the FUNCTARGS keyword.
;
;  The user may also supply an optional function to compute an
;  additional delay.  The delay may be a function of the time and
;  position of both points "A" and "B".  For example, the "Shapiro
;  delay" of photons in the solar potential is one such kind of delay.
;  The calling convention is, 
;
;    DELAY = DELAY_FUNCTION(DELAY_ARG1, DELAY_ARG2, $
;               T1, X1, Y1, Z1, T2, X2, Y2, Z2, $
;               POSUNITS=, TBASE=, ...)
;
;  The returned delay must be in seconds, with the sense that a
;  positive value of DELAY indicates that the actual light travel time
;  is *longer* than the classical geometric travel time.
;
;     DELAY_ARG1, DELAY_ARG2 - can be any user-desired variables
;     T1 - same as T1 passed to LITMSOL2
;     X1,Y1,Z1 - same as passed to LITMSOL2
;     T2 - trial T2 interaction time in TDB Julian days
;     X2,Y2,Z2 - trial T2 interaction position, in POSUNITS
;     POSUNITS, TBASE - same as passed to LITMSOL2
;        ... additional keywords - passed via DELAY_FUNCTARGS
;            
; INPUTS:
;
;   T1 - epoch of interaction, in Julian days, in the TDB timescale.
;        (scalar or vector)
;
;   X1, Y1, Z1 - coordinates of interaction, referred to the solar
;                system barycenter, in J2000 coordinates.  Units are
;                described by POSUNITS. (scalar or vector)
;
;   FUNC2 - a scalar string, is the name of subroutine to call which
;           must compute position of body at time T2.
;
;   INFO2, RAW2 - arguments to the FUNC2 interpolation function.  At
;                 the very minimum, the INFO2 variable must be a
;                 structure of the form,
;                        INFO2 = {C: (speed of light in m/s), $
;                                 AU: (1 AU in light-seconds), $
;                                 ... other fields ... }
;                 The AU field is only required if POSUNITS EQ 'AU'.
;
; OUTPUTS:
;
;   T2 - upon output, epoch of interaction at the second solar system
;        body, in Julian days, in the TDB timescale.
;
; KEYWORD PARAMETERS:
;
;   DELAY_FUNCTION - user function to compute extra delay factors
;           based on the photon trajectory.  
;            
;   DELAY_ARG1,DELAY_ARG2 - arguments to the DELAY_FUNCTION.  These
;           variables are not touched by LITMSOL2, but merely passed
;           directly to DELAY_FUNCTION.
;
;   DELAY_FUNCTARGS - a single structure containing additional keyword
;           arguments passed to DELAY_FUNCTION using the _EXTRA method.
;
;   ERROR - upon return, a vector giving the estimated error in the
;           solution for each point, expressed in POSUNITS.  This
;           quantity should be less than TOLERANCE unless the number
;           of iterations exceeded MAXITER.
;
;   FUNCTARGS - a single structure containing additional keyword
;               arguments passed to FUNC2 using the _EXTRA method.
;
;   FUNCTSAVE - a named variable which will contain the results of
;               the SAVE keyword when calling FUNC2 upon return.
;
;   LIGHT_TIME - upon return, LIGHT_TIME is an array containing the
;                estimated light time for each requested time.
;
;   MAXITER - maximum number of solution iterations to be taken.
;             Default: 5
;
;   METHOD - solution method used, one of 'CONSTANT' or 'CORRECTOR'
;            The 'CONSTANT' method uses simple iteration.  The
;            'CORRECTOR' method uses a linear corrector to accelerate
;            convergence by accounting for the line of sight velocity,
;            but requires VX1, VY1, VZ1 to be passed.
;            Default: 'CONSTANT'
;
;   NITER - upon return, contains the actual number of iterations used.
;
;   POSUNITS - the units for positions, one of 'CM', 'KM', 'LT-S' or
;              'AU'.
;              Default: 'CM'
;
;   RECEIVER - if set, then the epoch T1 is a reception of a photon.
;              Otherwise T1 is the epoch of transmission of a photon.
;
;   TGUESS - a vector of the same size as T1, containing an initial
;            estimate of T2.
;            Default: LITMSOL2 uses its own estimate based on T1.
;
;   TOLERANCE - the solution tolerance, expressed in POSUNITS.
;               Default: 1000 CM
;
;   VX1, VY1, VZ1 - upon input, the body velocity at time T1, in
;                   VELUNITS units.  This information is required only
;                   if the CORRECTOR method is used.
;
;   VELUNITS - the units for velocities (and Shapiro derivative).
;              Default: POSUNITS+'/S'
;
;   X2, Y2, Z2, VX2, VY2, VZ2 - upon return, the body position and 
;            velocity at time T2, in units of POSUNITS and VELUNITS.
;
; EXAMPLE:
;
; SEE ALSO:
;
;   JPLEPHREAD, JPLEPHINTERP, SHAPDEL
;
;
; MODIFICATION HISTORY:
;   Major modifications, based on LITMSOL, 2009-01-05, CM
;   Documented, 2009-05-12, CM
;
;  $Id: litmsol2.pro,v 1.5 2010/05/04 21:01:52 craigm Exp $
;
;-
; Copyright (C) 2002, 2007, 2009, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

pro litmsol2, t1, x1, y1, z1, t2, func2, info2, raw2, $
              functargs=args2, functsave=save2, $
              tolerance=tol0, posunits=posunits0, $
              receiver=receiver, maxiter=maxiter0, light_time=ltm, $
              tguess=tguess, error=diff, niter=i, $
              vx1=vx1, vy1=vy1, vz1=vz1, $
              x2=x2, y2=y2, z2=z2, vx2=vx2, vy2=vy2, vz2=vz2, $
              method=method0, $
              delay_function=delfunc, delay_arg1=delinfo, delay_arg2=delraw, $
              delay_functargs=delargs

  ;; Default position and velocity units
  if n_elements(posunits0) EQ 0 then begin
      posunits = 'CM'
  endif else begin
      posunits = strtrim(posunits0(0),1)
  endelse
  velunits=posunits+'/S'

  npts = n_elements(x1)
  if (n_elements(y1) NE npts) OR (n_elements(z1) NE npts) then begin
     message, 'ERROR: number of points in X1, Y1, Z1 do not match'
  endif

  usevel = 0

  if n_elements(method0) EQ 0 then begin
     method = 'CONSTANT'
  endif else begin
     method = strupcase(strtrim(method0(0),2))
  endelse
  if method EQ 'CORRECTOR' then begin
     if (n_elements(vy1) NE npts) OR (n_elements(vz1) NE npts) then begin
        message, 'ERROR: when METHOD="CORRECTOR" the number of velocity and position points must match'
     endif
     usevel = 1
  endif

  ;; Default tolerances
  if n_elements(tol0) EQ 0 then begin
      tol = 1000d     ;; 10 m tolerance
      posunits = 'CM'
  endif else begin
      tol = tol0(0)
  endelse

  if n_elements(maxiter0) EQ 0 then maxiter = 5L $
  else                              maxiter = floor(maxiter0(0))>2
      
  case posunits of 
      'CM':   clight = info2.c*1d2  ;; CM/S
      'KM':   clight = info2.c*1d-3 ;; KM/S
      'LT-S': clight = 1d           ;; LT-S/S
      'AU':   clight = 1d/info2.au  ;; AU/S
  endcase

  ;; Use TGUESS if provided, otherwise estimate T2 as T1 to begin with
  dt0 = t1*0
  if n_elements(tguess) EQ n_elements(t1) then begin
      t2 = tguess 
  endif else begin
      t2 = t1
  endelse
  ltm = t2-t1


  ;; ==================== BEGIN ITERATION
  ct = 1L
  i = 0L
  while (ct GT 0) AND (i LT maxiter) do begin

      if arg_present(save2) OR n_elements(save2) GT 0 then begin
          call_procedure, func2, info2, raw2, t1+ltm, $
            x2, y2, z2, vx2, vy2, vz2, $
            velocity=usevel, posunits=posunits, velunits=velunits, $
            save=save2, _EXTRA=args2
      endif else begin
          call_procedure, func2, info2, raw2, t1+ltm, $
            x2, y2, z2, vx2, vy2, vz2, $
            velocity=usevel, posunits=posunits, velunits=velunits, $
            _EXTRA=args2
      endelse
      
      ;; Any systematic delays can be factored in as well (for example
      ;; the Shapiro delay).  Must be computed in seconds
      delay = 0
      if n_elements(delfunc) GT 0 then begin
         delay = call_function(delfunc, delinfo, delraw, $
                               t1, x1, y1, z1, t1+ltm, x2, y2, z2, $
                               posunits=posunits, tbase=tbase, $
                               _EXTRA=delargs)
         delay = delay/86400d ;; Convert to days
      endif

      ;; Compute distance from T1 to T2 in physical and light-second units

      x21 = (x2-x1) & y21 = (y2-y1) & z21 = (z2-z1)
      r21 = sqrt(x21^2 + y21^2 + z21^2)
      p21 = 0
      if method EQ 'CORRECTOR' then begin
         ;; Linear corrector factor
         p21 = ((vx2-vx1)*x21 + (vy2-vy1)*y21 + (vz2-vz1)*z21)/r21
      endif

      if keyword_set(receiver) then begin
          cfac  = -clight
          delay = -delay
      endif else begin
          cfac  = clight
      endelse

      ;; Linear corrector including line-of-sight velocity term
      dtcor = (-ltm + r21/cfac/86400d + delay) / (1 - p21/cfac)
      ltm = ltm + dtcor

      diff = abs(dtcor)*clight*86400d
      wh = where(diff GT tol, ct)

      ;; Prepare for next iteration
      i = i + 1
  endwhile

  t2 = t1 + ltm
  return
end
