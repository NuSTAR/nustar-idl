;+
; NAME:
;   PXPERFECT
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Postscript device settings for "pixel perfect" matching to screen plot layout
;
; CALLING SEQUENCE:
;   PS_EXTRA = PXPERFECT([/LANDSCAPE], [/INCHES], [THICK_FACTOR=tf], [SCALE=s])
;
; DESCRIPTION:
;
;   PXPERFECT is designed to achieve nearly "pixel perfect" matching
;   of plot layout when rendering a plot on the IDL Postscript device.
;   The dimensions and character sizes of the current display device
;   are used to construct a group of settings which can be passed to
;   the Postscript device driver using the DEVICE procedure.
;
;   The key capability of PXPERFECT is to determine the size of fonts
;   to match the on-screen size.  Once this size is determined, IDL
;   will adjust other Postscript output dimensions such as plot
;   margins, font sizes, symbol sizes, etc, to match exactly the
;   on-screen layout.
;
;   The current direct graphics device must be a screen display
;   device, such as 'X' or 'WIN'.  The user wouuld first call
;   PXPERFECT() to determine what the appropriate settings for the
;   Postscript device would be.  At a later time, the user may switch
;   to the Postscript device to render the plot for output.
;
;   This is the approximate order of calling:
;     ;; Prerequisite: current graphics device is display device
;     SET_PLOT, 'X'  ;; or 'WIN'
;     TF = 1.0  ;; Thickness factor (see "Dealing with line thickness" below)
;
;     ;; User adjusts the plot layout to taste
;     PLOT, ... data ..., thick=1.0*TF
; 
;     ;; Capture layout settings and then initialize Postscript
;     PS_EXTRA = PXPERFECT(THICK_FACTOR=TF)
;     SET_PLOT, 'PS'       ;; NOTE: PXPERFECT() called *before* SET_PLOT
;     DEVICE, _EXTRA=PS_EXTRA
;
;     ;; User calls same plot command(s) with no changes to layout
;     PLOT, ... data ..., thick=1.0*TF
;
;     ;; Close output plot file
;     DEVICE, /CLOSE
;    
;   If the display window is resized, then PXPERFECT should be called
;   again to capture the new layout settings.
;
;   The value returned by PXPERFECT is an IDL structure, with fields
;   that are meant to be passed to the IDL Postscript driver using the
;   DEVICE, _EXTRA=(...) statement.
;
;   Output Page Size.  The dimensions of the Postscript output page
;   will be set so that the output page exactly matches the displayed
;   plot window.  The algorithm does assume that the user's display
;   density settings are correct, in particular, that !D.X_PX_CM is a
;   correct reflection of the number of screen pixels per centimeter.
;
;   The user can adjust the output page size in several ways.
;   PXPERFECT accepts the XSIZE, YSIZE and SCALE_FACTOR keywords and
;   interprets them in the same way that the standard procedure DEVICE
;   does.  In order to maintain the same layout, the user may specify
;   XSIZE or YSIZE, but not both.  If both XSIZE and YSIZE are
;   specified, then the aspect ratio of the output page will not match
;   the on-screen display window, and pixel-perfect layout matching
;   cannot be attained in that case.
;
;   Dealing with Line Thickness.  The Postscript device has a
;   different base line thickness compared to most on-screen display
;   devices.  The value returned in the THICK_FACTOR keyword is a
;   scale factor which should be multiplied by all thicknesses when
;   rendering to Postscript.
;
;   Thus, if the desired on-screen line width is 2.0 units, then the
;   Postscript line thickness will be 2.0*TF, where TF is the value
;   returned in the THICK_FACTOR keyword.
;
;   Passing Other Keywords to DEVICE.  PXPERFECT() accepts all the
;   keywords that the DEVICE procedure accepts.  Any keywords that do
;   not specifically affect PXPERFECT's operation are passed
;   along to the output structure, and hence to DEVICE.
;
;
; POSITIONAL PARAMETERS:
;
;   NONE
;
; KEYWORD PARAMETERS:
;
;   INCHES - set this keyword if Postscript dimensions are to be
;            specified in inches instead of centimeters.  This keyword
;            also specifies the units of the user-passed keywords
;            XSIZE, YSIZE, XOFFSET, YOFFSET.
;            Default: not set (i.e. centimeter units)
;
;   LANDSCAPE - set this keyword to indicate landscape orientation instead
;               of portrait orientation.
;               Default: not set (i.e. portrait orientation)
;
;   SCALE_FACTOR - a unitless scale factor which is used to scale the
;                  size of the Postscript page output.  By default the
;                  output page size in inches or centimeters is scaled
;                  to match the on-screen size.  Use this keyword to
;                  increase (>1.0) or decrease (<1.0) the size of the
;                  output page.
;                  Default: 1.0
;
;   THICK_FACTOR - upon output, THICK_FACTOR, which contain a factor
;                  which should be used to multiply all line width
;                  thicknesses.
;
;   XSIZE, YSIZE - user-requested output page size which may differ
;                  from default Postscript page size.  The user should
;                  specify either XSIZE or YSIZE, but not both;
;                  specifying both will cause the output page layout to not
;                  exactly match the on-screen graphic layout.  Also,
;                  XSIZE or YSIZE override the SCALE_FACTOR keyword.
;                  Default: not set (i.e. output page size will match
;                  on-screen size)
;
;  XOFFSET, YOFFSET - user-requested page offsets.
;                  Default: plot at origin (landscape plots are
;                  adjusted appropriately)
;
; RETURNS:
;
;   PXPERFECT returns a single IDL structure, which is meant to be
;   passed to the IDL Postscript device.  This structure is passed
;   using the DEVICE procedure and the _EXTRA mechanism.
;
; SIDE EFFECTS:
;
;   The graphics device must be set to a screen display device when
;   PXPERFECT is called.
;
;   Upon the first call to PXPERFECT, the graphics device is
;   momentarily switched to 'PS' in order to retrieve Postscript
;   device settings.
;
; EXAMPLE:
;   ;; Plot to screen display
;   PLOT, FINDGEN(10), charsize=1.5
;
;   ;; Initialize Postscript
;   PS = PXPERFECT()
;   SET_PLOT, 'PS'
;   DEVICE, _EXTRA=PS, FILENAME='outfile.ps'
;
;   ;; Same plot, to Postscript page
;   PLOT, FINDGEN(10), charsize=1.5
;
;   ;; Finish output
;   DEVICE, /CLOSE
;   
;
; SEE ALSO:
;
;   DEVICE, SET_PLOT
;
; MODIFICATION HISTORY:
;   Written, CM, 2010
;   Documented, CM, 2011-04-15
;   Square bracket array notation, CM, 2011-12-21
;   Logic fix for case when XSIZE & YSIZE given together, CM, 2012-09-27
;
;   $Id: pxperfect.pro,v 1.5 2012/09/27 23:18:54 cmarkwar Exp $
;
;-
; Copyright (C) 2010-2011,2012 Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-


function pxperfect_ps_px_cm
  COMPILE_OPT strictarr
  common pxperfect_ps_px_cm_common, ps_px_cm
  if n_elements(ps_px_cm) NE 0 then return, ps_px_cm

  old_d = !d.name
  set_plot, 'PS'
  ps_px_cm = !d.x_px_cm
  set_plot, old_d

  return, ps_px_cm
end

function pxperfect, landscape=landscape, scale_factor=scale0, $
                    thick_factor=tf, inches=inches, $
                    xsize=xsize0, ysize=ysize0, xoffset=xoffset0, yoffset=yoffset0, $
                    color=color0, bits_per_pixel=bpp0, $
                    _EXTRA=extra

  COMPILE_OPT strictarr
  if !d.name EQ 'PS' then $
     message, 'ERROR: you must run PXPERFECT before setting the PS driver'

  if n_elements(scale0) EQ 0 then scale = 1d $
  else scale = scale0[0]

  ;; Constants
  ;; The pixel densities of the Postscript and current display driver
  ;; in units of pixels per centimeter.
  disp_px_cm = !d.x_px_cm         ;; Expected px_cm for current display device
  ps_px_cm = pxperfect_ps_px_cm() ;; Expected px_cm for postscript device

  ;; Global constants
  cm_in = 2.54d  ;; [centimeters/inch]
  ;; Conversion to inches if necessary
  ;;                         [in/cm]     [cm/cm]
  in = keyword_set(inches) ? (1/cm_in) :    1

  ;; Thickness factor to be used with subsequent plots
  tf = 3 * scale

  ;; Determine the "scale" factor between screen pixels and IDL
  ;; postscript display pixels.  This will be used to scale XSIZE,
  ;; YSIZE and character size.
  sf = (disp_px_cm)/scale  ;; [disp pix/cm]

  ;; Postscript character size to match exactly the size of 'X'
  ;; characters.  We take the on-screen pixels, and convert to
  ;; Postscript pixel units.
  ;; Units are:       [disp pix                    * (ps pix / cm) / (disp pix / cm)
  ps_character_size = [!d.x_ch_size, !d.y_ch_size] * ps_px_cm      / sf ;; [ps pix]

  ;; Compute the size of the output postscript page, assuming that we
  ;; will match the on-screen size.
  ;; Units are: [(disp pix)/(disp pix/cm)]  (with optional conversion to inches)
  xsize = !d.x_size / sf * in  ;; [cm or in]
  ysize = !d.y_size / sf * in  ;; [cm or in]

  ;; If the user requested XSIZE, YSIZE or both, then rescale the
  ;; output page to match.  Of course we also need to scale the
  ;; character size as well.
  rescale_fact = 1
  if n_elements(xsize0) NE 0 AND n_elements(ysize0) NE 0 then begin  ;; XSIZE and YSIZE
     ;; Can't achieve original on-screen ratio, but we try to
     ;; make the character size come out as close as possible.
     rescale_fact = sqrt(xsize0[0]*ysize0[0]/xsize/ysize) 
     xsize = xsize0[0]
     ysize = ysize0[0]
     ps_character_size = ps_character_size * rescale_fact
  endif else if n_elements(xsize0) NE 0 then begin ;; XSIZE only
     rescale_fact = xsize0[0]/xsize
     xsize = xsize0[0]
     ysize = ysize * rescale_fact
     ps_character_size = ps_character_size * rescale_fact
  endif else if n_elements(xsize0) NE 0 then begin ;; YSIZE only
     rescale_fact = ysize0[0]/ysize
     xsize = xsize * rescale_fact
     ysize = ysize0[0]
     ps_character_size = ps_character_size * rescale_fact
  endif

  ;; User-requested XOFFSET or YOFFSET
  xoffset = 0
  if n_elements(xoffset0) NE 0 then xoffset = xoffset0[0]
  yoffset = (keyword_set(landscape) ? xsize : 0)  ;; Special case for landscape
  if n_elements(yoffset0) NE 0 then yoffset = yoffset0[0]

  ;; Default settings for color are: "yes, 8-bit color!" (but allow
  ;; user override)
  color = 1
  if n_elements(color0) NE 0 then color = color0[0]
  bpp = 8
  if n_elements(bpp0) NE 0 then bpp = bpp0[0]

  ;; Set line width equal to one screen pixel width
  ;; (and divide by standard PS line width of 10 postscript pixels)
  ;; NOTE: doesn't work as expected since DEVICE, OUTPUT=string 
  ;; always creates a new page.  Sigh.
  one_pix_width = 1d * ps_px_cm / sf * rescale_fact / 10d
  ps_thickness_cmd = $
     '/sys_setlinewidth /setlinewidth load def '+$
     '/setlinewidth { '+$
       string(one_pix_width,format='(D0)')+' mul /setlinewidth '+$
     '} bind def '

  ;; DEVICE structure which enables these capabilities
  ps = {xsize: xsize, xoff: xoffset, $
        ysize: ysize, yoff: yoffset, $
        set_character_size: ps_character_size, $
        inches: keyword_set(inches), color: color, bits_per_pixel: bpp, $
        landscape: keyword_set(landscape), $
        scale_factor: 1.0 $
       }
  ;; Combine with any other Postscript settings
  if n_elements(extra) NE 0 then ps = create_struct(ps, extra)
  
  return, ps

end
