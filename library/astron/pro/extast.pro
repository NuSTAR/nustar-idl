pro extast,hdr,astr,noparams, alt=alt
;+
; NAME:
;     EXTAST
; PURPOSE:
;     Extract ASTrometry parameters from a FITS image header.
; EXPLANATION:
;     Extract World Coordinate System information 
;     ( http://fits.gsfc.nasa.gov/fits_wcs.html ) from a FITS header and 
;     place it into an IDL structure.
;
; CALLING SEQUENCE:
;     EXTAST, hdr,  astr, [ noparams, ALT= ]   
;
; INPUT:
;     HDR - variable containing the FITS header (string array)
;
; OUTPUTS:
;     In the following, index 1 & 2 refer to the first and second astrometry
;     axes respectively. The actual axis numbers are stored in .AXIS
;
;     ASTR - Anonymous structure containing astrometry info from the FITS 
;             header ASTR always contains the following tags (even though 
;             some projections do not require all the parameters)
;      .NAXIS - 2 element array giving image size
;      .CD   -  2 x 2 array containing the astrometry parameters CD1_1 CD1_2
;               in DEGREES/PIXEL                                 CD2_1 CD2_2
;      .CDELT - 2 element double vector giving physical increment at the 
;                 reference pixel
;      .CRPIX - 2 element double vector giving X and Y coordinates of reference 
;               pixel (def = NAXIS/2) in FITS convention (first pixel is 1,1)
;      .CRVAL - 2 element double precision vector giving R.A. and DEC of 
;               reference pixel in DEGREES
;      .CTYPE - 2 element string vector giving projection types, default
;               ['RA---TAN','DEC--TAN']
;      .LONGPOLE - scalar giving native longitude of the celestial pole 
;             (default = 180 for zenithal projections) 
;      .LATPOLE - scalar giving native latitude of the celestial pole default=0)
;      .PV2 - Vector of projection parameters associated with latitude axis
;             PV2 will have up to 21 elements for the ZPN projection, up to 3 
;             for the SIN projection and no more than 2 for any other 
;             projection  
;
;     Fields added for version 2:
;      .PV1 - Vector of projection parameters associated with longitude axis
;      .AXES  - 2 element integer vector giving the FITS-convention axis 
;               numbers associated with astrometry, in ascending order. 
;               Default [1,2].
;      .REVERSE - byte, true if first astrometry axis is Dec/latitude
;      .COORD_SYS - 1 or 2 character code giving coordinate system, including
;                 'C' = RA/Dec, 'G' = Galactic, 'E' = Ecliptic, 'X' = unknown.
;      .PROJECTION  - 3-letter WCS projection code
;      .KNOWN    - true if IDL WCS routines recognise this projection
;      .RADECSYS - String giving RA/Dec system e.g. 'FK4', 'ICRS' etc.
;      .EQUINOX  - Double giving the epoch of the mean equator and equinox
;      .DATEOBS  - Text string giving (start) date/time of observations
;      .MJDOBS   - Modified julian date of start of observations.
;      .X0Y0     - Implied offset in intermediate world coordinates (x,y)
;                  if a non-standard fiducial point is set via PV1 and also
;                  PV1_0a =/ 0, indicating that an offset should be
;                  applied to place CRVAL at the (x,y) origin.
;                  Should be *added* to the IWC derived from application of
;                  CRPIX, CDELT, CD to the pixel coordinates.
;
;      .DISTORT - optional substructure specifying any distortion parameters
;                 currently implemented only for "SIP" (Spitzer Imaging 
;                 Polynomial) distortion parameters
;
;       NOPARAMS -  Scalar indicating the results of EXTAST
;            -1 = Failure - Header missing astrometry parameters
;             1 = Success - Header contains CROTA + CDELT (AIPS-type) astrometry
;             2 = Success - Header contains CDn_m astrometry, rec.    
;             3 = Success - Header contains PCn_m + CDELT astrometry. 
;             4 = Success - Header contains ST  Guide Star Survey astrometry
;                           (see gsssextast.pro )
; OPTIONAL INPUT/OUTPUT KEYWORDS:
;       ALT -  single character 'A' through 'Z' or ' ' specifying an alternate 
;              astrometry system present in the FITS header.    The default is
;              to use the primary astrometry or ALT = ' '.   If /ALT is set, 
;              then this is equivalent to ALT = 'A'.   See Section 3.3 of 
;              Greisen & Calabretta (2002, A&A, 395, 1061) for information about
;              alternate astrometry keywords.    If not set on input, then
;              ALT is set to ' ' on output.
; PROCEDURE:
;       EXTAST checks for astrometry parameters in the following order:
;
;       (1) the CD matrix PC1_1,PC1_2...plus CDELT*, CRPIX and CRVAL
;       (2) the CD matrix CD1_1,CD1_2... plus CRPIX and CRVAL.   
;       (3) CROTA2 (or CROTA1) and CDELT plus CRPIX and CRVAL.
;
;       All three forms are valid FITS according to the paper "Representations 
;       of World Coordinates in FITS by Greisen and Calabretta (2002, A&A, 395,
;       1061 http://fits.gsfc.nasa.gov/fits_wcs.html ) although form (1) is 
;       preferred.
;
; NOTES:
;       1.  An anonymous structure is created to avoid structure definition
;       conflicts.    This is needed because some projection systems
;       require additional dimensions (i.e. spherical cube
;       projections require a specification of the cube face).
;
;       2,   Some FITS headers (e.g.from HST/ACS) include SIP forward distortion
;       coefficients but do not include the reverse coefficients.   As of 
;       December 2013, AD2XY uses the BROYDEN() function to solve for the 
;       reverse distortion in this case.      
; PROCEDURES CALLED:
;      GSSSEXTAST, ZPARCHECK
; REVISION HISTORY
;      Written by B. Boothman 4/15/86
;      Accept CD001001 keywords               1-3-88
;      Accept CD1_1, CD2_1... keywords    W. Landsman    Nov. 92
;      Recognize GSSS FITS header         W. Landsman    June 94
;      Get correct sign, when converting CDELT* to CD matrix for right-handed
;      coordinate system                  W. Landsman   November 1998
;      Consistent conversion between CROTA and CD matrix  October 2000
;      CTYPE = 'PIXEL' means no astrometry params  W. Landsman January 2001
;      Don't choke if only 1 CTYPE value given W. Landsman  August 2001
;      Recognize PC00n00m keywords again (sigh...)  W. Landsman December 2001
;      Recognize GSSS in ctype also       D. Finkbeiner Jan 2002
;      Introduce ALT keyword              W. Landsman June 2003
;      Fix error introduced June 2003 where free-format values would be
;      truncated if more than 20 characters.  W. Landsman Aug 2003
;      Further fix to free-format values -- slash need not be present Sep 2003
;      Default value of LATPOLE is 90.0  W. Landsman February 2004
;      Allow for distortion substructure, currently implemented only for
;          SIP (Spitzer Imaging Polynomial)   W. Landsman February 2004 
;      Correct LONGPOLE computation if CTYPE = ['*DEC','*RA'] W. L. Feb. 2004
;      Assume since V5.3 (vector STRMID)  W. Landsman Feb 2004
;      Yet another fix to free-format values   W. Landsman April 2004
;      Introduce PV2 tag to replace PROJP1, PROJP2.. etc.  W. Landsman May 2004
;      Convert NCP projection to generalized SIN   W. Landsman Aug 2004
;      Add NAXIS tag to output structure  W. Landsman Jan 2007
;      .CRPIX tag now Double instead of Float   W. Landsman  Apr 2007
;      If duplicate keywords use the *last* value W. Landsman Aug 2008
;      Fix typo for AZP projection, nonzero longpole N. Cunningham Feb 2009
;      Give warning if reverse SIP coefficient not present  W. Landsman Nov 2011
;      Allow obsolete CD matrix representations W. Landsman May 2012
;      Work for Paritel headers with extra quotes R. Gutermuth/WL  April 2013
;
;      Version 2:  J. P. Leahy, July 2013
;        - Support long & lat axes not being the first 2.
;        - Implemented PV1 and hence non-default phi0 and theta0
;        - Added AXES, REVERSE, COORD_SYS, PROJECTION, RADECSYS, EQUINOX,
;          DATEOBS, MJDOBS, PV1, and X0Y0 tags to the structure.
;        - More checks for inconsistencies in the keywords.
;      v2.1 21/7/13 Missing mjdobs & equinox changed to NaN (was -1 & 0);
;          Converts GLS to SFL if possible; added KNOWN tag.
;      v2.2 21/9/13 GLS conversion fixed.
;      v2.3 Remove warning if reverse SIP coefficients not found, since AD2XY
;          now iterates on the forward coefficients.
;-
 On_error, 0
 compile_opt idl2
 ;
 ; List of known map types copied from wcsxy2sph. Needs to be kept up
 ; to date!
 ;
 map_types=['DEF','AZP','TAN','SIN','STG','ARC','ZPN','ZEA','AIR','CYP',$
            'CAR','MER','CEA','COP','COD','COE','COO','BON','PCO','SFL',$
            'PAR','AIT','MOL','CSC','QSC','TSC','SZP','HPX','HCT','XPH']

 if ( N_params() LT 2 ) then begin
     print,'Syntax - EXTAST, hdr, astr, [ noparams, ALT = ]'
     return
 endif

 proj0 = ['CYP','CEA','CAR','MER','SFL','PAR','MOL','AIT','BON','PCO', $
          'TSC','CSC','QSC']
 radeg = 180.0D0/!DPI
 keyword = STRUPCASE(strtrim(strmid( hdr, 0, 8), 2))

; Extract values from the FITS header.   This is either up to the first slash
; (free format) or first space

 space = strpos( hdr, ' ', 10) + 1
 slash = strpos( hdr, '/', 10)  > space
 
 N = N_elements(hdr)
 len = (slash -10) > 20
 len = reform(len,1,N)
 lvalue = strtrim(strmid(hdr, 10, len),2)
 remchar,lvalue,"'"
 zparcheck,'EXTAST',hdr,1,7,1,'FITS image header'   ;Make sure valid header
 noparams = -1                                    ;Assume no astrometry to start

 if N_elements(alt) EQ 0 then begin
     alt = '' & altstr = ''
 endif else BEGIN
     if (alt EQ '1') then alt = 'A' else alt = strupcase(alt)
     altstr = ' for alternate system '+alt
 ENDELSE
 
 ; Search for astrometric axes:
 test = STREGEX(keyword,'^CTYPE[1-9][0-9]{0,2}'+alt+'$', LENGTH = ctlen)
 typ = WHERE(test GE 0, ntyp)
 lon = -1  & lat = -1
 lon_form = -1 & lat_form = -1
 
 IF ntyp GT 0 THEN BEGIN
     ctlen = ctlen[typ] - STRLEN('CTYPE'+alt) ; gives # digits in axis number
     
     lon0 = WHERE(STRMID(lvalue[typ],0,5) EQ 'RA---')
     lon1 = WHERE(STRMID(lvalue[typ],1,4) EQ  'LON-')
     lon2 = WHERE(STRMID(lvalue[typ],2,4) EQ   'LN-')
     lon = [lon0, lon1, lon2]
     form = [REPLICATE(0,N_ELEMENTS(lon0)),REPLICATE(1,N_ELEMENTS(lon1)), $
             REPLICATE(2,N_ELEMENTS(lon2))]
     good = WHERE(lon GT 0, ngood)
     IF ngood GT 1 THEN MESSAGE, /INFORMATIONAL, $
                  'Multiple longitude axes'+altstr+': Using last."
     lon = MAX(lon, subs)
     lon_form = lon GE 0 ? form[subs] : -1

     lat0 = WHERE(STRMID(lvalue[typ],0,5) EQ 'DEC--')
     lat1 = WHERE(STRMID(lvalue[typ],1,4) EQ  'LAT-')
     lat2 = WHERE(STRMID(lvalue[typ],2,4) EQ   'LT-')
     lat = [lat0, lat1, lat2]
     form = [REPLICATE(0,N_ELEMENTS(lat0)),REPLICATE(1,N_ELEMENTS(lat1)), $
             REPLICATE(2,N_ELEMENTS(lat2))]
     good = WHERE(lat GT 0, ngood)
     IF ngood GT 1 THEN MESSAGE, /INFORMATIONAL, $
                  'Multiple latitude axes'+altstr+': Using last."
     lat = MAX(lat,subs)
     lat_form = lat GE 0 ? form[subs] : -1
 ENDIF
;
; Longitude axis data is initially stored in element 0 and latitude
; axis data in element 1 of the various arrays. For backwards compatibility,
; if latitude has a lower axis number in the FITS header, they will be swapped 
; into the (latitude, longitude) order in the final structure, and the .REVERSE 
; field will be set to true (ie. 1B).
; 
 lonc = lon GE 0 ? STRMID(keyword[typ[lon]],5,ctlen[lon]) : '1'
 latc = lat GE 0 ? STRMID(keyword[typ[lat]],5,ctlen[lat]) : '2'

 ctype = ['','']
 l = where(keyword EQ 'CTYPE'+lonc+alt,  N_ctype1)
 if N_ctype1 GT 0 then ctype[0] = lvalue[l[N_ctype1-1]]
 l = where(keyword EQ 'CTYPE'+latc+alt,  N_ctype2)
 if N_ctype2 GT 0 then ctype[1] = lvalue[l[N_ctype2-1]]
 ctype = strtrim(ctype,2)

 badco = lon_form NE lat_form 
 CASE lon_form OF
     -1: coord = 'X'  ; unknown type of coordinate
      0: coord = 'C'  ; celestial coords, i.e. RA/Dec
      1: BEGIN  ; longitude format is xLON where x = G, E, etc.
          coord = STRMID(ctype[0],0,1)
          badco = badco || coord NE STRMID(ctype[1],0,1)
      END
      2: BEGIN  ; longitude format is yzLN 
          coord = STRMID(ctype[0],0,2)
          badco = badco || coord NE STRMID(ctype[2],0,2)
      END
      ELSE: MESSAGE, 'Internal error: unexpected lon_form' 
 ENDCASE

 proj = STRMID(ctype[0], 5, 3)
 badco = badco || proj NE STRMID(ctype[1], 5, 3)
 IF badco THEN BEGIN
     MESSAGE, 'ERROR' + altstr + $
      ': longitude and latitude coordinate types must match:', /CONTINUE
     MESSAGE, 'Coords were CTYPE'+lonc+alt+': ' + ctype[0] + $
                        '; CTYPE'+latc+alt+': ' + ctype[1]
 ENDIF
 
 naxis = lonarr(2)
 l = where(keyword EQ 'NAXIS'+lonc,  N_ctype1)
 if N_ctype1 GT 0 then naxis[0] = lvalue[l[N_ctype1-1]]
 l = where(keyword EQ 'NAXIS'+latc,  N_ctype2)
 if N_ctype2 GT 0 then naxis[1] = lvalue[l[N_ctype2-1]]
  
  
; If the standard CTYPE* astrometry keywords not found, then check if the
; ST guidestar astrometry is present

 check_gsss = (N_ctype1 EQ 0)
 if N_ctype1 GE 1  then check_gsss = (strmid(ctype[0], 5, 3) EQ 'GSS')

 if check_gsss then begin

        l = where(keyword EQ 'PPO1'+alt,  N_ppo1)
        if N_ppo1 EQ 1 then begin 
                gsssextast, hdr, astr, gsssparams
                if gsssparams EQ 0 then noparams = 4
                return
        endif
        ctype = ['RA---TAN','DEC--TAN']
 endif

 if (ctype[0] EQ 'PIXEL') then return
 if N_ctype2 EQ 1 then if (ctype[1] EQ 'PIXEL') then return

 crval = dblarr(2)

 l = where(keyword EQ 'CRVAL'+lonc+alt,  N_crval1)
 if N_crval1 GT 0 then crval[0] = lvalue[l[N_crval1-1]]
 l = where(keyword EQ 'CRVAL'+latc+alt,  N_crval2)
 if N_crval2 GT 0 then crval[1] = lvalue[l[N_crval2-1]]
 if (N_crval1 EQ 0) || (N_crval2 EQ 0) then return  

 crpix = dblarr(2)
 l = where(keyword EQ 'CRPIX'+lonc+alt,  N_crpix1)
 if N_crpix1 GT 0 then crpix[0] = lvalue[l[N_crpix1-1]]
 l = where(keyword EQ 'CRPIX'+latc+alt,  N_crpix2)
 if N_crpix2 GT 0 then crpix[1] = lvalue[l[N_crpix2-1]]
 if (N_crpix1 EQ 0) || (N_crpix2 EQ 0) then return  


 cd = dblarr(2,2)
 cdelt = [1.0d,1.0d]

GET_CD_MATRIX:

 l = where(keyword EQ 'PC'+lonc+'_'+lonc + alt,  N_pc11) 
 if N_PC11 GT 0 then begin 
        cd[0,0]  = lvalue[l]
        l = where(keyword EQ 'PC'+lonc+'_'+latc + alt,  N_pc12) 
        if N_pc12 GT 0 then cd[0,1]  = lvalue[l[N_pc12-1]]
        l = where(keyword EQ 'PC'+latc+'_'+lonc + alt,  N_pc21) 
        if N_pc21 GT 0 then cd[1,0]  = lvalue[l[N_pc21-1]]
        l = where(keyword EQ 'PC'+latc+'_'+latc + alt,  N_pc22) 
        if N_pc22 GT 0 then cd[1,1]  = lvalue[l[N_pc22-1]]
         l = where(keyword EQ 'CDELT'+lonc+ alt,  N_cdelt1) 
        if N_cdelt1 GT 0 then cdelt[0]  = lvalue[l[N_cdelt1-1]]
         l = where(keyword EQ 'CDELT'+latc+ alt,  N_cdelt2) 
        if N_cdelt2 GT 0 then cdelt[1]  = lvalue[l[N_cdelt2-1]]
        noparams = 3
 endif else begin 

    l = where(keyword EQ 'CD'+lonc+'_'+lonc + alt,  N_cd11) 
    if N_CD11 GT 0 then begin        ;If CD parameters don't exist, try CROTA
        cd[0,0]  = strtrim(lvalue[l[N_cd11-1]],2)
        l = where(keyword EQ 'CD'+lonc+'_'+latc + alt,  N_cd12) 
        if N_cd12 GT 0 then cd[0,1]  = lvalue[l[N_cd12-1]]
        l = where(keyword EQ 'CD'+latc+'_'+lonc + alt,  N_cd21) 
        if N_cd21 GT 0 then cd[1,0]  = lvalue[l[N_cd21-1]]
        l = where(keyword EQ 'CD'+latc+'_'+latc + alt,  N_cd22) 
        if N_cd22 GT 0 then cd[1,1]  = lvalue[l[N_cd22-1]]
        noparams = 2
    endif else begin

; Now get rotation, first try CROTA2, if not found try CROTA1, if that
; not found assume North-up.   Then convert to CD matrix - see Section 5 in
; Greisen and Calabretta

        l = where(keyword EQ 'CDELT'+lonc + alt,  N_cdelt1) 
        if N_cdelt1 GT 0 then cdelt[0]  = lvalue[l[N_cdelt1-1]]
        l = where(keyword EQ 'CDELT'+latc + alt,  N_cdelt2) 
        if N_cdelt2 GT 0 then cdelt[1]  = lvalue[l[N_cdelt2-1]]
        if (N_cdelt1 EQ 0) || (N_Cdelt2 EQ 0) then return   
                                           ;Must have CDELT1 and CDELT2

        l = where(keyword EQ 'CROTA'+latc + alt,  N_crota) 
        if N_Crota EQ 0 then $
            l = where(keyword EQ 'CROTA'+lonc + alt,  N_crota) 
        if N_crota EQ 0 then begin
	          l = where(keyword EQ 'PC001001', N_PC00)
	          l = where(keyword EQ 'CD001001', N_CD00)
	          if (N_PC00 GT 0) || (N_CD00 GT 0) then begin
	              message,'Updating obsolete CD matrix representation',/INF
		            FITS_CD_FIX, hdr
		            keyword = strtrim(strmid(hdr,0,8),2)
		            goto, GET_CD_MATRIX
	          endif else crota = 0.0d 
	      endif else crota = double(lvalue[l[N_crota-1]])/RADEG
        cd = [ [cos(crota), -sin(crota)],[sin(crota), cos(crota)] ] 
 
        noparams = 1           ;Signal AIPS-type astrometry found
     
    endelse
 endelse
 
 case proj of 
     'ZPN': npv = 21
     'SZP': npv = 3
     else:  npv = 2
 endcase

 index = proj EQ 'ZPN' ? strtrim(indgen(npv),2) : strtrim(indgen(npv)+1,2)     
 pv2 = dblarr(npv)

 for i=0,npv-1 do begin 
     l = where(keyword EQ 'PV'+latc+ '_' + index[i] + alt,  N_pv2)
     if N_pv2 GT 0 then pv2[i] = lvalue[l[N_pv2-1]] 
 endfor
 
 pv1 = DBLARR(5)
 pv1_set = BYTARR(5)
 FOR i=0,4 DO BEGIN
     l = WHERE(keyword EQ 'PV'+lonc+'_' + STRTRIM(i,2) + alt, N_pv1)
     pv1_set[i] = N_pv1 GT 0
     IF pv1_set[i] THEN pv1[i] = DOUBLE(lvalue[l[N_pv1-1]])
 ENDFOR
 xyoff = pv1[0] NE 0d0
 phi0  = pv1[1]
 if pv1_set[2] THEN theta0 = pv1[2]
 if pv1_set[3] then longpole = pv1[3] else begin
     l = where(keyword EQ 'LONPOLE' + alt,  N_lonpole)
     if N_lonpole GT 0 then  longpole = double(lvalue[l[N_lonpole-1]]) 
 endelse
 if pv1_set[4] then latpole = pv1[4] else begin
     l = where(keyword EQ 'LATPOLE' + alt,  N_latpole)
     latpole = N_latpole GT 0 ? double(lvalue[l[N_latpole-1]]) : 90d0 
 endelse
 
; Convert NCP projection to generalized SIN projection (see Section 6.1.2 of 
; Calabretta and Greisen (2002)

 if proj EQ 'NCP' then begin
     ctype = repstr(ctype,'NCP','SIN')
     proj = 'SIN'
     PV2 = [0d0, 1d0/tan(crval[1]/radeg) ]
     longpole = 180d0
 endif 
 
; Convert GLS projection (Sect 6.1.4, ibid), but per e-mail from Mark
; Calabretta the correction to CRPIX and CRVAL should only be applied
; to the second axis.
 IF proj EQ 'GLS' THEN BEGIN
     IF crota EQ 0d0 THEN BEGIN
         crpix[1] -= crval[1]/cdelt[1] ; Shift reference point to dec = 0
         crval[1] = 0d0
         ctype = repstr(ctype,'GLS','SFL')
         proj = 'SFL'
     ENDIF
 ENDIF 

 test = WHERE(proj EQ map_types)
 known = test GE 0
 
 ; If LONPOLE (or PV1_3) is not defined in the header, then we must determine 
; its default value.    This depends on the value of theta0 (the native
; longitude of the fiducial point) of the particular projection)

 conic = (proj EQ 'COP') || (proj EQ 'COE') || (proj EQ 'COD') || $
         (proj EQ 'COO')

 IF conic THEN BEGIN 
     IF N_pv2 EQ 0 THEN message, $
     'ERROR -- Conic projections require a PV2_1 keyword in FITS header'
     theta_a = pv2[0]
 ENDIF ELSE BEGIN ; Is it a zenithal projection?
     if (proj EQ 'AZP') || (proj EQ 'SZP') || (proj EQ 'TAN') || $
        (proj EQ 'STG') || (proj EQ 'SIN') || (proj EQ 'ARC') || $
        (proj EQ 'ZPN') || (proj EQ 'ZEA') || (proj EQ 'AIR') || $
        (proj EQ 'XPH') then begin
         theta_a = 90d0
     endif else theta_a = 0d0
 ENDELSE
     
 IF ~pv1_set[2] THEN BEGIN
     theta0 = theta_a 
     pv1[2] = theta_a
 ENDIF
 
 if N_elements(longpole) EQ 0 then begin 
     if crval[1] GE theta0 then longpole = 0d0 else longpole = 180d0
     if pv1_set[1] THEN longpole += phi0
 endif
 
 pv1[3] = longpole
 pv1[4] = latpole

 IF xyoff && (phi0 NE 0d0 || theta0 NE theta_a) THEN BEGIN 
 ; calculate IWC offsets x_0, y_0
     WCSSPH2XY, phi0, theta0, x0, y0, CTYPE = ctype, PV2 = pv2
     x0y0 = [x0, y0]
 ENDIF ELSE x0y0 = [0d0, 0d0]
   
 axes = FIX([lonc,latc])
 flip = axes[0] GT axes[1]
 IF flip THEN BEGIN
     naxis = REVERSE(naxis)
     axes  = REVERSE(axes)
     cdelt = REVERSE(cdelt)
     crpix = REVERSE(crpix)
     crval = REVERSE(crval)
     ctype = REVERSE(ctype)
     cd    = ROTATE(cd,2)
     x0y0  = REVERSE(x0y0)
 ENDIF
 
 equinox = GET_EQUINOX( hdr,eq_code, ALT = alt) 
 IF equinox EQ 0 THEN equinox = !values.D_NAN
 radecsys = ''
 mjdobs  = !values.D_NAN
 dateobs = 'UNKNOWN'
 l = WHERE(keyword EQ 'RADESYS' + alt,  N_rdsys)
 IF N_rdsys GT 0 THEN radecsys = lvalue[l[N_rdsys-1]] ELSE BEGIN
     l = WHERE(keyword EQ 'RADECSYS',  N_rdsys)
     IF N_rdsys GT 0 THEN radecsys = lvalue[l[N_rdsys-1]]
 ENDELSE
 IF N_rdsys GT 0 THEN radecsys = STRUPCASE(STRTRIM(radecsys,2))
 
 l = WHERE(keyword EQ 'MJD-OBS',  N_mjd)
 IF N_mjd GT 0 THEN mjdobs = DOUBLE(lvalue[l[N_mjd-1]])
 l = WHERE(keyword EQ 'DATE-OBS',  N_date)
 IF N_date GT 0 THEN dateobs = STRUPCASE(lvalue[l[N_date-1]])
 
 IF N_mjd GT 0 && N_date EQ 0 THEN dateobs = date_conv(mjdobs+2400000.5d0,'FITS')
 IF N_date GT 0 THEN BEGIN
                            ; try to convert to standard format:
     dateobs = date_conv(dateobs,'FITS', BAD_DATE=bad_date) 
     IF ~bad_date THEN BEGIN
        mjdtest = date_conv(dateobs,'MODIFIED')
        IF N_mjd EQ 0 THEN mjdobs = mjdtest ELSE $
            IF ABS(mjdtest - mjdobs) GT 1 THEN MESSAGE, $
               'DATE-OBS and MJD-OBS are inconsistent'
     ENDIF ELSE dateobs = 'UNKNOWN'
 ENDIF

 IF (coord EQ 'C' || coord EQ 'E' || coord EQ 'H') THEN BEGIN
     IF N_rdsys EQ 0 THEN CASE eq_code OF
        -1: radecsys = 'ICRS' ; default if no header info.
         0: radecsys = equinox GE 1984d0 ? 'FK5' : 'FK4'
         1: radecsys = equinox GE 1984d0 ? 'FK5' : 'FK4'
         2: radecsys = 'FK4'
         3: radecsys = 'FK5'
         4: ; shouldn't get here as implies radecsys exists.
         else: MESSAGE, 'Internal error: unrecognised eq_code'
     ENDCASE
 ENDIF
 
; Note that the dimensions and datatype of each tag must be explicit, so that
; there is no conflict with structure definitions from different FITS headers
 
 ASTR = {NAXIS:naxis, CD: cd, CDELT: cdelt, CRPIX: crpix, CRVAL: crval, $
         CTYPE: string(ctype), $
         LONGPOLE: double( longpole[0]),  LATPOLE: double(latpole[0]), $
         PV2: pv2, PV1: pv1, $
         AXES: axes, REVERSE: flip, $
         COORD_SYS: coord, PROJECTION: proj, KNOWN: known, $
         RADECSYS: radecsys, EQUINOX: DOUBLE(equinox), $
         DATEOBS: dateobs, MJDOBS: DOUBLE(mjdobs), X0Y0: x0y0}

; Check for any distortion keywords

 if strlen(ctype[0]) GE 12 then begin
     distort_flag = strmid(ctype[0],9,3)
     case distort_flag of 
         'SIP': begin
             l = where(keyword EQ 'A_ORDER',  N) 
             if N GT 0 then a_order  = lvalue[l[N-1]] else a_order = 0
             l = where(keyword EQ 'B_ORDER',  N) 
             if N GT 0 then b_order  = lvalue[l[N-1]] else b_order = 0
             l = where(keyword EQ 'AP_ORDER',  N) 
             if N GT 0 then ap_order  = lvalue[l[N-1]] else ap_order = 0
             l = where(keyword EQ 'BP_ORDER',  N) 
             if N GT 0 then bp_order  = lvalue[l[N-1]] else bp_order = 0
             a = fltarr(a_order+1,a_order+1)
             b = fltarr(b_order+1,b_order+1) 
             ap = fltarr(ap_order+1,ap_order+1)
             bp = fltarr(bp_order+1,bp_order+1)

             for i=0, a_order do begin
                 for j=0, a_order do begin
             l = where(keyword EQ 'A_' + strtrim(i,2) + '_' + strtrim(j,2), N)
             if N GT 0 then a[i,j] = lvalue[l[N-1]]
                 endfor
             endfor

             for i=0, b_order  do begin
                 for j=0, b_order do begin
             l = where(keyword EQ 'B_' + strtrim(i,2) + '_' + strtrim(j,2), N)
             if N GT 0 then b[i,j] = lvalue[l[N-1]]
                 endfor
             endfor

             for i=0, bp_order do begin
                 for j=0, bp_order do begin
             l = where(keyword EQ 'BP_' + strtrim(i,2) + '_' + strtrim(j,2), N)
             if N GT 0 then bp[i,j] = lvalue[l[N-1]]
                 endfor
             endfor

             for i=0, ap_order do begin
                 for j=0, ap_order do begin
             l = where(keyword EQ 'AP_' + strtrim(i,2) + '_' + strtrim(j,2), N)
             if N GT 0 then ap[i,j] = lvalue[l[N-1]]
                 endfor
             endfor
   
             distort = {name:distort_flag, a:a, b:b, ap:ap, bp:bp}
             astr = create_struct(temporary(astr), 'distort', distort)
         end
     else: message,/con,'Unrecognized distortion acronym: ' + distort_flag 
 endcase
 endif
 return
 end
