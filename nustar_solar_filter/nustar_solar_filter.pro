

pro nustar_solar_filter,cldir,obsid,$
                        regfile = regfile,abstr = abstr,$
                        erange = erange, tbin=tbin,mindt=mindt,$
                        usr=usr, help = help



  IF n_elements(cldir) EQ 0 OR keyword_set(help) THEN BEGIN
     print, 'Help for nustar_solar_filter: '
     print, 'syntax: nustar_solar_filter,cldir,obsid, '
     print,'                            regfile = regfile,abstr = abstr,'
     print,'                            erange = erange, tbin=tbin,mindt=mindt,'
     print,'                            usr=usr, help = help'
     
     print, ' Required inputs: '
     print, ' cldir is the full path to the event_cl directory that you want to check'
     print, ' obsid is the name of the observation (not sure why this is necessary...)'
     print, ' Optional keywords: '
     print, ' regfile is the region file that you want to use to filter the focal plane'
     print, '     (e.g. if there is some stray light). Default is no region filtering. '
     print, " abstr is a string 'A' or 'B', default is 'A' "
     print, " erange is an array of strings that sets the energy range, default is: ['5', '10'] "
     print,' tbin is....'
     print,' mindt is....'
     print,' usr is...'
     print,' /help shows this message.'
     return

  ENDIF


  IF n_elements(cldir) GT 1 OR n_elements(obsid) GT 1 THEN message, 'Only give me one file at a time!'
  

  ; Default is to use FPMA. Note that because of 
  ; the masking below it's a pain to do this for
  ; BOTH A and B at once.
  IF ~keyword_set(abstr) THEN abstr = 'A' ELSE $
     IF n_elements(abstr) NE 1 THEN message, 'abstr: Only one FPM at a time.'

  ; Default usr to empty string:
  IF ~keyword_set(usr) THEN usr = '' 

  ; Default energy band to 3 - 20 keV:
  IF ~keyword_set(erange) THEN erange = ['3', '20'] 


                                ; Check for the minimum duration of a
                                ; GTI. Default to 10seconds
  if not keyword_set(mindt) then mindt=10.

                                ; Check for the minimum duration of a
                                ; GTI. Default to 100seconds
  if not keyword_set(tbin) then tbin=100.



  ; Default mask is everything:
  mask = intarr(1000, 1000)
  mask[*, *]=1

  ; If you've set the region file, then we have to do a little more work:
  IF keyword_set(regfile) THEN begin 
                                ; Make a temp directory to work in:
     tmpdir = 'nustar_solar_filter_tmp'
     file_mkdir, tmpdir
                                ; Make the images into the tmpdir
     make_nustar_image, cldir, obsid, erange, abstr[iab], tmpdir
        
     ; Generate a mast based on the region file:
     mask = reg2mask(imfile, regfile)
  ENDIF
  
  


     ; Read in the standard GTIs

  gtifile = cldir+'/nu'+obsid+abstr+'01_'+usr+'gti.fits'
  IF ~file_test(gtifile) THEN BEGIN
     gtifile += '.gz'
     IF ~file_test(gtifile) THEN message, 'Missing GTI file: '+gtifile
  ENDIF
  gti = mrdfits(gtifile, 1, eh, /silent)

;  gti=mrdfits(cldir[ep]+'/nu'+obsid[ep]+abstr[iab]+'01_'+usr+ $
;              'gti.fits',1,eh,/silent)
 
  t1 = gti.start
  t2 = gti.stop

        

                                ; Convert to energy range to PI values:
  pi1 = (float(erange[0]) - 1.6) / 0.04
  pi2 = (float(erange[1]) - 1.6) / 0.04
  


                                ; Find only GTIs of at least mindt duration:
  ii=where(t2-t1 gt mindt, ngti)
  t1=t1[ii]
  t2=t2[ii]
     
     ; Set the first second for this OBSID
  t0=t1[0]
  
                                 ; Oversample the GTIs into bins with
                                ; tbin size (if tbin is set). Probably
                                ; should default this to some
                                ; reasonable bin size.
if keyword_set(tbin) then begin
    undefine,newt1,newt2
    cnt=0
    ep = 0
    for i=0,ngti-1 do begin
       if ep eq 0 then ntep=0 else for e=0,ep-1 do ntep+=nt[e]

       nbin=floor((t2[i]-t1[i])/tbin)
       if nbin le 1 then begin
          push,newt1,t1[i+ntep]
          push,newt2,t2[i+ntep]
          cnt++
       endif else BEGIN
          dt=(t2[i]-t1[i])/nbin
          for j=0,nbin-1 do begin
             push,newt1,t1[i+ntep]+j*dt
             push,newt2,t1[i+ntep]+(j+1)*dt
             cnt++
          endfor
       endelse
    ENDFOR

    ngti=cnt
    t1 = newt1
    t2 = newt2
 ENDIF



; Construct the rate histograms
rate=fltarr(ngti)


                                ; Read in events and filter out things
                                ; outside of the (0--1000 ) and
                                ; outside of the pi energy range amd
                                ; in the "good" part of the spatial
                                ; mask that you cosntructed above...
evtfile = cldir+'/nu'+obsid+abstr+'01_cl.evt'
IF ~file_test(evtfile) THEN BEGIN
   evtfile +='.gz'
   IF ~file_test(evtfile) THEN message, 'Missing event file: '+evtifle
endif

evts = mrdfits(evtfile, 1, eh, /silent)

goodones=where(mask[evts.x-1,evts.y-1] gt 0.5 and evts.pi ge pi1 and evts.pi lt pi2)

                                ; Time histogram for the GTI
                                ; oversampled bins...there's a
                                ; more elegant way to do this, but
                                ; this is probably fast enough/
for i=0,n_elements(t1)-1 do begin
   jj=where(evts[goodones].time ge t1[i] and evts[goodones].time lt t2[i])
   if jj[0] ne -1 then rate[i]+=n_elements(jj)
endfor


; Convert counts / bin to counts / sec (NOT livetime correcte...)
err=sqrt(rate)/(t2-t1)
rate=rate/(t2-t1)

;;;; You have the histogram as of here... ;;;;


col=strarr(n_elements(t1))
col[0:ngti-1]=1




plot, [0], /nodata, $
      /xsty, xrange = [min(t1), max(t2)] / 3600. / 24., $
      /ysty, yrange = [0, max(rate) +max(err)]

FOR i =0, ngti - 1 DO BEGIN
   oplot, [t1[i], t2[i]] / 3600. / 24., rate[i] * [1, 1], color = cgColor('White')
   oplot, [t1[i], t2[i]] / 3600. / 24., rate[i]+ err[i] * [-1, 1], color = cgColor('White')
ENDFOR

;plot,[0],/nodata,xra=[min(t1) / 3600./24.,max(t2/3600./24.)],/xst,yra=[0,max(rate)+max(err)],/yst
;for i=0,ngti-1 do begin
;    oplot,[t1[i],t2[i]]/3600./24.,rate[i]+[0.,0.],color=col[i]
;    oplot,(t1[i]+t2[i])/2./3600./24.+[0.,0.],rate[i]+err[i]*[-1.,1.],color=col[i]
;endfor
;stop


print,'When asked to click, click twice (one time each at the lower,'
print,'   then upper, ends of range) and then click outside the box'
undefine,excl1,excl2,incl1,incl2
yesno='n'
while strmid(yesno,0,1) ne 'y' do begin

   undefine,x,y
   clicker,x,y,/noprint
   todo=''
;read,todo,prompt='Exclude (x), Include (i), Abandon (a)? '
   print,'Exclude (x), Include (i), Abandon (a)? '
   todo=get_kbrd()
   if strmid(todo,0,1) eq 'x' or strmid(todo,0,1) eq 'i' then begin
      if strmid(todo,0,1) eq 'x' then begin
         push,excl1,x[0]*3600.*24.
         push,excl2,x[1]*3600.*24.
      endif else begin
         push,incl1,x[0]*3600.*24.
         push,incl2,x[1]*3600.*24.
      endelse
   endif
;read,yesno,prompt='Finished? '
   print,'Finished? (y/n) '
   yesno=get_kbrd()

   isdefined=size(excl1,/type)
   if isdefined ne 0 then for i=0,n_elements(excl1)-1 do begin
      ii=where((t1+t2)/2. gt excl1[i] and (t1+t2)/2. lt excl2[i])
      if ii[0] ne -1 then col[ii]=2
   endfor
   isdefined=size(incl1,/type)
   if isdefined ne 0 then for i=0,n_elements(incl1)-1 do begin
      ii=where((t1+t2)/2. gt incl1[i] and (t1+t2)/2. lt incl2[i])
      if ii[0] ne -1 then col[ii]=1
   endfor
   
   plot,[0],/nodata,xra=[0,max(t2/3600./24.)],/xst,yra=[0,max(rate)+max(err)],/yst
   for i=0,n_elements(t1)-1 do begin
      oplot,[t1[i],t2[i]]/3600./24.,rate[i]+[0.,0.],color=col[i]
      oplot,(t1[i]+t2[i])/2./3600./24.+[0.,0.],rate[i]+err[i]*[-1.,1.],color=col[i]
   endfor
   
endwhile


stop



ii=where(col eq 2)
if ii[0] ne -1 then begin
    undefine,newt1,newt2
    push,newt1,t1[ii[0]]+t0[0]
    if n_elements(ii) ge 2 then for i=1,n_elements(ii)-1 do $
          if ii[i]-ii[i-1] gt 1 then begin
        push,newt2,t2[ii[i-1]]+t0[0]
        push,newt1,t1[ii[i]]+t0[0]
    endif
    push,newt2,t2[ii[n_elements(ii)-1]]+t0[0]

    gti=mrdfits(cldir+'/nu'+obsid+abstr+'01_'+usr+'gti.fits',1,h,/silent)
    g1=gti.start
    g2=gti.stop
    for j=0,n_elements(newt1)-1 do begin
        for i=0,n_elements(g1)-1 do begin
            if g1[i] lt newt2[j] and g1[i] ge newt1[j] and $
                    g2[i] gt newt2[j] then g1[i]=newt2[j] $
              else if g2[i] gt newt1[j] and g2[i] le newt2[j] and $
                    g1[i] lt newt1[j] then g2[i]=newt1[j] $
              else if g1[i] lt newt1[j] and g2[i] gt newt2[j] then begin
                  temp=g2[i]
                  g2[i]=newt1[j]
                  g1=[g1[0:i],newt2[j],g1[i+1:n_elements(g1)-1]]
                  g2=[g2[0:i],temp,g2[i+1:n_elements(g2)-1]]
              endif else if newt1[j] le g1[i] and newt2[j] ge g2[i] then begin
                  g1=[g1[0:i-1],g1[i+1:n_elements(g1)-1]]
                  g2=[g2[0:i-1],g2[i+1:n_elements(g2)-1]]
              endif   ;else stop,'case not handled'
            if g1[i] lt newt2[j] and g1[i] ge newt1[j] and $
                    g2[i] gt newt2[j] then print,1 $
            else if g2[i] gt newt1[j] and g2[i] le newt2[j] and $ 
                    g1[i] lt newt1[j] then print,2 $
            else if g1[i] lt newt1[j] and g2[i] gt newt2[j] then print,3 $
            else if newt1[j] le g1[i] and newt2[j] ge g2[i] then print,4
        endfor
    endfor
    newgti=replicate({START:0.D, STOP:0.D},n_elements(g1))
    newgti.start=g1
    newgti.stop=g2
    sxaddpar,h,'NAXIS2',n_elements(g1)
    mwrfits,newgti,cldir+'/nu'+obsid+abstr+'01_usrgti.fits',h,/silent,/create

    print
    print,'Original Expsoure reduced from '+ $
           str(total(gti.stop-gti.start)/1000.,format='(F5.1)')+' ks to '+$
           str(total(newgti.stop-newgti.start)/1000.,format='(F5.1)')+' ks'
    print
endif


end
