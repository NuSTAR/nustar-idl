
pro reglc,cldir,obsid,regfile,abstr,e1,e2,imfile,tbin=tbin,mindt=mindt,$
      usr=usr

                                ; Code for filter an observation on time ranges.
                                ; Default settings are to use the whole FoV and
                                ; the 3 to 5 keV band that is
                                ; sensitive to solar flare photons.

  
                                ; Check to see if there's a
                                ; region file that may exclude
                                ; the source or something...if not,
                                ; use the whole FOV
  if size(regfile,/type) eq 7 then mask=reg2mask(imfile,regfile) else $
     if size(regfile,/type) eq 0 then begin
     mask=intarr(1000,1000)
     mask[*,*]=1
  endif else if size(regfile,/type) ne 4 then stop,'REGFILE is unacceptable.'

                                ; Check to see if the erange keywrods
                                ; are set. If not, default to 3--5 keV
  if size(e1,/type) eq 0 then begin
     e1 = 3.
     e2 = 5.
  endif

                                ; Convert to energy range to PI values:
  pi1 = (e1 - 1.6) / 0.04
  pi2 = (e2 - 1.6) / 0.04
  

                                ; Check for the minimum duration of a
                                ; GTI. Default to 100seconds
  if not keyword_set(mindt) then mindt=100.

  ; Set this to insert "usr" into the GTI name. (Should always be true?)
  if keyword_set(usr) then usr='usr' else usr=''


  ; Clear old values, just in case you call this multiple times.
  undefine,t1, t2


                                ; This loops allows the user to do
                                ; more than one OBSID at a
                                ; time...probably better to force them
                                ; to do it once...
  t0=fltarr(n_elements(obsid))
  nt=fltarr(n_elements(obsid))
  for ep=0,n_elements(obsid)-1 do begin

     ; This loops allows the user to JUST use A, B, or both.
     for iab=0,n_elements(abstr)-1 do begin
        
                                ; Read in the "standard" GTIs fo the
                                ; cleaned events.
        gti=mrdfits(cldir[ep]+'/nu'+obsid[ep]+abstr[iab]+'01_'+usr+ $
                    'gti.fits',1,eh,/silent)

        if iab eq 0 then begin
           gti1=gti.start
           gti2=gti.stop
           tt1=gti1
           tt2=gti2
        endif else begin
           undefine,tt1
           undefine,tt2

           for i=0,n_elements(gti)-1 do begin

              ; Merge the GTIs:
              minval=min(abs(gti1-gti[i].start),ii)
              if minval lt mindt then begin
                 if gti1[ii] gt gti[i].start then push,tt1,gti1[ii] $
                 else push,tt1,gti[i].start
                 minval=min(abs(gti2-gti[i].stop),ii)
                 if gti2[ii] lt gti[i].stop then push,tt2,gti2[ii] $
                 else push,tt2,gti[i].stop
              endif
           endfor
           
        endelse
     endfor

     ; Find only GTIs of at least mindt duration:
     ii=where(tt2-tt1 gt mindt)
     tt1=tt1[ii]
     tt2=tt2[ii]

     ; Set the first second for this OBSID
     t0[ep]=tt1[0]
     nt[ep]=n_elements(tt1) ; This is the number of GTIs for this obsid

     ; Store the GTIs
     push,t1,tt1
     push,t2,tt2 
endfor

                                ; Oversample the GTIs into bins with
                                ; tbin size (if tbin is set). Probably
                                ; should default this to some
                                ; reasonable bin size.
if keyword_set(tbin) then begin
    undefine,newt1,newt2
    for ep=0,n_elements(obsid)-1 do begin
        cnt=0
        for i=0,nt[ep]-1 do begin
            if ep eq 0 then ntep=0 else for e=0,ep-1 do ntep+=nt[e]
            nbin=floor((t2[i]-t1[i])/tbin)
            if nbin le 1 then begin
                push,newt1,t1[i+ntep]
                push,newt2,t2[i+ntep]
                cnt++
            endif else begin
                dt=(t2[i]-t1[i])/nbin
                for j=0,nbin-1 do begin
                    push,newt1,t1[i+ntep]+j*dt
                    push,newt2,t1[i+ntep]+(j+1)*dt
                    cnt++
                endfor
            endelse
        endfor
        nt[ep]=cnt
    endfor
    t1=newt1
    t2=newt2
endif


; Construct the rate histograms
rate=fltarr(n_elements(t1))
for ep=0,n_elements(obsid)-1 do for iab=0,n_elements(abstr)-1 do begin

                                ; Read in events and filter out things
                                ; outside of the (0--1000 ) and
                                ; outside of the pi energy range.
    evts=mrdfits(cldir[ep]+'/nu'+obsid[ep]+$
          abstr[iab]+'01_cl.evt',1,eh,/silent)
    ii=where(mask[evts.x-1,evts.y-1] gt 0.5 and evts.pi ge e1 and evts.pi lt e2)

    ; Time histogram
    for i=0,n_elements(t1)-1 do begin
        jj=where(evts[ii].time ge t1[i] and evts[ii].time lt t2[i])
        if jj[0] ne -1 then rate[i]+=n_elements(jj)
    endfor

 endfor

; Convert counts / bin to counts / sec
err=sqrt(rate)/(t2-t1)
rate=rate/(t2-t1)

;;;; You have the histogram as of here... ;;;;


prevnt=0
for ep=0,n_elements(obsid)-1 do begin
    ii=indgen(nt[ep])+prevnt
    t1[ii]-=t0[ep]
    t2[ii]-=t0[ep]
    prevnt+=nt[ep]
endfor

;t1/=3600.*24.
;t2/=3600.*24.
col=intarr(n_elements(t1))
col[0:nt[0]-1]=1
;if n_elements(nt) ge 2 then col[nt[0]:nt[0]+nt[1]-1]=2
;if n_elements(nt) ge 3 then col[nt[0]+nt[1]:nt[0]+nt[1]+nt[2]-1]=4

plot,[0],/nodata,xra=[0,max(t2/3600./24.)],/xst,yra=[0,max(rate)+max(err)],/yst
for i=0,n_elements(t1)-1 do begin
    oplot,[t1[i],t2[i]]/3600./24.,rate[i]+[0.,0.],color=col[i]
    oplot,(t1[i]+t2[i])/2./3600./24.+[0.,0.],rate[i]+err[i]*[-1.,1.],color=col[i]
endfor

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
