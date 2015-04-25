PRO make_nustar_image, cldir, obsid, erange, fpm, outdir

  ; Wrapper for calling nuproducts from IDL.
  ; cldir is full path to the event_cl directory that you want to use.
  ; obsid is the sequence ID
  ; erange is an array of string, e.g.: ['5', '10']
  ; fpm is either 'A' or 'B'
  ; outdir is the full path of the output directory


  evtfile = 'nu'+obsid+fpm+'01_cl.evt'

  IF ~file_test(cldir+evtfile) THEN BEGIN
     ; Try adding gz:
     evtfile+='.gz'
     IF ~file_test(cldir+evtfile) THEN message, 'Missing event file: ', cldir+evtfile
  ENDIF

  
  evtheader = headfits(cldir+evtfile)
  ra_obj = string(fxpar(evtheader, 'RA_OBJ'), format = '(d0)')
  dec_obj = string(fxpar(evtheader, 'DEC_OBJ'), format = '(d0)')


  stem = 'nu'+obsid
  stemout = stem+'_'+erange[0]+'to'+erange[1]
  cmd='nuproducts srcra='+ra_obj+ ' srcdec='+dec_obj+$
      ' indir='+cldir+' ' + 'infile='+cldir+evtfile+ $
      ' instrument=FPM'+fpm+' steminputs='+stem+' stemout='+stemout+$
      ' outdir='+outdir+' bkgextract=no runmkarf=no runmkrmf=no' + $
      ' clobber=yes lcfile=NONE bkglcfile=NONE phafile=NONE'
  print, cmd
  spawn, cmd

  ; Clean up random junk
  giffile = outdir+'/'+stemout+'_im.gif'
  f = file_test(giffile)
  IF f THEN spawn, 'rm '+giffile

  regfile = outdir+'/'+stemout+'_src.reg'
  f = file_test(regfile)
  IF f THEN spawn, 'rm '+regfile


end
