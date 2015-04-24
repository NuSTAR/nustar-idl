;+
; NAME:
;   FILE_COMPILE
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20771
;   Craig.Markwardt@nasa.gov
;
; PURPOSE:
;   Compile an arbitrary procedure
;
; CALLING SEQUENCE:
;   FILE_COMPILE, pathname, ERROR=error, ERRMSG=errmsg
;
; DESCRIPTION: 
;
;   FILE_COMPILE compiles a file containing an IDL procedure or
;   function.  After compilation the user may call the procedure or
;   function.
;
;   If compilation is successful, then ERROR is set to 0.  If the
;   compilation fails, then, ERROR is set to a non-zero error code,
;   and ERRMSG is set to a descriptive error message.
;
; INPUTS:
;
;   PATHNAME - scalar string, path name of file containing IDL
;              procedure or function.  PATHNAME must end in '.pro'.
;              The directory containing the file must be readable and
;              it must be possible to change to that directory as a
;              working directory using CD.
;
; KEYWORDS:
;
;   ERROR - upon return, a scalar integer giving status of
;           compilation, either 0 for success or non-zero for failure.
;
;   ERRMSG - upon return, a scalar string giving a descriptive error
;            message.
;
;   PRO_NAME - upon return, the name of the procedure, with path name
;              and '.pro' suffix removed.
;
; SEE ALSO:
;
;   RESOLVE_ROUTINE
;
; MODIFICATION HISTORY:
;   Documented, CM, Jun 2009
;   Small documentation changes, CM, 2012-10-09
;
;  $Id: file_compile.pro,v 1.2 2012/10/17 23:53:58 cmarkwar Exp $
;
;-
; Copyright (C) 2009, 2012, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

pro file_compile, pathname, pro_name=proname, error=err, errmsg=errmsg

  err = 1
  errmsg = ''

  dir = file_dirname(pathname)
  proname  = file_basename(pathname, '.pro')  ;; Remove suffix
  filename = file_basename(pathname)

  sep = path_sep(/search_path)

  if file_test(pathname, /read) EQ 0 then begin
     errmsg = 'FILE_COMPILE: '+pathname+' does not exist'
     err = -248
     return
  endif

  ;; Save current working directory
  cd, current=cwd
  changed_dir = 0
  !error_state.code = 0
  
  ;; Bracket this operation by a CATCH block, so that we don't
  ;; leave the working directory in a bad state
  catch, catcherr
  if catcherr EQ 0 then begin
     ;; Temporarily change to the subdirectory of the desired file
     changed_dir = 1
     cd, dir
     resolve_routine, proname, /either, /compile_full_file
     err = 0

  endif else begin
     errmsg = 'FILE_COMPILE: compilation of '+pathname+' failed'
  endelse
  catch, /cancel
  error_state = !error_state  ;; Save a temporary copy

  ;; Restore original working directory
  if changed_dir then cd, cwd

  if err NE 0 AND error_state.code NE 0 then begin
     ;; Attempt to capture the error condition returned by
     ;; RESOLVE_ROUTINE
     err    = error_state.code
     errmsg = error_state.msg
  endif

  return
end
