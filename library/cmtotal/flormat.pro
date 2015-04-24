;+
; NAME:
;   FLORMAT
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   Craig.Markwardt@nasa.gov
;
; PURPOSE:
;   Format a string with named format variables
;
; CALLING SEQUENCE:
;   RESULT = FLORMAT(FORMAT, [ struct ], [x=x, y=y, ...], [_EXTRA=struct])
;
; DESCRIPTION: 
;
;  The function FLORMAT is used to easily insert a set of named
;  parameters into a string using simple format codes.  The key point
;  is that format strings use *named* parameters instead of the
;  position in the string.
;
;  FLORMAT makes it easy to make maintainable and understandable
;  format codes.  FLORMAT is a convenience routine, which will be most
;  suitable for formatting tabular output, but can be used for any
;  complicated string formatting job where the positional parameters
;  of STRING() become hard to manage.  Users of Python will recognize
;  FLORMAT as implementing "string interpolation."
;
;  The user passes a format string similar to the IDL printf-style
;  format string (i.e. using modified "%" notation), and a set of
;  named fields either by passing a structure, keywords, or both.  The
;  output strings are composed by inserting the named fields into the
;  format string with any requested formatting.
;
;  The function FLORMAT is equivalent to the STRING(...,FORMAT=fmt)
;  method of formatting a string, where the format string is allowed
;  to have the name of the variable. 
;
;  Let us consider an example of formatting a time with hours, minutes
;  and seconds into a string as HH:MM:SS.  One could use FLORMAT()
;  like this,
;
;     result = flormat('%(hour)02d:%(min)02d:%(sec)02d', $
;                      hour=hour, min=min, sec=sec)
;
;  The variables HOUR, MIN and SEC are allowed to be scalars or
;  vectors.  The key point here is that the format string contains the
;  *named* keyword variables (or structure entries).  Unlike STRING(),
;  the actual variables can be passed in any order, since the format
;  string itself describes in what order the values will be assembled.
;  This is similar to string interpolation in Python.
;
;  The same variable can appear multiple times in the format string,
;  but the user only need to specify that variable once.  For example,
;
;     result = flormat('<A="%(href)s">Download %(href)s</A>', $
;                      href='filename.txt')
;
;  Note that HREF appears twice in the format string.
;
; INPUT VARIABLES:
;
;  FLORMAT() allows you to pass in the values as named keywords as
;  shown above, where the keyword values are arrays, or by passing in
;  an array of structures.  A similar example to the one above is,
;  
;     S = replicate({hour: 0, min: 0, sec: 0}, 100)
;     ;   ... fill the structure S with 100 time values ...
;     result = flormat('%(hour)02d:%(min)02d:%(sec)02d', s)
;  
;  In this case S is an array of structures, and the result will be an
;  array of strings with the same number of elements as S.
;
;  Compare this with standard IDL where a FOR-loop is required, no
;  repetition is permitted, and it is difficult to see which format
;  code corresponds to which variable.  For example,
;   
;     for i = 0, n_elements(hour)-1 do begin
;       result(i) = string(hour(i), min(i), sec(i), $
;                          format='(%"%02d:%02d:%02d")')
;
;   The input structure STRUCT may be an array of structures or a
;   structure of arrays.  It is also possible pass *both* a structure
;   STRUCT and keywords.  The important thing is that the each keyword
;   and each STRUCT.FIELD must evaluate to the same number of
;   elements.  If they don't, then the smallest number of elements is
;   used.
;
; PRINTF-STYLE FORMAT CODES
;
;  FLORMAT() uses format codes in either C printf-style format codes
;  (the default), or a new "$" shell-style syntax if /SHELL_STYLE$ is
;  set.
;
;  FLORMAT() assumes that by default the C printf-style format codes
;  are passed.  FLORMAT() uses a slightly short-hand notation for
;  print-style format codes which saves some space and is more
;  flexible.
;
;  Standard printf-style format codes are of the form,
;      FORMAT='(%"...format here...")'     ;; Standard IDL
;  The FLORMAT printf-style format codes simply dispense with the
;  redundant parentheses and percent symbol,
;      FORMAT='...format here...'          ;; FLORMAT notation
;  This notation improves the readability of the format string, since
;  only the actual format string needs to be present.  Also, this
;  notation does not embed one set of quotation marks within another,
;  as the standard IDL notation does, so format strings with quotation
;  marks will be easier to compose.
;
;  Standard IDL format codes look like this,
;        %s - string
;        %d - integer
;        %04d - integer zero-padded to 4 spaces, etc
;
;  The new FLORMAT format strings look like this,
;
;        %(name)s - string based on variable named NAME
;        %(value)d - integer based on variable named VALUE
;        %(index)04d - integer based on variable named INDEX, 
;                      zero-padded to 4 spaces
;
;  As you can see, the only difference is the addition of the variable
;  name in parenthesis.  These names are looked up in the input 
;  keywords and/or structure passed to FLORMAT().
;
; SHELL-STYLE FORMAT CODES
;
;  Shell style "$" is a convenience notation when strict formatting is
;  less important.  Shell-style "$" format strings will be signaled by
;  setting the SHELL_STYLE$ keyword.  Note the trailing dollar-sign
;  '$'.  The format coes will look like this,
;
;       $name   - variable named NAME will be placed here
;       $value  - variable named VALUE will be placed here, etc.
;
;  This is exactly how Unix shell string interpolation works.
;  Variables are substituted into place using their "natural" format
;  code, based on the variable type.
;
;     result = flormat('<A=\"$href\">Download $href</A>', /shell_style$, $
;                      href='filename.txt')
;
;  Note that quotation marks still need to be escaped as \", just the
;  same as calling STRING() or PRINT with a %-style format string.
;
; CAVEATS:
;
;  FLORMAT() is a convenience routine meant mostly to improve the
;  readability and maintainability of format codes. FLORMAT() is not
;  meant for high performance applications.  It spends time parsing
;  the input format string.  It also spends memory building up a
;  temporary output structure.  However, for most applications such as
;  constructing tables of up to thousands of entries, FLORMAT() should
;  be perfectly adequate.
;
;  The name "FLORMAT" is a play on the words "floor-mat" and "format."
;  The "L" in FLORMAT can be thought of standing for "long-form" IDL
;  format codes.
;
; PARAMETERS:
;
;   FORMAT - format string used to 
;
;   STRUCT - input structure containing named entries.  This should
;            either be an array of structures, with each field
;            containing a scalar; or, a structure where each field
;            contains an array with the same number of elements.
;
; RETURNS:
;
;   The resulting formatted strings.  The return value will be an
;   array of strings containing the same number of elements as passed
;   as input.
;
; KEYWORD PARAMETERS:
;
;   SHELL_STYLE$ - if set, then the format string is a shell-style
;                  string.
;
;   All named keywords are available to be used as named formats in
;   your format code.  Values may be either scalar, or vector.
;   Vectors dimensions must match the dimensions of STRUCT (if
;   STRUCT is passed).
;
; EXAMPLE:
;
;  
;  ; Additional examples appear above.
;
; SEE ALSO:
;
;   STRING, Format codes, C print-style format codes
;
; MODIFICATION HISTORY:
;   Written, CM, 14 Sep 2009
;   Finalized and documented, CM, 08 Dec 2011
;
;  $Id: flormat.pro,v 1.9 2013/03/16 23:29:40 cmarkwar Exp $
;
;-
; Copyright (C) 2011, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
pro flormat_structcheck, s, n, tn
  COMPILE_OPT strictarr
  tn = ''
  if n_elements(s) EQ 0 then return

  tp = size(s,/type)
  if tp NE 8 then message, 'ERROR: input variable must be a structure'

  if n_elements(n) EQ 0 then n = n_elements(s.(0))
  tn = tag_names(s)
  nt = n_elements(tn)

  for i = 1, nt-1 do begin
     n = n < n_elements(s.(i))
  endfor

  return
end

function flormat, format0, s0, _EXTRA=extra, shell_style$=shell, $
                  format_am_pm=am_pm, format_days_of_week=days_of_week, $
                  format_months=months
  COMPILE_OPT strictarr

  if n_params() LT 1 AND n_elements(extra) EQ 0 then begin
      USAGE:
      message, 'USAGE: string = FLORMAT(FORMAT, struct) or', /info
      message, '       string = FLORMAT(FORMAT, x=x, y=y, ...) or', /info
      message, '       string = FLORMAT(FORMAT, _EXTRA=struct)', /info
      return, ''
  endif

  ;; FORMAT must be a scalar
  tp = size(format0,/type)
  if tp NE 7 OR n_elements(format0) GT 1 then begin
      message, 'ERROR: FORMAT must be a scalar format string'
  endif
  fmt = format0[0]

  if n_elements(s0) EQ 0 AND n_elements(extra) EQ 0 then begin
     message, 'ERROR: you must either specify a structure or keywords'
  endif

  ;; Do data-checking and also compute the total number of elements
  flormat_structcheck, s0,    n, tn0
  flormat_structcheck, extra, n, tn1

  ;; Decide on whether it is a (%"") C-style or ($"") shell type format string
  fmt_type = '%'
  if keyword_set(shell) then fmt_type = '$'

  ;; Regular expression for %(varname) or $varname splitting

  ;; Example:     "blah blah %(varname) blah blah"
  ;;   splits to  "blah blah "  and   "          "
  ;; Example:     "blah blah $varname blah blah"
  ;;  splits to   "blah blah "      " blah blah"
  ;;                 
  regex = '%\([^)]*\)'  ;;  %(varname)
  if fmt_type EQ '$' then begin
      regex = '('+regex+'|\$[a-zA-Z_][a-zA-Z0-9_]*)' ;; or $varname
  endif
  spos = strsplit(fmt, regex, /regex, /preserve_null, length=slen)

  ninterp = n_elements(spos)-1
  ;; No special format codes requested, so return immediately
  if ninterp EQ 0 then return, fmt
  
  ;; Separate the format string into the "surrounding" string data (FMTS)
  ;; and the interpolation data (FMTI).
  fmts = strmid(fmt, spos, slen)

  ipos = spos+slen
  ilen = spos[1:*] - ipos
  ipos = ipos[0:ninterp-1]
  case fmt_type of
      '%': begin   ;; %(NAME) -> NAME
          ipos = ipos + 2
          ilen = ilen - 3
      end
      '$': begin   ;;  $NAME  -> NAME
          ipos = ipos + 1
          ilen = ilen - 1
      end
  endcase
  fmti = strmid(fmt, ipos, ilen)

  for i = 0, ninterp-1 do begin

      varname = fmti[i]

      ;; Check structure
      wh = where(strupcase(varname) EQ tn0, ct)
      if ct GT 0 then begin
         wh = wh[0]
         ;; Example kind of this field
         exemplar = s0[0].(wh)
         srci = 0L
      endif else begin
         ;; Check EXTRA
         wh = where(strupcase(varname) EQ tn1, ct)
         if ct GT 0 then begin
            wh = wh[0]
            ;; Example kind of this field
            exemplar = (extra.(wh))[0]
            srci = 1L
         endif else begin
            message, 'ERROR: tag name "'+varname+'" does not exist in input structure'
         endelse
      endelse

      tp   = size(exemplar, /type)
      dims = size(exemplar, /dimensions)

      code = ''                     ;; Default: code is already in format str
      ;; If the user put $varname then we must decide on an output format
      if fmt_type EQ '$' then begin
          case tp of 
              1:  code = 'd'   ;; BYTE
              2:  code = 'd'   ;; INT
              3:  code = 'd'   ;; LONG
              4:  code = 'g'   ;; FLOAT
              5:  code = 'g'   ;; DOUBLE
              7:  code = 's'   ;; STRING
              12: code = 'd'   ;; UINT
              13: code = 'd'   ;; ULONG
              14: code = 'd'   ;; ULONG64
              15: code = 'd'   ;; LONG64
              else: message, string(varname, $
               format='("ERROR: $",A0," must of real, integer or string type")')
          endcase
      endif

      ;; New tag name
      tni = string(i, format='("N",I0)')

      if n_elements(news) EQ 0 then begin
          news = create_struct(tni, exemplar)
          imap = [wh]
          isrc = [srci]
      endif else begin
          news = create_struct(news, tni, exemplar)
          imap = [imap, wh]
          isrc = [isrc, srci]
      endelse

      ;; Add %-style format code to appropriate string
      fmts[i] = fmts[i] + '%'+code
  endfor

  ;; Transfer the data to the new output structure
  outs = replicate(news, n)
  for i = 0, n_elements(imap)-1 do begin
     if isrc[i] EQ 0 then begin
        outs.(i) = s0.(imap[i])
     endif else begin
        outs.(i) = extra.(imap[i])
     endelse        
  endfor  

  ofmt = strjoin(fmts)

;;  ;; Replace '"' by '\"' (poor man's REPSTR)
;;  ofmts = strsplit(ofmt, '"', /preserve_null, /extract)
;;  nquote = n_elements(ofmts)-1
;;  if nquote GT 0 then begin
;;      ofmts[0:nquote-1] = ofmts[0:nquote-1] + '\"'
;;      ofmt = strjoin(ofmts)
;;  endif

  ofmt1 = '(%"'+ofmt+'")'
  return, string(outs, format=ofmt1, $
                 am_pm=am_pm, days_of_week=days_of_week, months=months)

end
