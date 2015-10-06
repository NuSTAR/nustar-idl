FUNCTION nustar_rate, rate, real = real
  
; Returns the "real" incident rate for a given measured rate. If /real
; is set, then it
; returns the "observed" rate for a given incident rate.

IF keyword_set(real) THEN BEGIN
   result = rate / (1.d + rate * 2.5e-3)
ENDIF ELSE BEGIN
    result = rate / (1.d - rate * 2.5e-3)
 ENDELSE
return, result

end
