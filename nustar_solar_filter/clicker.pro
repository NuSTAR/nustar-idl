pro clicker,xarr,yarr,noprint=noprint,stp=stp,overplot=overplot

; This program can be used to click on the plot
; many times.  Also the SELECTOR.PRO program.
;
; INPUT:
;   Clicks with the cursor
;   /overplot Overplot the points
;   /noprint  Don't print anything
;   /stp      Stop at the end of the program
;
; OUTPUT:
;   xarr   Array of x-values
;   yarr   Array of y-values
;
; Written by D. Nidever  September 2005


; Getting plotting region
pos = [ (convert_coord(!p.clip(0:1),/device,/to_data))[0:1],$
      (convert_coord(!p.clip(2:3),/device,/to_data))[0:1] ]
xr = [pos(0),pos(2)]
yr = [pos(1),pos(3)]

; Initiatlize variables
n = 0.
undefine,xarr
undefine,yarr

if not keyword_set(noprint) then print,'Click outside the box to stop selecting'

cursor,curx,cury
if keyword_set(overplot) then oplot,[curx],[cury],ps=sym(5)

while (curx le max(xr) and curx ge min(xr) and cury le max(yr) and cury ge min(yr)) do begin

 
  if n eq 0 and not keyword_set(noprint) then begin
      print,'------------------------------'
      print,'         X            Y'
      print,'------------------------------'
  end
  if not keyword_set(noprint) then print,float(curx),float(cury)
  wait,0.5

  push,xarr,curx
  push,yarr,cury

  cursor,curx,cury
  if keyword_set(overplot) then oplot,[curx],[cury],ps=8

  n = n_elements(curx)

end

if not keyword_set(noprint) then print,'------------------------------'
wait,0.5

if keyword_set(stp) then stop

end
