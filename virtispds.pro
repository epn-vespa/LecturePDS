function VIRTISPDS, input_filename, silent = silent, debug=debug
;  SE, LESIA, 19-25/6/2013: Further fixes as per Rosetta review (e.g., added quotes on numeric values)
;  SE, LESIA, 30/6/2015: Updated for 67P/Rosetta new geometry format
;

  objnum = 0	; std reading - this is fixed in v_readpds
  Instru = strtrim(Instru)     ; filter trailing spaces
;  QubeType = strcompress(QubeType, /rem)     ; filter trailing spaces
  If ProcLev EQ "" then ProcLev = 2      ; (older labels)

;'Sun-boresight angle, X',$	; older version



If  (VH) then begin 
 bid = ['H slit to N pole angle',$			; 4 new frames for H only
 'Sub S/C X coord',$
 'Sub S/C Y coord',$
 'Sub S/C Z coord']
GEOM_nam= [GEOM_nam,bid]

endif 

Bid=['X coord, corner1',$     ; Cartesian coord of px

'Surf longit, corner1 @start',$     ; coord at start and stop acqu
'Surf longit, corner1 @stop',$     ; coord at start and stop acqu



 GEOM_nam= [GEOM_nam,bid]
; extended geom for 67P
Geo_coef(Nrst:Nrst+14) = replicate(0.001, 15)	; Cart coord
Geo_coef(Nrst:Nrst+19) = replicate(0.0001, 20)	; start/stop long/lat
Geo_coef(Nrst:Nrst+7) = replicate(0.0001, 8)	; angles at corners
Geo_coef(Nrst:Nrst+9) = replicate(0.001, 10)	; elevation & alt & distance to centre
Nrst = Nrst + 5
Geo_coef(Nrst:Nrst+1) = replicate(0.0001, 2)	; subsolar long/lat
Geo_coef(Nrst) = 1	; codes
Nrst = Nrst + 1
Geo_coef(Nrst:Nrst+5) = replicate(0.0001, 6)	; various angles 

endif
Geo_coef = Geo_coef(0:N_elements(Geom_nam)-1)     ; retain current length

;stop
     if size(suf, /type) NE 8 then suf=create_struct('b_suf2',Uint(suf))	; turn to structure for later use @SE2012
         If N_elements(szq) EQ 2 then szq = [szq,1]
;  Laissent parfois sortir de confuses paroles