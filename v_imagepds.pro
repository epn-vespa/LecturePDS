function V_IMAGEPDS, filename,label, img, NOSCALE = noscale, NOROTATE = norotate, SILENT = silent, prefix = prefix, suffix = suffix

;+ 
;
; NAME:
;	V_IMAGEPDS
; PURPOSE:
;	Read one image object in a PDS file
;
; CALLING SEQUENCE:
;	Result=V_IMAGEPDS(Filename,Label,img[,/NOSCALE,/SILENT] )
;
; INPUTS:
;	FILENAME = Scalar string containing the name of the PDS data file 
;		to be read.
;	Label = String array containing the "header" from the PDS file.
;	Img = "pointer" from v_objpds defining the object to be read
;
; OUTPUTS:
;	Result = image (2D) data array read from file, according to format described in label.
;
; OPTIONAL INPUT KEYWORDS:
;	NOSCALE - If present and non-zero, then the ouput data will not be
;		scaled using the optional SCALING_FACTOR and OFFSET keywords 
;		in the PDS header.   Default is to scale.
;
;	NOROTATE - If present and non-zero, the ouput data will not be
;		oriented using the optional DISPLAY_DIRECTION keywords. 
;
;	SILENT - Suppresses console messages.
;
; EXAMPLE:
;	Read a PDS file TEST.PDS into an IDL image array, im. Do not scale 
;	the data with BSCALE and BZERO.
;
;		IDL> im = V_IMAGEPDS( 'TEST.PDS', lbl, img, /NOSCALE)
;
;
; PROCEDURES USED:
;	v_readps library
;
; MODIFICATION HISTORY:
;	Adapted by John D. Koch from READFITS by Wayne Landsman,December,1994
;       25 Sep 1998, a.c.raugh: fixed bug which expected negative SAMPLE_BITS
;                               values to indicate real sample, causing a lonarr
;                               to be created rather than a fltarr; Fixed 
;                               calculation of byte offsets in detached PDS
;                               labels; Added lines to close and free logical
;                               units before return.
;       02 Oct 1998, a.c.raugh: Analyzed code and added comments throughout;
;                               Added code to deal properly with unsigned 
;                               integers and signed bytes; Re-wrote pointer
;                               parsing code to improve robustness;
;       27 July 1999, M. Barker: fixed bug that produced a negative skip when
;                               there was no offset provided in file pointer
;
;   Oct. 99       Modified for VIRTIS, Stephane Erard, IAS
;   Sept. 2000    Updated from SBNIDL 2.0, Stephane Erard
;          + added tests on offset and scaling factor
;          + fixed file path for any system
;          + convert to LSB architecture is needed
;   Nov. 2000     Fixed conversion to MSB, SE
;   Dec. 2000     Handles non-conformity in VIRTIS H DM files 
;            written before dec 2000, SE
;   Updated, June 2005 (SE, LESIA):
;          - Use modern swapping methods, much faster
;          (now process Vax floats and LSB integers independently)
;          - Now support all PDS data types, including PC_REAL
;  Updated, Oct 2005 (SE, LESIA):
;          - Added support for embedded browse images 
;          (independently from images; this function can be used to read both types)
;          - Object pointer parsing now in v_pointpds (+ fixed object pointers given in bytes)
;          - Now can read images mixed with other objects correctly
;          - Implemented basic bitmasking (in v_bmaskpds.pro)
; Updated, Dec 2005 (SE, LESIA):
;          - Fix bitmask reading if not provided between "
; Updated, Feb 2006 (SE, LESIA):
;          - Fixed structure length for I/O (solves rare EOF errors depending on dimensions)
; Updated, March 2006 (SE, LESIA):
;          - No longer tries to read data from label directory for detached labels (requires 6.0)
; Updated, June 2006 (SE):
;          - Now performs IEEE float swapping (required to read floats on Intel)
; Updated, Jan. 2007 (SE):
;          - Fixed to close all files when multiple images with attached label 
; Stephane Erard, LESIA, Feb 2007:
;          - New handling of detached labels (OK from IDL 5.5)
;          - Fixed for basic reading when multiple images are present 
;          (only objects IMAGE and BROWSE_IMAGE are read).
; Alejandro Cardesin, IASF, June 2007:
;          Fix for absent files error on Windows
; Stephane Erard, LESIA, Oct 2008:
;          - Removed shortcircuit logical operators for compatibility with older versions
; B. Laurent, OVParis, 8/2012
; 			Simplified to process a single image (previous version was 2.8.2)
; S Erard, 9/2012: 
;			Fixes, consistency check, + uses v_ojbpds
;		  Won't stop if window objects present (but assumes they are defined _after_ the image)
;		  Now supports suffix and prefix, return them in keywords
;		  Fixed Noscale (not changed in output)
;		  Now supports Display Directions (default is IDL natural order, different from readpds) 
;		 	+ added kw NOROTATE
;-
;
;###########################################################################


  On_error, 0                    ;2 =Return to user    0= debug
							
; If there is no input file name, abort:

  if N_params() LT 2 then begin		
     print,'Syntax - result = V_IMAGEPDS(filename,label,img[,/NOSCALE,/SILENT])'
    return, -1
  endif
  
; Save the input parameters:

  fname   = filename 
  noscl = keyword_Set(NOSCALE)
  norotate = keyword_Set(NOROTATE)
  silent  = keyword_set(SILENT)

; Checkout image pointer
  if (img.pointer EQ '') then begin
     if ~(SILENT) then message, /cont, 'ERROR - No pointer to image data found in '+img.name
     return, -1
  endif

  
  VIRTISH = 0
  VIRTISOUPS = 0
  Instru = v_pdspar(label,'INSTRUMENT_NAME')
  If strmid(strupcase(instru(0)),1,8) EQ 'VIRTIS_H' then VIRTISH = 1
  Date = (v_pdspar(label,'PRODUCT_CREATION_TIME', count = Nc))(0)
  If Nc EQ 0 then if ~(SILENT) then message, fname+' missing PRODUCT_CREATION_TIME keyword', /cont $
   else begin 
    annee = fix(strmid(date,0,4))
    If annee LE 2000 and VIRTISH EQ 1 then VIRTISOUPS=1
   endelse

; ...RECORD_BYTES...

  record_bytes = long(v_pdspar(label,'RECORD_BYTES'))
  if !ERR EQ -1 then begin
     Print, 'ERROR - '+fname+' missing required RECORD_BYTES keyword'
     Print, 'ERROR - '+fname+'Trying 512...'     ; useful in some cases
     record_bytes = 512
   endif

; ...IMAGE parameters (these should appear once for each image, thus the 
;    various COUNTs returned should always be equal):

  label_img = label[img.start:img.stop]
  
  Xvar = v_pdspar( label_img,'line_samples',COUNT=xcount,INDEX=x_ind) 
  Yvar = v_pdspar( label_img,'lines',COUNT=ycount,INDEX=y_ind)
  if xcount(0) NE ycount(0) then begin
     if ~(SILENT) then message, /cont, 'ERROR - '+fname+': LINE_SAMPLES and LINES count discrepancy.'
     return, -1
  endif


  bitpix = v_pdspar( label_img, 'SAMPLE_BITS',COUNT=pixes,INDEX=pix_ind)
  smp_type = v_pdspar(label_img, 'sample_type', COUNT=smpcount, INDEX=smp_ind)
  if pixes(0) NE smpcount(0) then begin
     if ~(SILENT) then message, 'ERROR - '+fname+': SAMPLES_TYPE and SAMPLE_BITS count discrepancy.'
     return, -1
  endif

  Bmask = v_pdspar(label_img,'SAMPLE_BIT_MASK',COUNT=maskcount,INDEX=bmk_ind, /nonum)

  bscale = float(v_pdspar(label_img, 'scaling_factor',INDEX=scl_ind))
  if (scl_ind(0) eq -1) then bscale = 1.
  bzero  = float(v_pdspar(label_img, 'offset', INDEX=zer_ind))
  if (zer_ind(0) eq -1) then bzero = 0.
  if (bzero(0) EQ 0. and bscale(0) EQ 1.) then Noscl =1

    ; sample/line display direction
    ; Use UP as default to perform no rotation if kw are absent
    ddval = ["UP", "DOWN", "LEFT", "RIGHT"]
    sdd = v_pdspar( label_img,'SAMPLE_DISPLAY_DIRECTION',COUNT=Precount,INDEX=Preind)
;        if strupcase(sdd) NE "LEFT" then sdd = "RIGHT"
        temp = where (strupcase(sdd) eq ddval, count)
        If count EQ 0  then sdd = "RIGHT"

    ldd = v_pdspar( label_img,'LINE_DISPLAY_DIRECTION',COUNT=Precount,INDEX=Preind)
;        if strupcase(ldd) NE "UP" then ldd = "DOWN"
        temp = where (strupcase(ldd) eq ddval, count)
        If count EQ 0  then ldd = "UP"

    ; line prefix/suffix bytes:
    prefix_bytes = v_pdspar( label_img,'LINE_PREFIX_BYTES',COUNT=Precount,INDEX=Preind)
	    if precount  EQ 0 then prefix_bytes = long(0)
    suffix_bytes =  v_pdspar( label_img,'LINE_SUFFIX_BYTES',COUNT=Precount,INDEX=Preind)
    	if precount  EQ 0 then suffix_bytes = long(0)

  images = xcount

  xp = x_ind                    ; LINE_SAMPLES
  yp = y_ind                    ; LINES
  bp = pix_ind                  ; SAMPLE_BITS
  sp = smp_ind                  ; SAMPLE_TYPE
  ;sfp= scl_ind                  ; SCALING_FACTOR
  ;zp = zer_ind                  ; OFFSET
  Bm = bmk_ind                  ; BIT MASK
  

  if xp GT -1 AND yp GT -1 AND bp GT -1 AND sp GT -1 then begin
		; existence has been checked just above
    X = (long(Xvar))(0)	; if WINDOWS are present, assume they are defined _after_ the image !
    Y = (long(Yvar))(0)
    bitpix = bitpix(0)
    smp_type = smp_type(0)

     
; If we're not running in SILENT mode, we print the array dimensions:
     
     if ~(SILENT) then begin
        if X GT 0 then if Y GT 0 then begin
           text = string(X)+' by'+string(Y)
           message,'Now reading ' + text + ' array',/INFORM    
        endif else begin
           message,fname+" has X or Y = 0, no data array read",/CON
        endelse
     endif
     
; Grab the appropriate value for SAMPLE_BITS and convert it to a scalar:

     bits = v_str2num(bitpix)
     
; Determine the byte ordering by checking the SAMPLE_TYPE value:
     
     sample_type = smp_type
     Stype = sample_type
     
     If VIRTISOUPS then Stype = 'LSB_'+Stype
     
; second argument is the number of bytes, not bits
; fct result (IDL_type) is the type of variable to be used 
     IDL_type = v_typepds(Stype, bits/8, ITYPE = integer_type, $
                          Stype = sample_type)
     
; retrieve bitmask if present
     if bm GT -1 then Mask = Bmask
     
     
     
;==================================================================
      ; Open file, retrieve offset to object

     PtObj =  V_POINTPDS(img.pointer,record_bytes)
     datafile_found = (PtObj.filename NE '')
     if datafile_found NE 0 then begin ; detached label
        
        fname = file_search(PtObj.filename, /fold) ; works from IDL 5.5 and up
        temp = file_info(fname)
                                ; If not found in current directory, try in label directory
        if fname EQ "" or ~(temp.exists) then begin
;          if not(temp.exists) then begin
           DirName = v_getpath(filename, FBname) ; get path to label under IDL ³ 5.4
           fname = file_search(Dirname+PtObj.filename, /fold)
           temp = file_info(fname)
        endif
;          if not(temp.exists) then  message, 'ERROR - Could not re-open '+ PtObj.filename
        if fname EQ "" or ~(temp.exists) then begin		
        	message, 'ERROR - Could not re-open '+ PtObj.filename, /cont
    		return, -1
		endif
 
        
        openr, unit, fname, ERROR=err, /GET_LUN, /Compress
        
     endif else begin           ; attached label
        
        openr, unit, fname, ERROR=err, /GET_LUN, /Compress
        if err NE 0 then begin
           message, 'ERROR - Could not re-open '+fname, /cont
           return, -1
        endif
     endelse

      ;===================================================================
      ; OK, now we're ready to read the image data.  We'll associate the opened
      ; data file unit with an array of the appropriate type 
      ; (retrieved above by v_typepds from bits per pixel, in 'bits', and 
      ;  sample type, in 'sample_type'):

     If IDL_type EQ 0 then begin
		  message, 'Unknown data type', /cont
    	  return, -1
 	 endif
;     element = Make_array(X, Y, Type = IDL_type, /nozero)

     line1 = make_array(X, type = idl_type, /nozero)
     If prefix_bytes NE 0 then $
  	   line2 = {prefix:bytarr(prefix_bytes), image:line1} $
	 else line2 = {image:line1}	 
     If suffix_bytes NE 0 then line2 = create_struct(line2, "suffix", bytarr(suffix_bytes)) 

     element = replicate(line2, Y) 


     point_lun,unit,PtObj.offset
     readu,unit,element
     free_lun, unit

     If prefix_bytes NE 0 then Prefix = element.prefix
     If suffix_bytes NE 0 then Suffix = element.suffix
     Element = element.image
	 

      ; Convert to local architecture:

     CASE sample_type OF
        'MSB': V_swapData, element, SILENT = silent
        'LSB': V_swapData, element, /LSB, SILENT = silent
        'IEEE': V_swapData, element, SILENT = silent
        'PC': V_swapData, element, /LSB, SILENT = silent
        'VAX': v_vaxtoIEEE, element ; always floats
        else: begin 
           message,'WARNING - Unrecognized SAMPLE_TYPE ('+smp_type+'), no conversion performed', /INF
        end
     ENDCASE

     
      ; Performs bit masking before conversions if required
      ; (may cause problems with unconventional IDL types)    

     if bm(0) GT -1 then begin
        element = v_bmaskpds( element, mask)
     endif
     
     
      ; If the native data type is one unsupported by IDL, we need to allocate
      ; new space and perform the appropriate conversion.  Unsupported types
      ; known so far include signed bytes, unsigned integers, and unsigned
      ; long integers:

     ; Convert signed bytes, not an IDL type 

     if (IDL_type EQ 1 AND  integer_type EQ 'SIGNED') then begin

        ; Allocate an array of 2-byte integers to hold the final values:

        element = fix(element)
        fixitlist = WHERE(element GT 127)
        if fixitlist[0] GT -1 then begin
           element[fixitlist] = element[fixitlist] - 256
        endif
        
     endif 
     

     ; Perform conversion to unsigned integers in IDL versions < 5.2

     if (!version.release LT 5.2) then begin
        
        if (IDL_type EQ 2  AND integer_type EQ 'UNSIGNED') then begin
           
           element = long(element)
           fixitlist = WHERE(element LT 0)
           if fixitlist[0] GT -1 then begin
              element[fixitlist] = element[fixitlist] + 65536
           endif
           
        endif else if (bits EQ 32  AND  integer_type EQ 'UNSIGNED') then begin

        ; These must be converted to real numbers.  In order to preserve as
        ; much precision as possible, we convert to double-precision reals:
          ; (long64 are not defined in IDL 5.2)

           element = double(element)
           fixitlist = WHERE(element LT 0.D0)
           if fixitlist[0] GT -1 then begin
              element[fixitlist] = element[fixitlist] + 4.294967296D+9
           endif
           
        endif
        
     endif

      ; Now we scale the data we've read in using the corresponding
      ; SCALING_FACTOR and OFFSET values from the label, unless the user
      ; has indicated /NOSCALE:


     if ~NOSCL then begin
;        if sfp(0) GT -1 then begin
           scl = bscale(0)
           if scl NE 1.0 then element = temporary(element)*scl
;        endif
        
;        if zp(0) GT -1 then begin
           zero = bzero(0)
           if zero NE 0 then element = temporary(element)+zero
;        endif
     endif
     
      ; End of processing for one image (obsolete)
     
  endif
	
	; process orientation keywords - won't stop if undefined
    	
    if ~NOROTATE then begin	

;		if (sdd eq "RIGHT") then begin
;		       element = (ldd eq "UP") ? rotate(element, 0) : $
;		          (ldd eq "DOWN") ? rotate(element, 7) : -1 
;		   endif else if (sdd eq "LEFT") then begin
;		       element = (ldd eq "UP") ? rotate(element, 5) : $
;		          (ldd eq "DOWN") ? rotate(element, 2) : -1
;		   endif else if (sdd eq "UP") then begin
;		       element = (ldd eq "LEFT") ? rotate(element, 3) : $
;		          (ldd eq "RIGHT") ? rotate(element, 4): -1
;		   endif else if (sdd eq "DOWN") then begin
;		       element = (ldd eq "LEFT") ? rotate(element, 6) : $
;		          (ldd eq "RIGHT") ? rotate(element, 1) : -1
;		   endif

		if (sdd eq "RIGHT") then begin
		       Drot = (ldd eq "UP") ? 0 : $		;IDL basic orientation, OK
		          (ldd eq "DOWN") ?  7 : -1 
		   endif else if (sdd eq "LEFT") then begin
		       element = (ldd eq "UP") ?  5 : $
		          (ldd eq "DOWN") ?  2 : -1
		   endif else if (sdd eq "UP") then begin
		       element = (ldd eq "LEFT") ?  3 : $
		          (ldd eq "RIGHT") ?  4: -1
		   endif else if (sdd eq "DOWN") then begin
		       element = (ldd eq "LEFT") ?  6 : $
		          (ldd eq "RIGHT") ?  1 : -1
		   endif
		   ; Now preserve output if rotation not clear
		   If Drot GT 0 then element = rotate(element, Drot)
		   		
		   if (Drot eq -1) then $
		       if ~(SILENT) then message, "Invalid LINE_DISPLAY_DIRECTION " + ldd + $
		              " and SAMPLE_DISPLAY_DIRECTION " + sdd + " combination", /CONT

    endif


  ; Close the input unit:

  close,unit
  free_lun,unit

; Return array

  return, element  
  
end 
