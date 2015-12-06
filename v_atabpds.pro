;------------------------------------------------------------------------------
;	Reads a PDS Ascii table object into an IDL structure.
;   Reads a single table identified by pointer and label lines
;
;	Result = V_ATABPDS (filename, label, objindex, [/SILENT, C_NAME=sfdsfd])
;	     (if detached label, this is the label file)
;	Label = String array containing the PDS label itself (returned by v_headpds)
;           object = Object definition limits in label + data pointer (returned by v_objpds)
;
; OUTPUTS:
;            .columnN = N vectors containing the PDS table (can be of various types)
;
; KEYWORDS:
;   SILENT: suppresses any messages from the procedure
;               Obj_def =  V_OBJPDS(label, /all)
;               Obj_num = (size(obj_def, /dim))(0)      ; number of objects found
;               data = V_ATABPDS(filename, label, Obj_def(i), SILENT = silent)
;
; COMMENTS (2012 version):
;       Requires correct label formatting (ROW_BYTES must include line terminator, CR-LF)
;          + correct data types in the label (!)
;       Does not require the last data record to be completed
;       Does not require column names
;       STREAM mode: each line is a record in this case (with variable length) 
;		Returns all floats in double precision... (heritage from tascpds V3)
;
;
; MODIFICATION HISTORY:
;	 TASCPDS: 06 Jan 2004, P. Khetarpal (version from SBNPDS 3)
;	 V_ATABPDS:
;  Stephane Erard, LESIA, Sept 2012: New version derived from SBNPDS V3 referred above
;		(implements multicolumn ascii tables) - minimal adaptations here to fit v_readpds library and I/O
;		+ Modifications to v_btabPDS (including STREAM mode support) transferred here brutally 
;		Original version could not work in multicolumn mode, fixed
;		Always return column_names as first tag (not present in previous version), not an issue 
;			(this is also written outside the structure by v_readpds)
;  SE, LESIA, Oct 2012: 
;		Removed remnant spaces from data type before analysis (was blocking some datasets)
;		Now support table prefix/suffix, return them in the output structure (final CR-LF may remain in suffix)
;		Fixed a nasty bug in multi column elements 
;  SE, LESIA, Dec 2012: 
;		Now check column name number (returns if different from COLUMNS kw)
;			 - may be difficult to handle for calling routine, but at least the error is clear
;  SE, LESIA, Jan 2013:
;		Fixed STREAM mode + avoid conversion error (string to byte) with some IDL versions
;


   
   
; object definition area in label
labobj = label(object.start:object.stop)
labobj = [labobj, '   ']	; extra line for future use with v_parpds
   start_ind = object.start
      
; -- end Origin
 if !ERR EQ -1 then begin
   message,'ERROR - '+filename+': missing required INTERCHANGE_FORMAT keyword', /cont
   return, -1
 endif else begin
   inform = inform(0)
   infst =  strpos(inform,'"') 		; remove '"'s from inform 
   if infst GT -1 then $			
	inform = strmid(inform,infst+1,strpos(inform,'"',infst+1)-infst-1)
   if inform EQ "BINARY" then message, $
	'ERROR- '+filename+' is a binary table file; try BTABPDS.'
 endelse
 record_bytes = v_pdspar(label,'RECORD_BYTES')	
; if !ERR EQ -1 then message, $
;        'ERROR - '+fname+' missing required RECORD_BYTES keyword'
 if !ERR EQ -1 then record_bytes= 1     ; default (should not be used...)

 
   
      return, -1
      return, -1
      return, -1
      return, -1
   endif
      return, -1
	
   endif
   
   it_offset_flag = 1 ; (default value for later - was missing, TBC @SE2012)
   
       ; Look for row prefix/suffix bytes in table object only (@SE2012)
    prefix_bytes = v_pdspar( labobj,'ROW_PREFIX_BYTES',COUNT=Precount,INDEX=Preind)
	    if precount  EQ 0 then prefix_bytes = long(0)
    suffix_bytes =  v_pdspar( labobj,'ROW_SUFFIX_BYTES',COUNT=Precount,INDEX=Preind)
    	if precount  EQ 0 then suffix_bytes = long(0)

   ; Check for stream mode files	@SE2012
Stream = 0
rec_type=v_pdspar(label,'RECORD_TYPE')
; Comment: uses RECORD_TYPE(0) - may not be OK if there are several FILE Objects in the label
if size(rec_type,/n_el) gt 1 then begin
	rec_type2=v_pdspar(label,'RECORD_TYPE')
	if rec_type2[0] ne '' then rec_type=rec_type2[0] else rec_type=rec_type[0]
endif
IF (strupcase(rec_type) EQ 'STREAM') then stream = 1


      
            	message, $
                 return, -1
            endif else $
            	message, $
                 return, -1
            endif

   for j = 0L, g_name_count-1 do begin
   
  
	; strange... and inefficient if count is completly off
   while name_ind[0] LT Y_ind[ypos[0]] do begin
 If cols GT size(name, /dim) then begin
    message, 'ERROR - Column name number mismatch ', /cont
	columns = name
    return, -1
 endif
 columns = name(0:cols-1)


	datafile_found = (PtObj.filename NE '')
	if datafile_found NE 0 then begin     ; if detached label, look for file location
     fname = file_search(PtObj.filename, /fold)        ; works from IDL 5.5 and up
     temp = file_info(fname)
; If not found in current directory, try in label directory
     if fname EQ "" or not(temp.exists) then begin
      DirName = v_getpath(filename, FBname)     ; get path to label under IDL ≥ 5.4
      fname = file_search(Dirname+PtObj.filename, /fold)	  
      temp = file_info(fname)
     endif
     if fname EQ "" or not(temp.exists) then begin
     	 message, 'ERROR - Could not re-open '+ PtObj.filename, /cont
     	 return, -1
     endif
    endif

    	return, -1
      endelse
   
   
;print, X, Y, prefix_bytes,suffix_bytes


 if skip NE 0 then begin
  bidon = strarr(skip)
  readf, unit, bidon
 endif
 filedata = strarr(Y)
 readf, unit, filedata
 free_lun, unit

endif else begin

    filedata=bytarr(X+prefix_bytes+suffix_bytes,Y,/NOZERO)
    point_lun,unit,skip
    readu,unit,filedata
   close, unit
   
endelse

	
   
   ;help, filedata
;	separate prefix and suffix from data (@SE2012)
	If prefix_bytes NE 0 then begin
	 prefix = filedata(0:prefix_bytes-1, *)
	 filedata = filedata(prefix_bytes:*, *)
	endif
	If suffix_bytes NE 0 then begin
	 suffix = filedata(X:*, *)
	 filedata = filedata(0:X-1, *)
	endif
;help, filedata

;print, lf+1, X+prefix_bytes+suffix_bytes-1
      type = strtrim(type,2) ; remove extra spaces
         new = make_array(nitem,Y,/STRING,VALUE=" ")
            pos = 0
			      temp = strsplit(new[i,j], '"(),' , /ext)

	If prefix_bytes NE 0 then data = CREATE_STRUCT (data, 'prefix',prefix)
   if not (SILENT) then help, /STRUCTURE, data
