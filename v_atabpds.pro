;------------------------------------------------------------------------------; NAME: V_ATABPDS;; PURPOSE: 
;	Reads a PDS Ascii table object into an IDL structure.
;   Reads a single table identified by pointer and label lines
;; CALLING SEQUENCE: 
;	Result = V_ATABPDS (filename, label, objindex, [/SILENT, C_NAME=sfdsfd]);; INPUTS:;    Filename: Scalar string containing the name of the PDS file to read
;	     (if detached label, this is the label file)
;	Label = String array containing the PDS label itself (returned by v_headpds)
;           object = Object definition limits in label + data pointer (returned by v_objpds)
;
; OUTPUTS:;	Result = Structure with fields:
;            .columnN = N vectors containing the PDS table (can be of various types)
;
; KEYWORDS:;	C_name: returns column names in a vector 
;   SILENT: suppresses any messages from the procedure;; EXAMPLES:;	Read a single Ascii table, the i-th object in the label, in a variable:
;               Obj_def =  V_OBJPDS(label, /all)
;               Obj_num = (size(obj_def, /dim))(0)      ; number of objects found
;               data = V_ATABPDS(filename, label, Obj_def(i), SILENT = silent)
;;
; COMMENTS (2012 version):
;       Requires correct label formatting (ROW_BYTES must include line terminator, CR-LF)
;          + correct data types in the label (!)
;       Does not require the last data record to be completed
;       Does not require column names
;       STREAM mode: each line is a record in this case (with variable length) 
;		Returns all floats in double precision... (heritage from tascpds V3)
;
;
; MODIFICATION HISTORY:;    Adapted by John D. Koch from READFITS by Wayne Landsman, December,1994
;	 TASCPDS: 06 Jan 2004, P. Khetarpal (version from SBNPDS 3)
;	 V_ATABPDS:;       Stephane Erard, LESIA, Feb 2006 Written (derived from tascpds v2, 1 item / column)
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
;;------------------------------------------------------------------------------function V_ATABPDS, filename, label, object, SILENT=silent, C_name = columns

   ON_ERROR, 0   ; check for number of parameters in function call:   if N_params() LT 3 then begin      print, 'Syntax: Result = TASCPDS (filename,label,objindex[,/SILENT])'      return, -1   endif   if keyword_set(SILENT) then silent = 1 else silent = 0
   
   
; object definition area in label
labobj = label(object.start:object.stop)
labobj = [labobj, '   ']	; extra line for future use with v_parpds
   start_ind = object.start   end_ind = object.stop fname = filename ; -- Origin   ; obtain viable objects:;   objects = GET_VIABLE(label,"ALL");   objpos = where(objindex EQ objects.index);   if objpos[0] NE -1 then begin;      objarray = objects.array[objpos[0]];   endif else begin;      print, "Invalid objindex specified.";      return, -1;   endelse   ; set object pointers for current object:;   start_ind = objindex;   if objpos[0]+1 EQ n_elements(objects.array) then begin;      labelsize = size(label);      end_ind = labelsize[1];   endif else $;      end_ind = objects.index[objpos[0]+1]
      
; -- end Origin   ; Check for valid interchange format:    inform = v_pdspar( labobj, 'INTERCHANGE_FORMAT' )     
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

 
      ; obtain required keywords for the object:   columns_num = v_PDSPAR (label,'COLUMNS',INDEX=col_ind)   if !ERR EQ -1 then begin      message, 'Error: missing required COLUMNS keyword.', /cont
      return, -1   endif    glob_X = v_PDSPAR (label, 'ROW_BYTES',INDEX=X_ind)   if !ERR EQ -1 then begin      message, 'Error: missing required ROW_BYTES keyword.', /cont
      return, -1   endif   glob_Y = v_PDSPAR (label,'ROWS',INDEX=Y_ind)   if !ERR EQ -1 then begin      message, 'Error: missing required ROWS keyword.', /cont
      return, -1   endif   ; obtain information for each column object:   glob_name = v_PDSPAR (label, 'NAME',COUNT=g_name_count, INDEX=glob_name_ind)   if !ERR EQ -1 then begin      message, 'Error: missing required NAME keywords', /cont   endif   gdata_type = v_PDSPAR (label,'DATA_TYPE',COUNT=gdata_count,INDEX=gdata_ind);   data_type = v_PDSPAR (label,'DATA_TYPE',COUNT=data_count,INDEX=data_ind)	; first try   if !ERR EQ -1 then begin message, $      'Error: missing required DATA_TYPE keywords.', /cont
      return, -1
   endif   length = v_PDSPAR (label,'BYTES',COUNT=byte_count,INDEX=byte_ind)   if !ERR EQ -1 then begin message, $      'Error: missing required BYTES keywords.', /cont
      return, -1   endif 
	   start_byte = v_PDSPAR (label,'START_BYTE',COUNT=strt_count,INDEX=strt_ind)-1   if !ERR EQ -1 then begin message, $      'Error: missing required START_BYTE keywords.', /cont
   endif   ; obtain parameter positions for current object:   colpos = where(col_ind GT start_ind AND col_ind LT end_ind)   xpos = where(X_ind GT start_ind AND X_ind LT end_ind)   ypos = where(Y_ind GT start_ind AND Y_ind LT end_ind)   namepos = where(glob_name_ind GT start_ind AND glob_name_ind LT end_ind)   datapos = where(gdata_ind GT start_ind AND gdata_ind LT end_ind)   ; specify params for this object:   cols = fix(v_STR2NUM(columns_num[colpos[0]]))   X = long(glob_X[xpos[0]])   Y = long(glob_Y[ypos[0]])   end_data_pos = n_elements(datapos)-1   data_type = gdata_type[datapos[0]:datapos[end_data_pos]]      data_ind = gdata_ind[datapos[0]:datapos[end_data_pos]]   data_count = n_elements(data_type)
   
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


         ; look for items in column objects:   arrays = 0   currentitemflag = -1   items = v_PDSPAR (label,'ITEMS',COUNT=arrays,INDEX=is_ind)   if !ERR GT -1 then begin      currentitem = where(is_ind GT start_ind AND is_ind LT end_ind)      if currentitem[0] NE -1 then begin         currentitemflag = 1         ; process ITEM_BYTES:         item_bytes = v_PDSPAR (label,'ITEM_BYTES',COUNT=iarrays,INDEX=ib_ind)         if !ERR GT -1 then begin            if iarrays NE arrays then begin               print, 'ERROR: ITEMS count and ITEM_BYTES count discrepancy'               return, -1            endif            itempos = where(ib_ind GT start_ind AND ib_ind LT end_ind)            end_item_pos = n_elements(itempos)-1            item_bytes = item_bytes[itempos[0]:itempos[end_item_pos]]            ib_ind = ib_ind[itempos[0]:itempos[end_item_pos]]             length = [temporary(length),item_bytes]            byte_ind = [temporary(byte_ind),ib_ind]            byte_count = byte_count + n_elements(item_bytes)         endif                     ; process ITEM_TYPE and DATA_TYPE:         if data_count[0] LT cols then begin            item_type = v_PDSPAR (label,'ITEM_TYPE',COUNT=iarrays,INDEX=it_ind)            if !ERR EQ -1 then begin 
            	message, $                 'ERROR: missing required ITEM_TYPE keyword' , /cont
                 return, -1
            endif else $            if iarrays NE arrays then begin 
            	message, $                 'ERROR: ITEMS count and ITEM_TYPE count discrepancy', /cont
                 return, -1
            endif            itempos = where(it_ind GT start_ind AND it_ind LT end_ind)            end_item_pos = n_elements(itempos)-1               item_type = item_type[itempos[0]:itempos[end_item_pos]]            it_ind = it_ind[itempos[0]:itempos[end_item_pos]]            data_type = [temporary(data_type),item_type]            data_ind = [temporary(data_ind),it_ind]            data_count = data_count + n_elements(item_type)         endif         ; process ITEM_OFFSET:         it_offset = v_PDSPAR (label,'ITEM_OFFSET',COUNT=ioffcnt,INDEX=ioffind)         if !ERR EQ -1 then begin            it_offset_flag = 0         endif else begin;            ioffpos = where(ioffcnt GT start_ind AND ioffcnt LT end_ind)            ioffpos = where(ioffind GT start_ind AND ioffind LT end_ind)	; that was a nasty one :)            if ioffpos[0] LT 0 then begin               it_offset_flag = 0            endif else begin               end_ioffpos = n_elements(ioffpos)-1               if end_ioffpos GT 0 then begin                  it_offset = it_offset[ioffpos[0]:ioffpos[end_ioffpos]]                  ioffind = ioffind[ioffpos[0]:ioffpos[end_ioffpos]]               endif            endelse         endelse      endif    endif   ; clean names and data_types:;   param = ['"',"'","(",")"];   for j = 0, g_name_count-1 do begin;      glob_name[j] = CLEAN (glob_name[j]);      glob_name[j] = REMOVE (glob_name[j], param)                  ;   endfor;   for j = 0, data_count-1 do begin;      data_type[j] = CLEAN (data_type[j], /SPACE);      data_type[j] = REMOVE (data_type[j], param);      temp = strsplit(data_type[j], '_',/EXTRACT);      if n_elements(temp) GT 1 then $;         data_type[j] = temp[1];   endfor   

   for j = 0L, g_name_count-1 do begin      temp = strsplit(glob_name[j], '"()' , /ext)      glob_name[j] = strjoin(temp)   endfor   for j = 0L, data_count-1 do begin      temp = strsplit(data_type[j], '"()' , /ext)      data_type[j] = strjoin(temp)      temp = strsplit(data_type[j], '_',/EXTRACT)      if n_elements(temp) GT 1 then data_type[j] = temp[1]   endfor   
   
     ; fix names array to remove table name from the array for current object:   count = cols   end_name_pos = n_elements(namepos)-1   name = glob_name[namepos[0]:namepos[end_name_pos]]   name_ind = glob_name_ind[namepos[0]:namepos[end_name_pos]]
	; strange... and inefficient if count is completly off
   while name_ind[0] LT Y_ind[ypos[0]] do begin      name = name[1:count]      name_ind = name_ind[1:count]      count = count - 1   endwhile   ; specify column headings:;   columns = strarr(cols+1);   columns[0] = "columns";   columns[1:cols] = name
 If cols GT size(name, /dim) then begin
    message, 'ERROR - Column name number mismatch ', /cont
	columns = name
    return, -1
 endif columns = strarr(cols)
 columns = name(0:cols-1)

   ; obtain pointer information:	PtObj =  V_POINTPDS(object.pointer,record_bytes);   pointer = POINTPDS (label, fname, objarray)	; Tbchanged	skip = PtObj.offset    ; offset in bytes
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
   ; if not SILENT, then inform user of status: - duplicated below ?   if silent EQ 0 then begin      str = cols*Y      text = strtrim(string(cols),2)+' Columns and '+ strtrim(string(Y),2)+' Rows'      if str GT 0 then begin         message, 'Now reading table with '+text,/INFORM      endif else begin    	message,fname+" has ROWS or COLUMNS = 0, no data read", /cont
    	return, -1
      endelse   endif
   
   
;print, X, Y, prefix_bytes,suffix_bytes   ; read data - no assoc   openr, unit, fname, /GET_LUN, ERROR = err, /compress   if err LT 0 then message,'Error opening file ' + ' ' + fname

if STREAM then begin     ; files in stream mode, pointers are given as line numbers
 if skip NE 0 then begin
  bidon = strarr(skip)
  readf, unit, bidon
 endif
 filedata = strarr(Y)
 readf, unit, filedata
 free_lun, unit

endif else begin
;   status = fstat(unit)   XY = X * Y;   file = assoc(unit, bytarr(XY,/NOZERO),skip);   filedata = file[0]
    filedata=bytarr(X+prefix_bytes+suffix_bytes,Y,/NOZERO)
    point_lun,unit,skip
    readu,unit,filedata
   close, unit   free_lun, unit
   
endelse

	   ; check for end-of-line characters and X dimension:   bad_line_term = 0   cr = where(byte(filedata) EQ 10b, crcount)   lf = where(byte(filedata) EQ 13b, lfcount)   if cr[0] LT 0 then $      if not (SILENT) then print, 'Error in table: no carriage return characters found.' + $             ' Proceeding.'   if lf[0] LT 0 then begin      if not (SILENT) then print, 'Error in table: no line feed characters found. Proceeding.'      goto, FORMATDATA   endif   if NOT (crcount EQ lfcount and total(cr-lf) EQ crcount) then begin      if not (SILENT) then print,'Error in table: Carriage return and line feed should ' + $            'terminate each line. Proceeding.'      bad_line_term = 1   endif
   
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

;print, lf+1, X+prefix_bytes+suffix_bytes-1   ; should never block here if label correctly formatted   if (lf[0]+1 NE X+prefix_bytes+suffix_bytes-1) then begin      if NOT (bad_line_term) then $         if not (SILENT) then print,'Error in label or table: row_bytes keyword set to ' + $               strcompress(X,/re)+'; actual value is '+$               strcompress(lf[0]+1,/re)+'. Proceeding.'      if ~stream then X = lf[0]+1-prefix_bytes-suffix_bytes      goto, FORMATDATA   endif FORMATDATA:   ; formatting data and converting into string array:   if ~stream then  filedata = reform(filedata,X,Y)   table = string(filedata);print, x, y	; CR-LF may remain in suffix at this stage (@SE2012)   ; conversion of string into structure of appropriate column vectors:   data = CREATE_STRUCT ('column_names',columns)   ;/****** start of for loop to store columns data **********/   for k = 0L, cols-1 do begin      vect = 0      col_name = 'column'+strtrim(string(k+1),2)      if k LT cols-1 then begin         startpos = where(strt_ind GT name_ind[k] AND strt_ind LT name_ind[k+1])         typepos = where(data_ind GT name_ind[k] AND data_ind LT name_ind[k+1])         lenpos = where(byte_ind GT name_ind[k] AND byte_ind LT $                        name_ind[k+1],bitenum)         if currentitemflag GT 0 then begin            it = where(is_ind GT name_ind[k] AND is_ind LT name_ind[k+1])           if it_offset_flag NE 0 then $            ioff = where(ioffind GT name_ind[k] AND ioffind LT name_ind[k+1])         endif      endif else begin         startpos = where(strt_ind GT name_ind[k])         typepos = where(data_ind GT name_ind[k])         lenpos = where(byte_ind GT name_ind[k],bitenum)         if currentitemflag GT 0 then begin            it = where(is_ind GT name_ind[k])            if it_offset_flag NE 0 then $               ioff = where(ioffind GT name_ind[k])         endif      endelse      ; if more than one lenpos, then find the smallest and largest      least = lenpos[0]      big = lenpos[0]      for b = 0L, bitenum - 1 do begin         if v_STR2NUM(length[lenpos[b]]) LT v_STR2NUM(length[least]) $            then least = lenpos[b] else big = lenpos[b]      endfor      lenpos = big[0]      start = start_byte[startpos]      bytes = length[lenpos]      vect = strmid(table,start[0],bytes[0])	; ici c'est encore OK      lenpos = least[0]      bytes = length[lenpos]      bytes = v_STR2NUM(bytes[0])            type = data_type[typepos]      type = type[0]
      type = strtrim(type,2) ; remove extra spaces      ; process items:      if currentitemflag GT 0 then if it[0] GT -1 then begin         nitem = long(items[it[0]])         if it_offset_flag NE 0 then $            inc = it_offset[ioff[0]] $         else inc = bytes
         new = make_array(nitem,Y,/STRING,VALUE=" ")         j = 0         j = long(j)         ymaxindex = long(Y)         while j LT ymaxindex do begin            pos = v_STR2NUM(start[0])	; what the fuck ??!
            pos = 0            i = 0            i = long(i)            imaxindex = long(nitem)            while i LT imaxindex do begin	               new[i,j] = strmid(vect[j],pos,bytes)               pos = pos + inc               if type NE 'CHARACTER' AND type NE 'TIME' AND type NE 'DATE' $               then begin                  param = ['"' , "'" , "(" , ")" , ","];                  new[i,j] = REMOVE(new[i,j],param);                  new[i,j] = CLEAN(new[i,j])                 
			      temp = strsplit(new[i,j], '"(),' , /ext)			      new[i,j] = strjoin(temp)               endif               i = i + 1            endwhile            j = j + 1         endwhile         vect = new      endif else begin         ; cleaning individual elements in the columns:         if type NE 'CHARACTER' AND type NE 'TIME' AND type NE 'DATE' $          then begin            z = 0            z = long(z)            number = long(n_elements(vect) -1)            param = ['"' , "'" , "(" , ")" , ","]            while z LE number do begin;               vect[z] = REMOVE(vect[z],param)			   temp = strsplit(vect[z], '"(),' , /ext)			   vect[z] = strjoin(temp)               z = z + 1            endwhile;            vect = CLEANARR(vect,/SPACE)         endif      endelse      ; adding the column values to the table structure:      CASE type OF         'INTEGER': data=CREATE_STRUCT(data,col_name,long(vect))'UNSIGNED_INTEGER': data=CREATE_STRUCT(data,col_name,long(vect))            'REAL': data=CREATE_STRUCT(data,col_name,double(vect))           'FLOAT': data=CREATE_STRUCT(data,col_name,double(vect))       'CHARACTER': data=CREATE_STRUCT(data,col_name,vect)          'DOUBLE': data=CREATE_STRUCT(data,col_name,double(vect))            'BYTE': data=CREATE_STRUCT(data,col_name,long(vect))         'BOOLEAN': data=CREATE_STRUCT(data,col_name,long(vect))            'TIME': data=CREATE_STRUCT(data,col_name,vect)            'DATE': data=CREATE_STRUCT(data,col_name,vect)              else: message, $                      type+' not a recognized data type!'      ENDCASE   endfor

	If prefix_bytes NE 0 then data = CREATE_STRUCT (data, 'prefix',prefix)	If suffix_bytes NE 0 then data = CREATE_STRUCT (data, 'suffix',suffix)  
   if not (SILENT) then help, /STRUCTURE, data
   return, dataend