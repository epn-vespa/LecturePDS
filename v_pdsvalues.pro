function V_PDSVALUES, label, keyword, abort, Vstructure = Vstruct, COUNT=matches, INDEX = nfound, $
     NAMESPACE = namespace, NONUMERIC = Nonumeric, NODELIMITER = Nodelimiter  

;
; NAME:
;          V_PDSVALUES
;
; PURPOSE:
;     Returns tables of keywords/values from a PDS label, to be analysed further
;	  Will save time when parsing many kw + makes analysis of a PDS archive easy
;
; CALLING SEQUENCE:
;     result = V_PDSVALUES( lbl, kw, abort, MVAL=, COUNT=, INDEX =, NAMESPACE =])
;
; INPUTS:
;     Lbl =  PDS label array, (e.g. as returned by V_HEADPDS or V_READPDS)  
;          string array, may be multiline
;
;
; OUTPUTS:
;     Result = Table of values in the label, reformatted, strings
;     keyword = Table of corresponding keywords present in the header
;
;
; KEYWORDS:
;     ABORT - string specifying that V_PDSPAR should do a RETALL
;          if a parameter is not found.  ABORT should contain
;          a string to be printed if the keyword parameter is not found.
;          If not supplied V_PDSPAR will return with a negative
;          !err if a keyword is not found.
;
;     COUNT - Nb of actual keywords present in the label
;
;     INDEX - Returns the line numbers of actual kw in the input PDS label.
;
;     {MVAL - Optional keyword to return the value of requested keyword 
;          that exceeds the normal allowable string size that can be 
;          printed by the PRINT function. The 'zeroth' record of MVAL-
;          MVAL(*,0) contains the number of following records that 
;          contain meaningful information.
;			— Removed from input parameters, unused (but left in code)}
;
;     Vstructure - Returns values with proper type in a structure. 
;          Tags = numerical order in label, to allow for redundant keywords
;
;     NAMESPACE - Namespaces are prefices ending in ":" that introduce 
;           instrument-specific keywords (e.g., ROSETTA:). 
;           Normally, the search does not include the namespace (i.e., it is 
;           filtered from both the search string 'name' and the label 'lbl').
;           When this option is set, the match must include the possible 
;           namespace.
;
;     NONUMERIC - If set, returns string as is, does not extract numerical value.
;            Mostly useful to: 
;				- preserve units after values (default is to return the 
;            first numerical value found in string).
;		     	- inhibit complex-type interpretation when processing 
;			 2-elt vectors (axis_items in arrays...)
;
;     NODELIMITER - If set, removes string delimiters (double quotes) around a
;			 _single_ value before possible numeric convertion (i.e., if NONUM not set)
;            Intended to filter extra quotes in VIRTIS Rosetta labels in (numeric) kw such 
;				as PROCESSING_LEVEL_ID (ESA specs are different from VIRTIS VEx)
;			 Arrays of strings can be processed one elt at a time after reading:
;				 help, v_listpds('("2","3")') 		; value returned by v_pdspar
;			     <Expression>    STRING    = Array[2]
;				help, v_str2Nnum((v_listpds('("2","3")'))(0), /ext)
;			     <Expression>    INT       =        2
;
;
; SIDE EFFECTS:
;          !err is set to -1 if parameter not found.
;
;
; EXAMPLES:
;
;   lab = v_headpds('T1_00237397602.CAL')
;	sd = v_pdsvalues(lab, kw, Vstruct= Vst, COUNT=matches, INDEX = nfound, /NODELIM)
;		; keywords and corresponding typed values
;	for ii = 0, matches-1 do print, ii,' ',  kw(ii), '   ', vst.(ii)
;
; PROCEDURE:
;     First removes blank lines and comments from label.
;     Each element of lbl is searched for a '= ' and the part of each 
;     that preceeds the '= ' is saved in the variable 'keyword', with 
;     any line that contains no '= ' also saved in keyword. 
;     Namespaces are filtered from both 'Name' and 'keyword'.
;     Spaces are removed.
;     'keyword' is then searched for elements that exactly match 'Name'. 
;     If search succeeds then returns following characters and possibly 
;     next lines, if they do not contain a keyword. An error occurs if search fails.
;     String values are converted to numeric values, if possible, 
;     by the V_STR2NUM function (if /NONUMERIC not set)
;
;    
; NOTE:
;	  Will extract numeric value only if first char is a number
;		 - but will preserve dates and biblio ref as strings
;	  Does not support '=' in comments. Tested with multiline examples
;
;  *** IMPORTANT COMMENT ***:
;     Last line of the object label cannot be parsed correctly in general for some reason.
;	  Whenever needed (if object contains sub-objects, eg TABLEs), add an extra line to 
;	  object labels from v_objdef for use with v_parpds.
;
;
; MODIFICATION HISTORY:
;     Adapted by John D. Koch from SXPAR by DMS, July, 1994 
;	v_pdspar: 
;     Modified for VIRTIS, Stephane Erard, IAS, Oct. 1999
;     Updated from SBNIDL 2.0, Stephane Erard, Sept. 2000
;     SE, LESIA, Oct 2013: Filter string delimiters (on option only) to support new Rosetta specs
;	v_pdsvalues: 
;     SE, LESIA, Nov 2013, adapted from v_pdspar:
;		Parses a complete label and returns everything for future analysis
;     SE, LESIA, April 2017, for PDS spectral lib:
;		 Edited in l 185 to search for  '= ' instead of  ' = ' (may be removed afterward ?)
;-
;
;###########################################################################
;
; LICENSE
;
;  Copyright (c) 1999-2013, Stéphane Erard, CNRS - Observatoire de Paris
;  All rights reserved.
;  Non-profit redistribution and non-profit use in source and binary forms, 
;  with or without modification, are permitted provided that the following 
;  conditions are met:
; 
;        Redistributions of source code must retain the above copyright
;        notice, this list of conditions, the following disclaimer, and
;        all the modifications history.
;        Redistributions in binary form must reproduce the above copyright
;        notice, this list of conditions, the following disclaimer and all the
;        modifications history in the documentation and/or other materials 
;        provided with the distribution.
;        Neither the name of the CNRS and Observatoire de Paris nor the
;        names of its contributors may be used to endorse or promote products
;        derived from this software without specific prior written permission.
; 
; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND ANY
; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE REGENTS AND CONTRIBUTORS BE LIABLE FOR ANY
; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
;----------------------------------------------------------------------

!ERR=0
 params = N_params()
 if params LT 2 then begin
     print,'Syntax - result = v_pdsvalues(lbl,values [,abort,MVAL=,COUNT=,INDEX=,NAMESPACE = ])'
     return, -1
 endif 

 value = 0
 if params LE 2 then begin
      abort_return = 0
      abort = 'PDS Label'
 endif else abort_return = 1
if abort_return then On_error,1 else On_error,2     ; return


;       Check for valid header + remove unused lines

  lbl = label
  s = size(lbl)
  if ( s(0) NE 1 ) or ( s(2) NE 7 ) then $
        message,abort+' (first parameter) must be a string array'

; removes empty line (begins with spaces + 10b)
;   + comment line (begins with spaces + '/*') — SE, Dec 2005
; (mandatory to parse values starting with an empty line)
temp = where(strmid(strtrim(lbl,1), 0, 1) EQ string(10b) $
     or strmid(strtrim(lbl,1), 0, 2) EQ '/*', comp=indiceO)
lbl = lbl(indiceO)



;     Loop on lines of the header 

 Sep = '= '					; SE2012 : try to match '=' with no space, need to filter extra space (see below) - TBC
 key_end = strpos(lbl,Sep)			;look for ' = ' in all lines of lbl
 r = size(key_end)
 stopper = r(r(r(0)-1))
 keyword = strarr(stopper)

 for j = 0,stopper-2 do begin 
;    if key_end(j) LT 0 then keyword(j) = '*' $ 
;       else keyword(j)=strcompress(strmid(lbl(j),0,key_end(j)),/rem)
    if key_end(j) LT 0 then begin 
          keyword(j) = '*'                                                                 ; FIX for end_object, SE Feb2006
          if strcompress(strmid(lbl(j),0,strlen(lbl(j))-1),/rem) EQ 'END_OBJECT' then $
               keyword(j) = 'END_OBJECT'
       endif else keyword(j)=strcompress(strmid(lbl(j),0,key_end(j)),/rem)
   if NOT(keyword_set(namespace)) then begin
     temp = (strsplit(keyword(j), ':', /extract))
     if size(temp, /dim) GT 1 then keyword(j) = temp(1)
   endif
 endfor
matches = stopper-1			; pretend to be looking for a kw (will return the total nb)
nfound = indgen(stopper-1)	; pretend to found it on every line




; Process string parameter and use V_STR2NUM to obtain numeric value

    line = lbl(nfound)
    nfd = size(nfound)
    quitter = nfd(nfd(nfd(0)-1))
    svalue = strarr(quitter)
    mvalue = strarr(quitter,100)
    value = svalue
    lsep = strlen(Sep)
    i = 0

    while i LT quitter do begin      ; loops on occurrences of keyword
      n = nfound(i)
      knd = key_end(n)
      retrn = strpos(line(i),string(10b))
      if retrn EQ -1 then retrn = 80
      svalue(i) = strmid(line(i),knd+lsep,retrn-knd-lsep+1)
      svalue(i) = strmid(line(i),knd+lsep,retrn-knd-lsep)
;      spot = strpos(svalue(i),string(10b))
;      if spot GT 0 then svalue(i)=strmid(svalue(i),0,spot)      ; removes EOL on one byte
      spot = strpos(svalue(i),'/*')         ; removes comments in line, SE, Oct 2003
      if spot GT 0 then svalue(i)=strmid(svalue(i),0,spot)


; Process multiline lists, SE oct 2003 - Dec 2005
; if starts with an empty line
      j = 0
      if (strcompress(svalue(i), /rem)) EQ string(0b) then begin
          If  keyword(n+1) NE '*' then begin     ; no value present
            !ERR =-1
             return, value
          endif
       svalue(i) = lbl(n+1)          ; to next line
       spot = strpos(svalue(i),string(10b))
       if spot GT 0 then svalue(i)=strmid(svalue(i),0,spot)      ; removes EOL on one byte
       spot = strpos(svalue(i),'/*')         ; removes comments in line
       if spot GT 0 then svalue(i)=strmid(svalue(i),0,spot)        
       j = 1
      endif

; append following lines with no keyword
     j = j+1
     while (keyword(n+j) EQ '*') do begin
        nxtline = lbl(n+j)
               ; deletes final LF before merging
        svalue(i) = svalue(i) + strcompress(strmid(nxtline,0,strlen(nxtline)-1))
        j = j+1
     endwhile

; Process string values
	  svalue(i)= strtrim(svalue(i),2)	; SE2012: remove leading space possibly left after separator + trailing spaces
      check = strpos(svalue(i),'"')	; first quote
      if check GT -1 then begin      ; [removes extra spaces (very old fashioned)]
         k = n
         c=strpos(svalue(i),'"',check+1)	; next quote
         if c GT -1  then value(i)=strcompress(svalue(i),/rem) else begin
              for a = 0,key_end(n)+1  do value(i)=value(i)+' '
              value(i) = value(i) + svalue(i)
         endelse
         m = 0
         m2 = 0
         while c LT 0 do begin	; [older processing for multiple lines? -apparently no longer used]
            k = k+1
           m = m+1
           m2=fix(m/24)
           if m2 EQ 0 then value(i)=value(i) + ' ' + lbl(k) $
                else if m2 GT 0 then mvalue(i,m2)=mvalue(i,m2)+' '+lbl(k) $
                else print,'Illegal value of variable m2 ='+m2            
           c = strpos(lbl(k),'"')
;         if (c GT -1) then if(keyword(k+1) EQ '*') then c = -1 
         endwhile 
         mvalue(i,0)=fix(m2(0))
      endif else $
          If not(keyword_set(Nonumeric)) then $
          svalue(i) = strcompress(svalue(i),/rem)
      i = i + 1
    endwhile
    
    
; filter quotes around a numeric value on option, SE2013
; (as a pre-processing, to handle all optional cases)
	If keyword_set(NODELIMITER) then begin
		 for i=0, quitter-1 do begin
		  bid = (strsplit(svalue(i), '"', /extract, count=nbid))(0)
		  if nbid EQ 1 then svalue(i) = bid
		 endfor
	Endif

    temp = v_str2num(svalue(N_elements(svalue)-1), type= stype) ; SE2012: use last element's type (for integers, in case they are long)
;    temp = v_str2num(svalue(0), type= stype) ; older
    value = Make_array(quitter,Type = stype)
    If keyword_set(Nonumeric) then value = svalue $
     else for i=0, quitter-1 do value(i) = v_str2num(svalue(i))	; added extraction to remove units, SE2013
    if quitter EQ 1 then value = value(0)     ; scalar

; Return index to line numbers in input label
     nfound = indiceO(nfound)    
; Remove final END in kw list
     keyword = keyword(0:stopper-2)
; Clean up extra lines
     value(where(keyword EQ '*')) = ' '
     Svalue(where(keyword EQ '*')) = ' '
     
;     Vstruct = create_struct(keyword(quitter-2), temp)	; pb with duplicate kw!
     temp = v_str2num(svalue(quitter-1), type = stype, /ext, num = num)
     Vstruct = create_struct(string(quitter-1), temp)
     for i=quitter-2, 0, -1 do begin
        temp = str2num(strmid(svalue(i), 0,1), type=Ftype)
        ext= Ftype EQ 7 ? 0 : 1 				; convert only if first char = number
		if strpos(keyword(i), 'TIME') GE 0 then ext = 0 	; will not convert PDS times 
		if strpos(keyword(i), 'REFERENCE_KEY_ID') GE 0 then ext = 0	; ...or biblio ref
;		print, svalue(i),' ', temp, ext, Ftype, '@'
     	temp = v_str2num(svalue(i), type = stype, ext = ext, num = ext)	; num to extract with no reserve
;	    Vstruct = create_struct(keyword(i), temp, Vstruct)
	    Vstruct = create_struct(string(i), temp, Vstruct)
     endfor

;              Tout cela ne vaut pas le poison qui découle
;                       De tes yeux, de tes yeux verts,
;              Lacs où mon âme tremble et se voit à l'envers...
;                        Mes songes viennent en foule
;                  Pour se désaltérer à ces gouffres amers.



return,value
END
