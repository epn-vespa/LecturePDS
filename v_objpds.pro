function V_OBJPDS, lbl, Otype, ALL= all,  SILENT = silent

;+ 
;
; NAME:
;	V_OBJPDS
;
; PURPOSE:
;	Return object descriptions from a PDS label
;
; CALLING SEQUENCE:
;	Result= V_OBJPDS(lbl, Otype [,/ALL] [,/SILENT] )
;
; INPUTS:
;   Lbl =  PDS label array, (e.g. as returned by V_HEADPDS or V_READPDS)  
;	Otype = Std PDS object name, scalar string (e.g.,"IMAGE", "QUBE", but also 'RED_IMAGE', etc...)
;                         (not used if option ALL is set)
;                        
;
; OUTPUTS:
;	Result = Structure with fields: 
;               .start
;               .stop = start and stop lines of object definition in label
;               .ptline = label line of pointer to the object, integer
;               .pointer = pointer to the object, string (value of .ptline)
;               .type = generic type of object, string (e.g., 'IMAGE')
;               .name = specific name of object, string (unique in the label, ie 'RED_IMAGE')
;               .category = 1 if main object (image/qube/table or derived/array/collection), 
;							0 if sub-object (others, including columns: no pointer to a data object)
;           Result is a vector with dimension = number of objects of this type
;           Result = 0 (scalar) if this object type is not present in label
;
; OPTIONAL KEYWORDS:
;	ALL - returns line numbers for all objects present, with support for sub-objects
;                    (Otype not required if set)
;	SILENT - suppresses terminal messages
;
; EXAMPLE:
;                   Otype = 'IMAGE'
;                   Obj_lines =  V_OBJPDS(lbl, Otype)
;                   Nobj = size(obj_lines, /dim)      ; number of objects found
;                   print, lbl(Obj_lines(0).start:Obj_lines(0).stop)     ; first image definition in label
;
;                   print, lbl(Obj_lines(0).ptline)     ; print label line with first image pointer
;                   record_bytes = long(v_pdspar(label,'RECORD_BYTES'))
;                   print,  v_pointpds(Obj_lines(0).pointer,record_bytes)     ; print pointer to first image
;
;
; COMMENTS:
;          1) It is assumed that general PDS rules are honored (not always true...):
;     - All PDS objects appear with the same type name as pointer and in object definition area 
;        (e.g. ^BROWSE_IMAGE should correspond to OBJECT = BROWSE_IMAGE)
;
;          2) Embedded objects are included in main object definition (e.g. COLUMN inside TABLE)
;          When parsing the main object properties, sub-objects may have to be filtered first.
;
;          3) Sub-objects are not associated with a PDS pointer (e.g., no pointer for COLUMN).
;          Result.Ptline associated with sub-objects equals -1 (to be checked upon return)
;          Result.Pointer associated with sub-objects is a null string

;	- 2012: 
;		- option ALL is OK, but when not set Otype must = the object NAME (not type)
;       - It is possible that PDS objects of a given type are expected to appear in the same 
;			order as pointer and object definition (But there is no reason for this. Would require 
;			a fix if there are misfunctionnings with the /ALL option)
;		- WINDOW cannot be processed as a special IMAGE type, since is does not contain a pointer.
;			This one is a PDS summum!
;
; MODIFICATION HISTORY:
;       Written: Stephane Erard, LESIA, Jan 2006
;            SE, Feb 2006: renamed tags, added pointer itself in structure for future use
;            SE, Feb 2007: handles object types that are not always associated to a pointer (ARRAY)
;            SE, Sept 2012: added extra fields for TYPE / CATEGORY
;            SE, Nov 2012: added dirty fix for VIMS � can't read the files anyway (inconsistent labels)
;-
;
;###########################################################################
;
; LICENSE
;
;  Copyright (c) 1999-2008, St�phane Erard, CNRS - Observatoire de Paris
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


;==================================================================
 
  On_error,2                  ;2 =Return to user    0= debug

If not(keyword_set(ALL)) then All = 0 

If not ALL then begin 
  Otype = strupcase(Otype)
  Pointlist = v_pdspar(lbl, '^'+Otype)          ; check PDS pointers
  if !ERR EQ -1 then begin     ; exit if no pointer to this object is found
     result = 0
     !ERR =-1
     return, result
  endif
endif

; Comm : number of pointers may be different from number of object definitions
; (e.g., COLUMN objects are not associated with a pointer)

object = v_pdspar(lbl,'OBJECT',COUNT=objects,INDEX=obj_ind)
if !ERR EQ -1 then message, $
        'ERROR - '+' missing required OBJECT keyword'
If not ALL then Type_ind = obj_ind(where(object EQ Otype)) $
     else Type_ind = obj_ind

Npt = (size(type_ind, /Dim))(0)     ; Number of objects found
If size(type_ind, /N_Dim) EQ 0 then Npt =1
result = {objList2, start:1L, stop:1L, ptline:1L, pointer:' ', type:' ', name:' ', category:0L}
if Npt GT 0 then result = replicate(result, Npt)
;result.count = Npt
result.start = Type_ind
If not ALL then result.type = Otype else result.type = object	; to be adjusted if /ALL
result.name = object	; OK


Eobject = v_pdspar(lbl,'END_OBJECT',COUNT=Eobjects,INDEX=Eobj_ind)
if Eobjects NE  objects then message, $
        'ERROR - '+' OBJECT and END_OBJECT do not match'
If not ALL then Type_ind = Eobj_ind(where(Eobject EQ Otype)) $
   else begin                                  ; must reorder END_OBJECTs 
     temp = Eobj_ind
     For i = Npt-1, 0, -1 do begin     ; dirty, but works
          j = min(where(temp GT Obj_ind(i)))
          Type_ind (i) = temp(j)
          temp(j) =0
     Endfor
   endelse

result.stop = Type_ind

bid1 = strarr(Npt)
bid = ' '
res = intarr(Npt)
for i = 0, Npt-1 do begin
   if res(i) EQ 0 then begin
     w = where(result(i).type EQ result.type)	; here, type = name (not necessarily unique)
     bid = v_pdspar(lbl,'^'+result(i).type, index= temp, count= ct)
     ; handles object types that are not always associated to a pointer (ARRAY)
     If ct LT N_elements(w) then begin
          temp0=lonarr(N_elements(w))
          temp0(0)=temp
          temp = temp0
          bid0=strarr(N_elements(w))
          bid0(0)=bid
          bid = bid0
     endif
;     print, temp
     result(w).ptline = temp
     result(w).pointer = bid
     res = res OR (result(i).type EQ result.type)     ; mark objects already found
   endif
;print, result.pointer, w
endfor

; identifies main objects to be handled, including derivatives/associated
; (composite type names should appear last in the array, ie browse_image after image)
Artype= ['IMAGE', 'QUBE', 'TABLE', 'SERIES', 'PALETTE', 'SPECTRUM', 'COLLECTION', 'ARRAY', 'HEADER']	
; generic type
Outtype= ['IMAGE', 'QUBE', 'TABLE', 'TABLE', 'TABLE', 'TABLE', 'COLLECTION', 'ARRAY', 'HEADER']
for ii = 0, (size(Artype, /dim))(0)-1 do begin
 for jj = 0, Npt-1 do begin
  if strmid(result(jj).type, strlen(artype(ii))-1,/rev) EQ artype(ii) then begin
  	result(jj).type = Outtype(ii)
  	result(jj).category = 1	; main object
  endif
 endfor
endfor

; should also remove encapsulated collection or array objects, currently done in v_readpds
; should also remove ALIAS?


; Dirty fix for VIMS 
for jj = 0, Npt-1 do begin
 If result(jj).name EQ 'SPECTRAL_QUBE' and result(jj).pointer EQ '' then $
	     result(jj).pointer = v_pdspar(lbl,'^QUBE', index= temp, count= ct)
endfor



Return, result

End
