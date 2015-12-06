function V_BMASKPDS, Data, Mask, bit_mask = bmask

;+ $Id: v_bmaskpds.pro,v 1.9 2013/11/05 13:18:54 sophie Exp $
;
; NAME:
;	V_BMASKPDS
;
; PURPOSE:
;	Apply bit masking on data
;
; CALLING SEQUENCE:
;	result = V_BMASKPDS( data, mask)
;
; INPUTS:
;	Data =  Data object to be processed (integers only)
;	Mask = String describing the mask to be applied, from PDS keyword SAMPLE_BIT_MASK
;                Of the form: "2#1111111111000000#" (scalar string)
;
; OUTPUTS:
;	result = processed data object
;
; KEYWORDS:
;           BIT_MASK: return applied bit mask in a variable
;
; SIDE EFFECTS:
;
; EXAMPLE:
;
;	IDL> mask =v_pdspar(lbl, 'SAMPLE_BIT_MASK', /nonum)     ; parse bit masks
;	IDL> mask = mask(1)                   ; select element of interest (for current object)
;	IDL> data = v_bmaskpds( data ,mask)   ; perform masking
;
; PROCEDURE:
;          Shifts bits to first non zero bit in mask (does not skip other unset bits so far).
;          Only implemented for binary masks (hexa masks also exists).
;          No support for negative values, should be OK.
;    
; NOTE:
;    Bitmasks have syntax bb#nnnnnn…# where:
;       bb is the base ID (can be 2, 16…) 
;       ## are field delimiters
;       nnn… is the bitmask info: 1 if bit is set, O if not (given in specified base),
;          MSB first (TBC). 
;    Read bitmak string using v_pdspar with option /Nonum to preserve whole string,
;   in case it is not delimited by quotes.
;    Masking must be performed after data swap, and before type conversion.
;
; MODIFICATION HISTORY:
;     Stephane Erard, LESIA, Oct. 2005: written
;     S Erard, LESIA, 8/2012: removed possible spaces from string first, would prevent conversion
;
;-
;###########################################################################
;
; LICENSE
;
;  Copyright (c) 1999-2008, Stéphane Erard, CNRS - Observatoire de Paris
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

On_error, 2                    ;2 =Return to user    0= debug

params = N_params()
 if params LT 2 then begin
     print,'Syntax - result = v_bmaskpds(data,mask)'
     return, -1
 endif 

type=size(Data,/type)
if ~ ((type ge 1 and type le 3) or (type ge 12 and type le 15)) then begin
	message, 'Bit masking only available for integers', /cont
	return, data
endif

dmask=v_str2num(strtrim(Mask,2))
if(size(dmask ,/type) eq 7) then begin
	message,'Bad mask',/cont
	return,data
endif


return,(data and dmask)

END
