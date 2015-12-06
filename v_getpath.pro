Function V_GetPath, filename, fname

;+ $Id: v_getpath.pro,v 1.6 2013/11/05 13:28:52 sophie Exp $
; NAME:
;	V_GetPath
;
; PURPOSE:
;	Extract directory and filename from a string
;
; CALLING SEQUENCE:
;	Dirname = V_GetPath(filename, Fname)
;
; INPUTS:
;          Filename = full path to the file to be read
;
; OUTPUTS:
;	result = Directory name + final separator ("dir/" under Unix)
;               Empty if not present in input string
;           Fname = File name alone in output
;
; COMMENT:
;          This function works in IDL ≥ 5.4, VMS not supported
;          A simpler way exists in IDL 6.0, do used for support of older versions
;
; EXAMPLES:
;          DirName = v_getpath(filename, Fname)
;
; MODIFICATION HISTORY:
;	Written: Stephane Erard, LESIA, LESIA 2007
;	Fix for Windows paths on IDL 6.2, SE, 23/2/2007
;	Fix for spaces in Mac paths (as translated when drag/drop on terminal), SE, 5/9/2012
;
;
;###########################################################################
;
; LICENSE
;
;  Copyright (c) 1999-2008, Stéephane Erard, CNRS - Observatoire de Paris
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
;--------------------------------------------------------------------


; Equivalent in IDL 6.0 and up:
;      Dname = File_DirName(filename, /mark)
;      Fname = File_BaseName(filename)

      CASE !version.os_family OF
       'unix' :Pathsep="/"
       'MacOS' :Pathsep =":"
       'Windows' :Pathsep ="\"
       ELSE: Pathsep =""     ; assumes data in the same directory
      ENDCASE


      dir = ''
      Dim = strsplit(filename, pathsep, /extract)
        count = (size(dim))(1)
        Fname = Dim(count-1)
        If count GT 1 then $
          Dname = strmid(filename, 0, strlen(filename)-strlen(Fname))$
        else Dname =""
          
        If strmid(!version.os_name, 0, 6) EQ 'Mac OS' then begin
          bouts = STRSPLIT(Dname,'\\ ', /EXTRACT, /RegEx) 
          Dname = StrJoin(bouts, ' ')
		endif
		
     Return, Dname

End