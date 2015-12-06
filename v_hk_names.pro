function v_hk_names, $
	instrument

;+ $Id: v_hk_names.pro,v 1.5 2010/09/27 15:11:00 erard Exp $
;
; NAME:
;  v_hk_names
;
; PURPOSE:
;  Returns an array containing all the housekeeping names for a given instrument
;  (Virtis-H, Virtis-M-IR or Virtis-M-V)
;  These names correspond to the housekeepings values returned by the function v_pdshk
;
; CALLING SEQUENCE:
;  result=v_hk_names(instrument)
;
; INPUTS:
;  instrument = name of the instrument
;
; OUTPUTS:
;  Result = string array containing all the housekeeping names
;
; MODIFICATION HISTORY:
;  Written by Florence HENRY, dec. 2005
;-
;
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


case strlowcase(instrument) of
	"virtis-h" : vh = 1
	"virtis-m-ir" : vh = 0
	"virtis-m-v" : vh = 0
	"virtis-m" : vh = 0
	else : return, ['']
endcase

if (vh eq 1) then $
	data = ['data SCET',$
		'Acquisition ID',$
		'# of subslices',$
		'1st serial #',$
		'Data Type__Real science/Dummy',$
		'Data Type__Spectrum type',$
		'Data Type__Shutter state',$
		'Data Type__Compression mode',$
		'Data Type__Average/summing mode',$
		'Data Type__Image type',$
		'ME_default HK SCET',$
		'V_MODE__V-M operative mode',$
		'V_MODE__V-H operative mode',$
		'V_MODE__ME operative mode',$
		'ME_PWR_STAT__-M power converter status',$
		'ME_PWR_STAT__-H power converter status',$
		'ME_PWR_STAT__-M IFE +5V power status',$
		'ME_PWR_STAT__-H IFE +5V power status',$
		'ME_PWR_STAT__ADC power status',$
		'ME_PWR_STAT__EEPROM +5V power status',$
		'ME_PWR_STAT__DPU_ID',$
		'ME_PS_TEMP',$
		'ME_DPU_TEMP',$
		'ME_DHSU_VOLT ',$
		'ME_DHSU_CURR',$
		'EEPROM_VOLT ',$
		'IFE_ELECTR_VOLT',$
		'H_ME_general HK SCET',$
		'H_ECA_STAT__-H ECA status',$
		'H_ECA_STAT__-H ECA power',$
		'H_COOL_STAT__-H cooler mode',$
		'H_COOL_STAT__-H cooler motor drv status',$
		'H_COOL_STAT__-H CCE +28V power',$
		'H_COOL_TIP_TEMP',$
		'H_COOL_MOT_VOLT',$
		'H_COOL_MOT_CURR',$
		'H_CCE_SEC_VOLT',$
		'H_HK_report SCET',$
		'HKRq_Int_Num',$
		'HKRq_Bias',$
		'HKRq_I_Lamp',$
		'HKRq_I_Shutter',$
		'HKRq_PEM_Mode',$
		'HKRq_Test_Init',$
		'HK_Rq_Device/On__HK_Rq_Det/On',$
		'HK_Rq_Device/On__HK_Rq_Shutter/On',$
		'HK_Rq_Device/On__HK_Rq_FPAHtr/On',$
		'HK_Rq_Device/On__HK_Rq_Lamp_Spect_T/On',$
		'HK_Rq_Device/On__HK_Rq_Lamp_Spect_S/On',$
		'HK_Rq_Device/On__HK_Rq_Lamp_Radio/On',$
		'HK_Rq_Device/On__HK_Rq_Temp_Det/On',$
		'HK_Rq_Device/On__HK_Rq_Status_Shutter/On',$
		'HK_Rq_Device/On__HKMS_Req_during_Acq/On',$
		'HKRq_Cover__HKRq_Cover_Dir',$
		'HKRq_Cover__HKRq_Cover_Wave',$
		'HKRq_Cover__HKRq_Cover_Status',$
		'HKRq_Cover__HKRq_Cover_Step',$
		'HKMs_Status__HKMs_ADC_Latchup',$
		'HKMs_Status__HKMs_Shutter/Closed',$
		'HKMs_Status__HKMs_Shutter/Open',$
		'HKMs_Status__FPGA_HES_1_H',$
		'HKMs_Status__FPGA_HES_2_H',$
		'HKMs_Status__HKMs_Annealing_Limit_Flag',$
		'HKMs_V_Line_Ref',$
		'HKMs_Vdet_Dig',$
		'HKMs_Vdet_Ana',$
		'HKMs_V_Detcom',$
		'HKMs_V_Detadj',$
		'HKMs_V+5',$
		'HKMs_V+12',$
		'HKMs_V+21',$
		'HKMs_V-12',$
		'HKMs_Temp_Vref',$
		'HKMs_Det_Temp',$
		'HKMs_Gnd',$
		'HKMs_I_Vdet_Ana',$
		'HKMs_I_Vdet_Dig',$
		'HKMs_I_+5',$
		'HKMs_I_+12',$
		'HKMs_I_Lamp',$
		'HKMs_I_Shutter/Heater ',$
		'HKMs_Temp_Prism',$
		'HKMs_Temp_Cal_S',$
		'HKMs_Temp_Cal_T ',$
		'HKMs_Temp_Shut',$
		'HKMs_Temp_Grating',$
		'HKMs_Temp_Objective',$
		'HKMs_Temp_FPA',$
		'HKMs_Temp_PEM ',$
		'HKDH_Last_Sent_Request',$
		'HKDH_Stop_Readout_Flag'] $
else $
	data = ['data SCET',$
	'Acquisition ID',$
	'# of subslices',$
	'1st serial #',$
	'Data Type__Real science/Dummy',$
	'Data Type__Spectrum type',$
	'Data Type__Shutter state',$
	'Data Type__Compression mode',$
	'Data Type__Average/summing mode',$
	'Data Type__Image type',$
	'ME_default HK SCET',$
	'V_MODE__V-M operative mode',$
	'V_MODE__V-H operative mode',$
	'V_MODE__ME operative mode',$
	'ME_PWR_STAT__-M power converter status',$
	'ME_PWR_STAT__-H power converter status',$
	'ME_PWR_STAT__-M IFE +5V power status',$
	'ME_PWR_STAT__-H IFE +5V power status',$
	'ME_PWR_STAT__ADC power status',$
	'ME_PWR_STAT__EEPROM +5V power status',$
	'ME_PWR_STAT__DPU_ID',$
	'ME_PS_TEMP',$
	'ME_DPU_TEMP',$
	'ME_DHSU_VOLT ',$
	'ME_DHSU_CURR',$
	'EEPROM_VOLT ',$
	'IFE_ELECTR_VOLT',$
	'M_ME_general HK SCET',$
	'M_ECA_STAT__-M ECA status',$
	'M_ECA_STAT__-M ECA power',$
	'M_COOL_STAT__-M cooler mode',$
	'M_COOL_STAT__-M cooler motor drv status',$
	'M_COOL_STAT__-M CCE +28V power',$
	'M_COOL_TIP_TEMP',$
	'M_COOL_MOT_VOLT',$
	'M_COOL_MOT_CURR',$
	'M_CCE_SEC_VOLT',$
	'MVIS_HK_report SCET',$
	'M_CCD_VDR_HK ',$
	'M_CCD_VDD_HK',$
	'M_+5_VOLT ',$
	'M_+12_VOLT',$
	'M_-12_VOLT',$
	'M_+20_VOLT',$
	'M_+21_VOLT',$
	'M_CCD_LAMP_VOLT',$
	'M_CCD_TEMP_OFFSET',$
	'M_CCD_TEMP',$
	'M_CCD_TEMP_RES',$
	'M_RADIATOR_TEMP',$
	'M_LEDGE_TEMP',$
	'OM_BASE_TEMP',$
	'H_COOLER_TEMP',$
	'M_COOLER_TEMP',$
	'M_CCD_WIN_X1',$
	'M_CCD_WIN_Y1',$
	'M_CCD_WIN_X2',$
	'M_CCD_WIN_Y2 ',$
	'M_CCD_DELAY',$
	'M_CCD_EXPO',$
	'M_MIRROR_SIN_HK',$
	'M_MIRROR_COS_HK',$
	'M_VIS_FLAG_ST__CCD scan flag',$
	'M_VIS_FLAG_ST__H/K acquisition flag',$
	'M_VIS_FLAG_ST__time error flag',$
	'M_VIS_FLAG_ST__word error flag',$
	'M_VIS_FLAG_ST__VIS checkout ADC latch-up status',$
	'M_VIS_FLAG_ST__last command to CCD calibration lamp',$
	'MIR_HK_report SCET',$
	'M_IR_VDETCOM_HK ',$
	'M_IR_VDETADJ_HK',$
	'M_IR_VPOS',$
	'M_IR_VDP',$
	'M_IR_TEMP_OFFSET',$
	'M_IR_TEMP',$
	'M_IR_TEMP_RES',$
	'M_SHUTTER_TEMP',$
	'M_GRATING_TEMP ',$
	'M_SPECT_TEMP',$
	'M_TELE_TEMP',$
	'M_SU_MOTOR_TEMP',$
	'M_IR_LAMP_VOLT',$
	'M_SU_MOTOR_CURR',$
	'M_IR_WIN_Y1 ',$
	'M_IR_WIN_Y2',$
	'M_IR_DELAY',$
	'M_IR_EXPO',$
	'M_IR_LAMP_SHUTTER__last current value of IR calib. lamp',$
	'M_IR_LAMP_SHUTTER__last command to IR calib. Lamp',$
	'M_IR_LAMP_SHUTTER__last current value of shutter',$
	'M_IR_LAMP_SHUTTER__last command to shutter',$
	'M_IR_FLAG_ST_IRFPA scan flah',$
	'M_IR_FLAG_ST__H/K acquisition flag',$
	'M_IR_FLAG_ST__time error flag',$
	'M_IR_FLAG_ST__IR word error flag',$
	'M_IR_FLAG_ST__scan word error flag',$
	'M_IR_FLAG_ST__IR detector status flag',$
	'M_IR_FLAG_ST__IR detector ADC latch-up status',$
	'M_IR_FLAG_ST__annealing heater last received cmd',$
	'M_IR_FLAG_ST__last cover command direction',$
	'M_IR_FLAG_ST__close position HES1',$
	'M_IR_FLAG_ST__open position HES2']

return, data

end
