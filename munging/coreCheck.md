Variable | Description | Type | Valid | Prop. Valid | Coded Missing/Inconsistent/Invalid | Blank
:--------|:-----------|:----|:-----|:-------------|:--------------------------------|:-----
AGE | Age in years at admission | integer | 7800241 | 0.9987 | 10521 | 0
AGEDAY | Age in days (when age < 1 year) | integer | 745054 | 0.0954 | 7065708 | 0
AMONTH | Admission month | integer | 7118184 | 0.9113 | 692578 | 0
ASOURCE | Admission source (uniform) | integer | 1686970 | 0.216 | 6123792 | 0
ASOURCEUB92 | Admission source (UB-92 standard coding) | character | 637901 | 0.0817 | 0 | 7172861
ASOURCE_X | Admission source (as received from source) | character | 1697685 | 0.2174 | 0 | 6113077
ATYPE | Admission type | integer | 6891588 | 0.8823 | 919174 | 0
AWEEKEND | Admission day is a weekend | integer | 7808284 | 0.9997 | 2478 | 0
DIED | Died during hospitalization | integer | 7805171 | 0.9993 | 5591 | 0
DISCWT | Weight to discharges in AHA universe | double | 7810762 | 1 | 0 | 0
DISPUB04 | Disposition of patient (UB-04 standard coding) | integer | 6743095 | 0.8633 | 1067667 | 0
DISPUNIFORM | Disposition of patient (uniform) | integer | 7805171 | 0.9993 | 5591 | 0
DQTR | Discharge quarter | integer | 7793565 | 0.9978 | 17197 | 0
DQTR_X | Discharge quarter (as received from source) | integer | 7793565 | 0.9978 | 17197 | 0
DRG | DRG in effect on discharge date | integer | 7810762 | 1 | 0 | 0
DRG24 | DRG, version 24 | integer | 7810762 | 1 | 0 | 0
DRGVER | DRG grouper version used on discharge date | integer | 7810762 | 1 | 0 | 0
DRG_NoPOA | DRG in use on discharge date, calculated without POA | integer | 7810762 | 1 | 0 | 0
DSHOSPID | Data source hospital identifier | character | 5273821 | 0.6752 | 0 | 2536941
ELECTIVE | Elective versus non-elective admission | integer | 7792360 | 0.9976 | 18402 | 0
FEMALE | Indicator of sex | integer | 7789677 | 0.9973 | 21085 | 0
HCUP_ED | HCUP Emergency Department service indicator | integer | 7810762 | 1 | 0 | 0
HOSPBRTH | Indicator of birth in this hospital | integer | 7810762 | 1 | 0 | 0
HOSPID | HCUP hospital identification number | double | 7810762 | 1 | 0 | 0
HOSPST | Hospital state postal code | character | 7810762 | 1 | 0 | 0
KEY | HCUP record identifier | double | 7810762 | 1 | 0 | 0
LOS | Length of stay (cleaned) | double | 7807930 | 0.9996 | 2832 | 0
LOS_X | Length of stay (as received from source) | double | 7795819 | 0.9981 | 14943 | 0
MDC | MDC in effect on discharge date | integer | 7810762 | 1 | 0 | 0
MDC24 | MDC, version 24 | integer | 7810762 | 1 | 0 | 0
MDC_NoPOA | MDC in use on discharge date, calculated without POA | integer | 7810762 | 1 | 0 | 0
MDNUM1_R | Physician 1 number (re-identified) | double | 4393918 | 0.5625 | 3416844 | 0
MDNUM2_R | Physician 2 number (re-identified) | double | 2632070 | 0.337 | 5178692 | 0
NCHRONIC | Number of chronic conditions | integer | 7810762 | 1 | 0 | 0
NDX | Number of diagnoses on this record | integer | 7810762 | 1 | 0 | 0
NECODE | Number of E codes on this record | integer | 7810762 | 1 | 0 | 0
NEOMAT | Neonatal and/or maternal DX and/or PR | integer | 7810762 | 1 | 0 | 0
NIS_STRATUM | Stratum used to sample hospital | integer | 7810762 | 1 | 0 | 0
NPR | Number of procedures on this record | integer | 7810762 | 1 | 0 | 0
ORPROC | Major operating room procedure indicator | integer | 7810762 | 1 | 0 | 0
PAY1 | Primary expected payer (uniform) | integer | 7794273 | 0.9979 | 16489 | 0
PAY1_X | Primary expected payer (as received from source) | character | 7797978 | 0.9984 | 0 | 12784
PAY2 | Secondary expected payer (uniform) | integer | 2417777 | 0.3095 | 5392985 | 0
PAY2_X | Secondary expected payer (as received from source) | character | 2715692 | 0.3477 | 0 | 5095070
PL_NCHS2006 | Patient Location: NCHS Urban-Rural Code (V2006) | integer | 7638129 | 0.9779 | 172633 | 0
PRDAY1 | Number of days from admission to PR1 | integer | 4484538 | 0.5741 | 3326224 | 0
PRDAY2 | Number of days from admission to PR2 | integer | 2424696 | 0.3104 | 5386066 | 0
PRDAY3 | Number of days from admission to PR3 | integer | 1403850 | 0.1797 | 6406912 | 0
PRDAY4 | Number of days from admission to PR4 | integer | 831793 | 0.1065 | 6978969 | 0
PRDAY5 | Number of days from admission to PR5 | integer | 533432 | 0.0683 | 7277330 | 0
PRDAY6 | Number of days from admission to PR6 | integer | 366597 | 0.0469 | 7444165 | 0
PRDAY7 | Number of days from admission to PR7 | integer | 209700 | 0.0268 | 7601062 | 0
PRDAY8 | Number of days from admission to PR8 | integer | 137447 | 0.0176 | 7673315 | 0
PRDAY9 | Number of days from admission to PR9 | integer | 86585 | 0.0111 | 7724177 | 0
PRDAY10 | Number of days from admission to PR10 | integer | 57989 | 0.0074 | 7752773 | 0
PRDAY11 | Number of days from admission to PR11 | integer | 36806 | 0.0047 | 7773956 | 0
PRDAY12 | Number of days from admission to PR12 | integer | 25610 | 0.0033 | 7785152 | 0
PRDAY13 | Number of days from admission to PR13 | integer | 18042 | 0.0023 | 7792720 | 0
PRDAY14 | Number of days from admission to PR14 | integer | 12900 | 0.0017 | 7797862 | 0
PRDAY15 | Number of days from admission to PR15 | integer | 9691 | 0.0012 | 7801071 | 0
PointOfOriginUB04 | Point of origin for admission or visit, UB-04 standard coding | character | 6072058 | 0.7774 | 0 | 1738704
PointOfOrigin_X | Point of origin for admission or visit, as received from source | character | 6094337 | 0.7802 | 0 | 1716425
RACE | Race (uniform) | integer | 6614593 | 0.8469 | 1196169 | 0
TOTCHG | Total charges (cleaned) | double | 7717057 | 0.988 | 93705 | 0
TOTCHG_X | Total charges (as received from source) | double | 7706768 | 0.9867 | 103994 | 0
TRAN_IN | Transfer in indicator | integer | 7789029 | 0.9972 | 21733 | 0
YEAR | Calendar year | integer | 7810762 | 1 | 0 | 0
ZIPINC_QRTL | Median household income national quartile for patient ZIP Code | integer | 7575371 | 0.9699 | 235391 | 0
