Variable | Description | Type | Valid | Prop. Valid | Coded Missing/Inconsistent/Invalid | Blank
:--------|:-----------|:----|:-----|:-------------|:--------------------------------|:-----
AHAID | AHA hospital identifier with the leading 6 | character | 595 | 0.5667 | 0 | 455
DISCWT | Weight to discharges in AHA universe | double | 1050 | 1 | 0 | 0
HFIPSSTCO | Hospital FIPS state/county code | double | 595 | 0.5667 | 455 | 0
H_CONTRL | Control/ownership of hospital | integer | 1035 | 0.9857 | 15 | 0
HOSPADDR | Hospital address from AHA Survey (Z011) | character | 575 | 0.5476 | 0 | 475
HOSPCITY | Hospital city from AHA Survey (Z012) | character | 575 | 0.5476 | 0 | 475
HOSPID | HCUP hospital identification number | double | 1050 | 1 | 0 | 0
HOSPNAME | Hospital name from AHA Survey (Z000) | character | 575 | 0.5476 | 0 | 475
HOSPST | Hospital state postal code | character | 1050 | 1 | 0 | 0
HOSPSTCO | Hospital modified FIPS state/county code | double | 595 | 0.5667 | 455 | 0
HOSPWT | Weight to hospitals in AHA universe | double | 1050 | 1 | 0 | 0
HOSPZIP | Hospital ZIP Code from AHA Survey (Z014) | character | 575 | 0.5476 | 0 | 475
HOSP_BEDSIZE | Bed size of hospital (STRATA) | integer | 1035 | 0.9857 | 15 | 0
HOSP_CONTROL | Control/ownership of hospital (STRATA) | integer | 1035 | 0.9857 | 15 | 0
HOSP_LOCATION | Location (urban/rural) of hospital | integer | 1035 | 0.9857 | 15 | 0
HOSP_LOCTEACH | Location/teaching status of hospital (STRATA) | integer | 1035 | 0.9857 | 15 | 0
HOSP_REGION | Region of hospital (STRATA) | integer | 1050 | 1 | 0 | 0
HOSP_TEACH | Teaching status of hospital | integer | 1035 | 0.9857 | 15 | 0
IDNUMBER | AHA hospital identifier without the leading 6 | character | 595 | 0.5667 | 0 | 455
NIS_STRATUM | Stratum used to sample hospital | integer | 1050 | 1 | 0 | 0
N_DISC_U | Number of AHA universe discharges in NIS_STRATUM | double | 1050 | 1 | 0 | 0
N_HOSP_U | Number of AHA universe hospitals in NIS_STRATUM | integer | 1050 | 1 | 0 | 0
S_DISC_U | Number of sample discharges in NIS_STRATUM | double | 1050 | 1 | 0 | 0
S_HOSP_U | Number of sample hospitals in NIS_STRATUM | integer | 1050 | 1 | 0 | 0
TOTAL_DISC | Total number of discharges from this hospital in the NIS | double | 1050 | 1 | 0 | 0
YEAR | Calendar Year | integer | 1050 | 1 | 0 | 0
HOSP_RNPCT | Percentage of RN among licensed nurses-H | integer | 954 | 0.9086 | 96 | 0
HOSP_RNFTEAPD | RN FTEs per 1000 adjusted patient days-H | double | 957 | 0.9114 | 93 | 0
HOSP_LPNFTEAPD | LPN FTEs per 1000 adjusted patient days-H | double | 965 | 0.919 | 85 | 0
HOSP_NAFTEAPD | Nurse aides per 1000 adjusted patient days-H | double | 960 | 0.9143 | 90 | 0
HOSP_OPSURGPCT | Percentage of all surgeries performed in outpatient setting-H | integer | 964 | 0.9181 | 86 | 0
HOSP_MHSMEMBER | Hospital is part of multiple hospital system-H | integer | 940 | 0.8952 | 110 | 0
HOSP_MHSCLUSTER | AHA multiple hospital system cluster code-H | integer | 518 | 0.4933 | 532 | 0
