$PARAM @annotated
TVKe: 1.69: elimination constant (/h)
TVVc: 5.70 : Typical central volume (L)
TVK12: 0.247 : Typical central to peripheral constant
TVK21: 11.43 : Typical peripheral to central constant
TVCCRCL: 0.946 : Typical effect of GFR on ke

$PARAM @annotated @covariates
CREAT : 1 : sérum creatinine en mg/dL
MIC: 1 : CMI value for PK/PD index
SEX : 0 : male
AGE : 60 : in years
BSA : 1.73 : body surface area in m2
WT : 72 : body weight in kg

$OMEGA @ name IIV @annotated
ETA1: 0.0367 : Variance of ETA1 on elimination constant
ETA2: 0.0615 : Variance of ETA2 on central volume
ETA3: 1.11 : Variance of ETA3 on k12
ETA4: 0.194 : Variance of ETA4 on k21
ETA5: 0.0964: Variance of ETA5 on CCRCL

$SIGMA
0.05 // proportional
1 // additive (mg/L)

$CMT @annotated
CENTRAL  : Central compartment (mg/L)[ADM, OBS]
PERIPHERIQUE : Peripheral compartment (mg)
TOVERMIC : Time above minimal inhibitory concentration
AUC_calc : Area under the curve

$MAIN

double A, B;
double sex_factor;

if(SEX == 1) {
  if(CREAT <= 0.7) {
    A = 0.7;
    B = -0.241;
  } else {
    A = 0.7;
    B = -1.2;
  }
  sex_factor = 1.012;
} else {
  if(CREAT <= 0.9) {
    A = 0.9;
    B = -0.302;
  } else {
    A = 0.9;
    B = -1.2;
  }
  sex_factor = 1.0;
}

double CRCL = 142 * pow(CREAT / A, B) * pow(0.9938, AGE) * sex_factor;
double CCRCL = TVCCRCL * exp(ETA5);
double KE1 = TVKe * exp(ETA1);
double K10 = KE1 * pow(CRCL / 64.92, CCRCL);
if(NEWIND <=1) {
  int i = 0;
    capture Vc = TVVc * exp(ETA2);

    while(Vc < 3) {
    if(++i > 100) {
      break;
    }
    simeta();
    Vc = TVVc * exp(ETA2);
  }
}

double K12 = TVK12 * exp(ETA3);
double K21 = TVK21 * exp(ETA4);
double Cmax = 0;

if(NEWIND <= 1 || EVID==1 || EVID==4) {
  Cmax = 0;   // Track Cmax in $DES
}

$ODE
dxdt_CENTRAL   =  K21 * PERIPHERIQUE - (K10 + K12) * CENTRAL;
dxdt_PERIPHERIQUE =  K12 * CENTRAL - K21 * PERIPHERIQUE;
double CP = CENTRAL/Vc;
double free_CP = 0.8*CP;
if(CP > Cmax) Cmax = CP;
double OVERMIC = 0;
if(free_CP>MIC) OVERMIC = 1;
dxdt_TOVERMIC = OVERMIC;
dxdt_AUC_calc = CENTRAL / Vc;

$TABLE
double IPRED = CENTRAL/Vc;                        // Individual prediction (mg/L)
double Y = (CENTRAL / Vc) * (1 + EPS(1)) + EPS(2); // Prediction with residual error (mg/L)
capture CL = K10 * Vc; // Clearance (L/h)
capture Q = K12 * Vc; // Intercompartimental clearance (L/h)
capture Vp = Q / K21; // Peripheral volume (L)
capture AUC = AUC_calc; // Area under the curve (exposure) (mg.h/L)

$CAPTURE 
IPRED, Y, Vc, CL, Q, Vp, AUC, Cmax,CRCL,CCRCL,KE1,K12,K21