**************************************************************
*** DATA

SETS

 bs              'bio scenarios'         /zero,p1nodisp,p1full,p2nodisp,p2full/

 mo              'monthly'               /1*12/
 msel(mo)        'subset of months'      /1*12/

* make scen a subset of ssup?
 scen            'scenarios'             /base 2011, hot 2012/
* ssup            'selected'              /base, hot/


 size            'vessel size'           /small  34 ft and under,
                                          mid    35-39 ft,
                                          large  40 ft and over/

 summer(mo)       'summer season '       /6*11  /
 winter(mo)       'winter season'        /12,1*5/

 yr               ' year'                /2011, 2012/
;


** Call data from EXCEL **
$CALL GDXXRW.EXE "2_lobster_data.xls" Index=Index!a1

PARAMETERS
         annualCost(*, size)      ' annual cost estimates ($) - GMRI Survey                                                     '
         baitCostPerTrip(mo,size) ' bait cost ($) - GMRI Survey                                                                 '
         c0(mo, size)             ' calibration intercept                                                                       '
         c1(mo, size)             ' calibration slope                                                                           '
*         c2(msel,size)            ' quadratic term for calibrated cost function                                                 '
         participation(mo,size)   ' count of boats that made at least 1 trip - 2011 Dealer data                                 '
         scenParticipation(scen, mo, size) 'participation'

         scenCatch(scen,mo,size)  ' mean lbs. per trip -- 2011 Dealer data                                                      '
         catchPerTrip(mo,size)    ' mean lbs. per trip -- 2011 Dealer data                                                      '
         dist(mo,size)            ' distance traveled - GMRI survey - Table 15                                                  '
         effortPerTrip(*, size)   ' crew-related parameters                                                                     '
         monthsOperating(size)    ' avg # months in operation by size - inferred from survey & dealer data '
         n(size)                  ' number of vessels (# boats) - 2011 Dealer data                                              '

         scenPbar(scen,mo)        ' average price -- 2011 Dealer data                                                           '
         pbar(mo)                 ' average price -- 2011 Dealer data                                                           '

         scenTrips(scen,mo, size) ' avg trips per month    -- Dealer data                                                      '
         trips(mo, size)          ' avg trips per month    -- Dealer data                                                      '

         scenProd(scen,mo)        ' total production (lbs) -- Dealer data                                                      '
;


$GDXIN 2_lobster_data.gdx
$LOAD annualCost, baitCostPerTrip, c0, c1, participation, scenParticipation,catchPerTrip, scenCatch, dist, effortPerTrip, monthsOperating, n,scenPbar, pbar, trips, scenTrips,scenProd
$GDXIN

** Calculated/Derived Parameters **
PARAMETERS
  capacity(mo)          ' capacity of supply chain to handle lobster -- increase over 2011 data          '

*  kbar(size)           ' mean number of boats                                                           '
  fuelCost(size)        ' annual fuel cost - GMRI survey                                                 '
  fixedCost(mo,size)    ' average fixed cost -- Vessel and Gear Maintenance from Table 13 of GMRI survey '
  days(size)            ' days of labor on average when  sternmen are present                            '
  freq(size)            ' frequency of crew on board                                                     '
  hrsPerTrip(size)      ' avg hrs per trip                                                               '

  distAnn(size)         ' annual distance covered                                                        '
  fuelCostPerMile(size) ' cost per mile                                                                  '

  a0(mo)                ' commodity demand curve intercept - linear est. based on 2009-11 dealer data    '
  a1(mo)                ' commodity demand curve slope     - linear est. based on 2009-11 dealer data    '
  a2(mo)                ' slope for qty above capacity level '

  laborCost(size)       ' total labor cost ($) - GMRI Survey (updated by Alexa)                          '
  lcostPerTrip(size)    ' labor cost per trip                                                            '
  lcostPerHr(size)      ' labor cost per hour                                                            '

*  lbarAnn(size)        ' average labor hours calculation                                                '
  lsup(mo)              ' exogenous labor supply - upper bound                                           '
  tripHrs(mo, size)     ' total trip hours PER BOAT  - GMRI                                              '
;

* Winter capacity not an issue.
* We are assuming 30 additional capacity for summer months
 capacity(mo)$winter(mo) =        scenProd('base',mo);
 capacity(mo)$summer(mo) =  1.3 * scenProd('base',mo);

 fuelCost(size)                = annualCost('fuel', size);
 fixedCost(mo,size)            = annualCost('fixed',size)/CARD(mo);   //monthsOperating(size)
 laborCost(size)               = annualCost('labor',size);

 distAnn(size)                 = SUM(mo, trips(mo, size) * dist(mo,size));
* @@@ update in scenario loop
 fuelCostPerMile(size)         = fuelCost(size)/distAnn(size);

* DEMAND parameters ***
* Winter values assigned first
a0(mo)                         = 5.043621044028 ;
a1(mo)                         = -0.000000495270;

* Summer values assigned
a0(mo)$summer(mo)              = 4.041875385873 ;
a1(mo)$summer(mo)              = -0.000000064453;

* SCALED winter
* y = -0.000495270x + 5.043621044
*
* SCALED summer
*  y = -0.000064453x + 4.041875386

***************************************************************************************************
* Try SCALING **

SCALAR def deflator /0.001/;

scenParticipation(scen, mo, size) = def * scenParticipation(scen, mo, size);
participation(mo,size)            = def * participation(mo,size)  ;
n(size)                           = def * n(size) ;
scenProd(scen,mo)                 = def * scenProd(scen,mo) ;
a1(mo)                            = a1(mo)/def;

$ONTEXT
scencatch(scen,mo,size)        = def * scencatch(scen,mo,size) ;
catchPerTrip(mo,size)          = def * catchPerTrip(mo,size)   ;
fuelCost(size)                 = def * fuelCost(size);
fixedCost(mo,size)             = def * fixedCost(mo,size);
fuelCostPerMile(size)          = def * fuelCostPerMile(size);
laborCost(size)                = def * laborCost(size);
lcostPerTrip(size)             = def *lcostPerTrip(size)   ;
lcostPerHr(size)               = def * lcostPerHr(size)     ;
lsup(mo)                       = def *lsup(mo)        ;
tripHrs(mo, size)              = def *tripHrs(mo,size);
$OFFTEXT

**** LABOR *************************************************************************************

SCALARS
  b0                          'Labor Supply  intercept ($ per hr)'
  b1                          'Labor supply slope - from Excel based on combo of GMRI Survey and dealer data'
  wbar                        'mean wage based on the data'
;

days(size)               = effortPerTrip('days', size);
freq(size)               = effortPerTrip('freq', size);
hrsPerTrip(size)         = effortPerTrip('hrs', size);
tripHrs(mo, size)        = freq(size) * days(size) * hrsPerTrip(size)  * trips(mo, size);
lsup(mo)                 = SUM(size, tripHrs(mo,size) * n(size) );
wbar                     = SUM(size, laborCost(size)/SUM(mo, tripHrs(mo, size) )/CARD(size))    ;

** ESTIMATE Labor Supply Parameters
VARIABLES  MSE         ' Mean Square Error';
POSITIVE VARIABLES
         BETA0         ' Intercept '
         BETA1         ' Slope '
         WHAT(mo)      ' Estimated wage'
;

EQUATIONS
         mse_e         ' Calculate MSE '
         wage_e(mo)    ' Calculate Estimated Wage '
;

mse_e..                  MSE        =E= SUM(mo,  (WHAT(mo) - wbar ) * (WHAT(mo) - wbar ));
wage_e(mo)..             WHAT(mo)   =E= BETA0 + BETA1 * SUM(size, tripHrs(mo, size) * participation(mo,size));

BETA0.up    = 12;
WHAT.lo(mo) = wbar/2;
WHAT.up(mo) = 2*wbar;

MODEL LSUPPLY /mse_e,wage_e/;
SOLVE LSUPPLY Using NLP Minimizing MSE;

b0 = BETA0.L;
b1 = BETA1.L;

DISPLAY wbar,b0,b1;

*****************************************************************
* REPORTING PARAMETERS -- Declared here and used post-analysis

 PARAMETERS
         utilization(msel, size) ' boat utilization as ratio of optimal to baseline values '
         diff(msel,size)             ' % deviation from 100% boat utilization                  '
         profit_pre(msel,size)       ' pre-optimization profit calculation                     '
         rev_pre(msel,size)
         cst_pre(msel,size)

         profit_post(msel,size)      ' post-optimization profit calculation                    '

         annReport_pre(*,size)       ' budget sheet pre-optimization ($1000)                   '
         annReport_post(*,size)      ' budget sheet post-optimization ($1000)                  '
         budgetRatio(*,size)         ' ratio of post to pre budgets                            '

         p_pre(msel)                 ' price                                                   '
         w_pre(msel)                 ' wage                                                    '
         totCatch(msel)              ' total catch in lbs.                                     '
         totHrs(msel)                ' total labor hours                                       '
         cs(msel)                    ' consumer surplus                                        '
         ps(msel, size)              ' producer surplus                                        '

         out(*,bs,msel, size)
         aggOut(*,bs,msel)
         implan(*,bs,size)         ' values to plug into IMPLAN                              '

         del(*, msel,size)           ' change in variables btwn scenarios                      '
         aggDel(*, msel)             ' change in agg variables btwn scenarios                  '
         mr(msel, size)              ' marginal revenue                                        '
         mc(msel, size)              ' marginal cost                                           '
         ar(msel, size)              ' average  revenue                                        '
         ac(msel, size)              ' average  cost                                           '
         devObs(*,scen, msel)        ' deviation from observed data                            '

;

PARAMETER        cadj(mo)        adjustment to operating costs;

* Shift operating costs as needed
* Feb appears particularly unprofitable hence the parameter value < 1 here.
 cadj(mo)            = 1.0;
 cadj('2')           = 0.6;

* cadj(mo)$summer(mo) = 0.1;
* cadj(mo)$winter(mo) = 1.1;


 SCALARS
        csAnn                        ' Annual comsumer surplus                                 '
        psAnn                        ' Annual producer surplus                                 '
;
