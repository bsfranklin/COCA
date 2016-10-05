*************************************************************
*** CALIBRATION

* OPTIONS *
$EOLCOM //

** DATA & MODEL Statement
$INCLUDE "J:\Research\2015_2018_COCA\3_Program\Lobster_GAMS\2_data.gms"

 totCatch(msel)        = SUM(size,  catchPerTrip(msel,size) * trips(msel,size) *  participation(msel,size) );
 totHrs(msel)          = SUM(size,  tripHrs(msel, size) * participation(msel,size));

$ONTEXT
* @@@ TRY USING AVERAGE 2011-12 data

 totCatch(msel)           = 0.5*SUM(scen,  scenProd(scen,msel) );
 trips(msel,size)         = 0.5*SUM(scen,  scenTrips(scen,msel,size) );
 totHrs(msel)             = 0.5*SUM((scen, size), freq(size) * days(size) * hrsPerTrip(size)  * trips(msel,size));
 catchPerTrip(msel,size)  = 0.5*SUM(scen, scenCatch(scen,msel,size));
 participation(msel,size) = 0.5*SUM(scen, scenParticipation(scen, msel, size));
$OFFTEXT

 p_pre(msel)           = a0(msel)  +  a1(msel)*totCatch(msel);
 w_pre(msel)           = b0 + b1 * totHrs(msel);

 profit_pre(msel,size) =  p_pre(msel) * catchPerTrip(msel,size)*trips(msel,size)
                        - w_pre(msel) * tripHrs(msel, size)
                        - (baitCostPerTrip(msel,size)  + fuelCostPerMile(size) * dist(msel,size)) * trips(msel,size) -  0.4 * fixedCost(msel,size) ;

DISPLAY profit_pre,lsup;

*****************************************************
** VARIABLES && MODEL
FREE VARIABLES
 Z                        'maximand'
;

POSITIVE VARIABLES
 K(msel,size)              'capital = number of boats '

 NLC(msel,size)          'Non-Labor costs'

 L(msel, size)             'labor demand = labor days * number of boats'
 LD(msel)                  'Labor demand'
 W(msel)

 X(msel, size)             'Landings'
 Q(msel)                   'Qty supplied = total landings across all size classes'
 P(msel)
;

EQUATIONS
 cal_obj_e                 'minimize MSE'
 nlc_e(msel,size)          'non-labor costs'

 landings_e(msel, size)    'Total catch by vessel size'
 q_e(msel)                 'quantity supplied'
 price_demand(msel)

 l_e(msel, size)           'labor by size class'
 ld_e(msel)                'labor demanded'
 price_labor(msel)

 cal_con(msel, size)
;


SCALAR           epsilon         small number /0.000001/;

*     0.5 * (a0(msel) - P(msel)) * Q(msel)  +  0.5 * P(msel) * Q(msel)


a1('6') = 1.2 * a1('6');
a1('7') = 1.2 * a1('7');

***********************************************************
 cal_obj_e..               Z                =E= SUM(msel, (a0(msel) + 0.5*a1(msel)* Q(msel))* Q(msel) - 0.5 * W(msel) * LD(msel) - SUM(size, NLC(msel,size))  );

 nlc_e(msel,size)..        NLC(msel,size)   =E= K(msel,size)*( baitCostPerTrip(msel,size)* trips(msel,size) + fuelCostPerMile(size)*dist(msel,size)*trips(msel,size) + 0.5 * cadj(msel) * fixedCost(msel,size));

 landings_e(msel,size)..   X(msel,size)     =E= catchPerTrip(msel,size) * trips(msel,size) *  K(msel,size);
 q_e(msel)..               Q(msel)          =E= SUM(size,  X(msel,size) );
 price_demand(msel)..      P(msel)          =E= a0(msel)  +  a1(msel) * Q(msel) ;

 l_e(msel, size)..         L(msel, size)    =E= tripHrs(msel, size) * K(msel,size);
 ld_e(msel)..              LD(msel)         =E= SUM(size,  L(msel,size) );
 price_labor(msel)..       W(msel)           =E= b0 + b1 * LD(msel) ;

 cal_con(msel, size)..     K(msel,size)  + epsilon   =L= participation(msel,size);

 MODEL BoatCal /
         cal_obj_e
         nlc_e
         landings_e
         q_e
         price_demand

         l_e
         ld_e
         price_labor

         cal_con
 /;


 BoatCal.scaleopt= 1;

 K.L(msel,size)  = participation(msel,size);
 L.L(msel,size)  = tripHrs(msel, size)* trips(msel,size)* K.L(msel,size);
 LD.L(msel)      = SUM(size,  L.L(msel,size) )  ;

 X.L(msel,size)  = catchPerTrip(msel,size) * trips(msel,size) * K.L(msel,size);
 Q.L(msel)       = SUM(size,  X.L(msel,size) );

 Z.L             = SUM((msel, size),           p_pre(msel)*  Q.L(msel)
                 - 0.5 * w_pre(msel) * LD.L(msel)
                 - K.L(msel,size)*( baitCostPerTrip(msel,size)* trips(msel,size) + fuelCostPerMile(size)*dist(msel,size)*trips(msel,size) + fixedCost(msel,size))
 );

 SOLVE BoatCal Using NLP MAXIMIZING Z;

 PARAMETERS
              utilization(msel, size)     'boat utilization as ratio of optimal to baseline values'
              mr(msel, size)              'marginal revenue'
              mc(msel,size)               'marginal cost check'
              mpi(msel,size)              'marginal profit check'
;

  utilization(msel, size)  = K.L(msel,size)/participation(msel,size);
  c1(msel,size)  = 2*cal_con.m(msel,size)/K.L(msel,size);
  c0(msel,size)  = 0.5 * cadj(msel) * fixedCost(msel,size) - cal_con.m(msel,size)  ;
  mr(msel, size) = P.L(msel)* catchPerTrip(msel,size) * trips(msel,size);
  mc(msel,size)  = baitCostPerTrip(msel,size)* trips(msel,size) + fuelCostPerMile(size)*dist(msel,size)*trips(msel,size) + c0(msel,size) + c1(msel,size)*K.L(msel,size) + 0.5 * W.L(msel) * tripHrs(msel,size);
  mpi(msel,size) = mr(msel, size) -  mc(msel,size);

*DISPLAY  p_pre,  w_pre, profit_pre,afixedCost, utilization, participation;
 DISPLAY utilization,participation, c0, c1, mc, mpi, cal_con.M, tripHrs;

 Execute_Unload '2_Lobster_data.gdx', c0,c1;
 Execute 'GDXXRW.EXE 2_Lobster_data.gdx Output=2_Lobster_data.xls par=c0 rng=calibration!b2';
 Execute 'GDXXRW.EXE 2_Lobster_data.gdx Output=2_Lobster_data.xls par=c1 rng=calibration!b17';


