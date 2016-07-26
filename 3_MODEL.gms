*****************************************************
** MODEL

FREE VARIABLES
 TS                       'total surplus - maximand'
;

POSITIVE VARIABLES
* CS(msel, size)            'consumer surplus'
* PS(msel, size)            'producer surplus'
 PC(msel)                  'production costs'

 K(msel,size)              'capital = number of boats '
 NLC(msel,size)            'non-labor costs'
 FC(msel,size)             'quasi-fixed cost '

 L(msel, size)             'labor demand = labor days * number of boats'
 LD(msel)                  'Labor demand'
 W(msel)                   'Wage'

 X(msel, size)             'Landings'

 Q(msel)                   'Qty supplied = total landings across all size classes'
 Q1(msel)
 Q2(msel)

 P(msel)                   'Commodity Price'
;

EQUATIONS
 obj_e                     'Objective function = Max Consumer Surplus + Producer Surplus'
* cs_e(msel, size)          'Consumer Surplus '
* ps_e(msel, size)          'Producer Surplus'

 pc_e(msel)                'Production costs (actual is half this value but this is accounted for in obj_e)'
 fc_e(msel, size)          'quasi-fixed cost calculation'
 nlc_e(msel,size)          'non-labor costs'

 landings_e(msel, size)    'Total catch by vessel size'
 q_e(msel)                 'quantity supplied'

 l_e(msel, size)           'labor by size class'
 ld_e(msel)                'labor demanded'

 l_con                     'labor constraint'
 k_con(msel,size)          'Boat capital constraint'

 price_demand_1(msel)        'supply price'
 price_demand_2(msel)

 price_labor(msel)         'wage calculation'
;

***********************************************************
* Maximize Consumer Surplus + Producer Surplus

 obj_e..                    TS                =E= SUM(msel, (a0(msel) + 0.5*a1(msel)* Q(msel))* Q(msel) - 0.5 * W(msel) * LD(msel) - SUM(size, NLC(msel,size))  );

* OLD:                      TS                =E= SUM(msel, 0.5 * (a0(msel) - P(msel)) * Q(msel)  + 0.5 * P(msel) * Q(msel) - PC(msel)  );

 pc_e(msel)..              PC(msel)          =E= 0.5 * W(msel) * LD(msel) + SUM(size, NLC(msel,size));
 nlc_e(msel,size)..        NLC(msel,size)    =E= K(msel,size)*( baitCostPerTrip(msel,size)* trips(msel,size) + fuelCostPerMile(size)*dist(msel,size)*trips(msel,size) + FC(msel,size) );
* @@@@ was multiplying FC by 0.5 but this seems to have been wrong!

* fc_e(msel, size)..        FC(msel,size)     =E= c0(msel,size)  +  0.5 * c1(msel,size)* K(msel,size) + 0.3333 * c2(msel,size)* K(msel,size) * K(msel,size);
 fc_e(msel, size)..        FC(msel,size)     =E= c0(msel,size)  +  0.5 * c1(msel,size)* K(msel,size);

* Catch
 landings_e(msel,size)..   X(msel,size)      =E= catchPerTrip(msel,size) * trips(msel,size) *  K(msel,size);
 q_e(msel)..               Q(msel)           =E= SUM(size,  X(msel,size) );

*** @@@ Not sure how this approach will work if I don't price in the objective fn!!
*
*  phat'   = phat*(1  - (1/sf)*(q/capacity) )
*  capacity  = 1.3 * q11
*
SCALAR sf scaling factor for price eqtn /0.01 /;

*@@@@
*a1('6') = 1.2 * a1('6');
*a1('7') = 1.2 * a1('7');
*@@@@

 price_demand_1(msel)$(winter(msel))..      P(msel) =E=   a0(msel)  +  a1(msel) * Q(msel) ;
 price_demand_2(msel)$(summer(msel))..      P(msel) =E=  (a0(msel)  +  a1(msel) * Q(msel));//* (1  - (1/sf)*(Q(msel)/capacity(msel)) )  ;

* Labor
 l_e(msel, size)..         L(msel, size)     =E= tripHrs(msel, size) * K(msel,size);
 ld_e(msel)..              LD(msel)          =E= SUM(size, L(msel,size) );
 price_labor(msel)..       W(msel)           =E= b0 + b1 * LD(msel) ;

* Constraints
 k_con(msel,size)..        K(msel,size)      =L= n(size)  ;

* @@@ I don't think we should have this here
 l_con(msel)..             LD(msel)          =L= lsup(msel);

 MODEL Lobster /  obj_e
                  pc_e
                  fc_e
                  nlc_e

                  landings_e
                  q_e
                  price_demand_1
                  price_demand_2

                  l_e
                  ld_e
                  price_labor

                  k_con
*                  l_con
/;





