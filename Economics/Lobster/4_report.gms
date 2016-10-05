****************************************************************************
*** REPORTING

 totCatch(msel)  = SUM(size,  catchPerTrip(msel,size) * trips(msel,size) *  participation(msel,size) );
 totHrs(msel)    = SUM(size,  tripHrs(msel, size) * participation(msel,size));

 p_pre(msel)     = a0(msel)  +  a1(msel)*totCatch(msel);
 w_pre(msel)     = b0 + b1* totHrs(msel);

 profit_pre(msel,size) =   p_pre(msel) * catchPerTrip(msel,size)*trips(msel,size)  - w_pre(msel) * tripHrs(msel, size)
                         - (baitCostPerTrip(msel,size)  + fuelCostPerMile(size) * dist(msel,size)) * trips(msel,size) - fixedCost(msel,size) ;

 rev_pre(msel,size)      =  pbar(msel) * catchPerTrip(msel,size)* trips(msel,size);
 cst_pre(msel,size)      =  wbar * tripHrs(msel, size) + (baitCostPerTrip(msel,size)  + fuelCostPerMile(size) * dist(msel,size)) * trips(msel,size) + fixedCost(msel,size);

 utilization(msel, size) = K.L(msel,size)/participation(msel,size);
 diff(msel,size)         = 100*( utilization(msel, size) - 1);

 profit_post(msel,size)$(K.L(msel,size) >0)  = (P.L(msel)* X.L(msel,size) -  W.L(msel)* L.L(msel, size))/ K.L(msel,size)
                                          - (baitCostPerTrip(msel,size) + fuelCostPerMile(size) * dist(msel,size))* trips(msel,size)   - fixedCost(msel,size) ;

 cs(msel) = 0.5 * (a0(msel) - P.L(msel)) * Q.L(msel);
 ps(msel,size)= P.L(msel) * X.L(msel, size) - 0.5 * W.L(msel)* L.L(msel, size) - NLC.L(msel,size) ;

 csAnn    = SUM(msel, cs(msel));
 psAnn    = SUM((msel,size), ps(msel,size));

$ONTEXT
 mr(msel, size) = a1(msel) *  catchPerTrip(msel,size) * trips(msel,size) *  K.L(msel,size) + P.L(msel)* catchPerTrip(msel,size) * trips(msel,size);
 mc(msel,size)  = 0.5 * b1 * tripHrs(msel, size)  * tripHrs(msel, size) * K.L(msel,size) + 0.5 * (b0 + b1 * tripHrs(msel, size) * K.L(msel,size) ) * tripHrs(msel, size)
                 + baitCostPerTrip(msel,size)* trips(msel,size) + fuelCostPerMile(size)*dist(msel,size)*trips(msel,size) + 0.5 * FC.L(msel,size) + c1(msel,size)*K.L(msel,size) ;


ar(msel, size) = ( a0(msel)  +  a1(msel) * catchPerTrip(msel,size) * trips(msel,size) * K.L(msel,size)  ) * catchPerTrip(msel,size) * trips(msel,size) ;
ac(msel, size) =  0.5 *( b0 + b1 * tripHrs(msel, size) * K.L(msel,size))* tripHrs(msel, size) + ( baitCostPerTrip(msel,size)* trips(msel,size) + fuelCostPerMile(size)*dist(msel,size)*trips(msel,size) + 0.5 * FC.L(msel,size) );
$OFFTEXT

 DISPLAY utilization,diff, p_pre, w_pre, profit_pre, profit_post,k_con.M;
 DISPLAY P.L, W.L, K.L, TS.L, psAnn, csAnn, ps, cs;


********************************************************************************

$ONTEXT
* SCALE to $1000
 annReport_pre('catch',size)    =  0.001 * SUM(msel,                    catchPerTrip(msel,size) * trips(msel,size) );
 annReport_pre('revenue',size)  =  0.001 * SUM(msel,                    pbar(msel) * catchPerTrip(msel,size)     * trips(msel,size));
 annReport_pre('bait',size)     =  0.001 * SUM(msel,                    baitCostPerTrip(msel,size)               * trips(msel,size));
 annReport_pre('fixed',size)    =  0.001 * SUM(msel,                    fixedCost(msel,size));
 annReport_pre('fuel',size)     =  0.001 * SUM(msel,                    fuelCostPerMile(size) * dist(msel,size)  * trips(msel,size));
 annReport_pre('labor',size)    =  0.001 *                              wbar * lbarAnn(size);

 annReport_post('catch',size)   =  0.001 * SUM(msel$(K.L(msel,size) > 0), X.L(msel,size)/ K.L(msel,size));
 annReport_post('revenue',size) =  0.001 * SUM(msel$(K.L(msel,size) > 0), P.L(msel) * X.L(msel, size)/ K.L(msel,size));
 annReport_post('bait',size)    =  0.001 * SUM(msel$(K.L(msel,size) > 0), baitCostPerTrip(msel,size)               * trips(msel,size));
 annReport_post('fixed',size)   =  0.001 * SUM(msel,                    fixedCost(msel,size));
 annReport_post('fuel',size)    =  0.001 * SUM(msel$(K.L(msel,size) > 0), fuelCostPerMile(size) * dist(msel,size)  * trips(msel,size));
 annReport_post('labor',size)   =  0.001 * SUM(msel$(K.L(msel,size) > 0), W.L(msel)* L.L(msel, size)/ K.L(msel,size));

 annReport_pre('profit',size)    =  annReport_pre('revenue',size)  - annReport_pre('bait',size)  - annReport_pre('fuel',size)  - annReport_pre('labor',size)  - annReport_pre('fixed',size) ;
 annReport_post('profit',size)   =  annReport_post('revenue',size) - annReport_post('bait',size) - annReport_post('fuel',size) - annReport_post('labor',size) - annReport_post('fixed',size) ;

 budgetRatio( 'catch',    size) =    annReport_post('catch',size)  / annReport_pre('catch',size)  ;
 budgetRatio( 'revenue',  size) =    annReport_post('revenue',size)/  annReport_pre('revenue',size);
 budgetRatio( 'bait',     size) =    annReport_post('bait',size)   /  annReport_pre('bait',size)   ;
 budgetRatio( 'fuel',     size) =    annReport_post('fuel',size)   /  annReport_pre('fuel',size)   ;
 budgetRatio( 'labor',    size) =    annReport_post('labor',size)  /  annReport_pre('labor',size)  ;
 budgetRatio( 'fixed',    size) =    annReport_post('fixed',size)  /  annReport_pre('fixed',size)  ;
 budgetRatio( 'profit',   size) =    annReport_post('profit',size) /  annReport_pre('profit',size) ;
$OFFTEXT





