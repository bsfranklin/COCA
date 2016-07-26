**************************************************************
*** MAIN
*
*
* OPTIONS *
$EOLCOM //

$INCLUDE "J:\Research\2015_2018_COCA\3_Program\Lobster_GAMS\2_Data.gms"
$INCLUDE "J:\Research\2015_2018_COCA\3_Program\Lobster_GAMS\3_Model.gms"

LOOP(scen,

  trips(mo, size)         = scenTrips(scen,mo, size);
  catchPerTrip(mo,size)   = scenCatch(scen,mo,size) ;
  participation(mo, size) = scenParticipation(scen, mo, size);
  pbar(mo)                = scenPbar(scen,mo);

  distAnn(size)           = SUM(mo, trips(mo, size) * dist(mo,size));
  fuelCostPerMile(size)   = fuelCost(size)/distAnn(size);
  tripHrs(mo, size)       = freq(size) * days(size) * hrsPerTrip(size)  * trips(mo, size);

 DISPLAY trips,catchPerTrip,tripHrs,fuelCostPerMile,distAnn;

 K.L(msel,size)    = participation(msel,size);
 L.L(msel,size)    = tripHrs(msel, size)* K.L(msel,size);
 LD.L(msel)        = SUM(size,  L.L(msel,size) )  ;
 W.L(msel)         = b0 + b1 * LD.L(msel);

 X.L(msel,size)    = catchPerTrip(msel,size) * trips(msel,size) * K.L(msel,size);
 Q.L(msel)         = SUM(size,  X.L(msel,size) );
 P.L(msel)         = a0(msel)  +  a1(msel) * Q.L(msel) ;

 FC.L(msel,size)   = c0(msel,size)  +  0.5 * c1(msel,size)* K.L(msel,size);

 PC.L(msel)        = W.L(msel) * LD.L(msel)
                    + SUM(size,  baitCostPerTrip(msel,size)               * trips(msel,size) * K.L(msel,size))
                    + SUM(size,  fuelCostPerMile(size) *  dist(msel,size) * trips(msel,size) * K.L(msel,size))
                    + SUM(size,  FC.L(msel,size))
                    ;

 TS.L              = SUM(msel, (a0(msel) + 0.5*a1(msel)* Q.L(msel))* Q.L(msel) - PC.L(msel)  );
*SUM((msel,size), 0.5 * (a0(msel) - P.L(msel)) * Q.L(msel)  + 0.5 * P.L(msel) * Q.L(msel) - PC.L(msel)  );

 Lobster.scaleopt        = 1;
 SOLVE Lobster Using NLP Maximizing TS;

 out('k',scen,msel, size)   =    K.L(msel,size);
 out('l',scen,msel, size)   =    L.L(msel,size);
 out('x',scen,msel, size)   =    X.L(msel,size);

 out('bc',scen,msel, size)   =    K.L(msel,size) *  baitCostPerTrip(msel,size)            * trips(msel,size) ;
 out('fc',scen,msel, size)   =    K.L(msel,size) *  fuelCostPerMile(size)*dist(msel,size) * trips(msel,size) ;
 out('lc',scen,msel, size)   =    W.L(msel) * L.L(msel,size) ;
 out('rv',scen,msel ,size)   =    X.L(msel,size)*P.L(msel);

 aggOut('q',scen,msel)      =    Q.L(msel);
 aggOut('ld',scen,msel)     =    LD.L(msel);
 aggOut('p',scen,msel)      =    P.L(msel);
 aggOut('rv',scen,msel)     =    aggOut('p',scen,msel) * aggOut('q',scen,msel);
 aggOut('w',scen,msel)      =    W.L(msel);
 aggOut('ts', scen, msel)   =    (a0(msel) + 0.5*a1(msel)* Q.L(msel))* Q.L(msel) - 0.5 * W.L(msel) * LD.L(msel) - SUM(size, NLC.L(msel,size)) ;

** REPORTING
$INCLUDE "J:\Research\2015_2018_COCA\3_Program\Lobster_GAMS\4_report.gms"

 aggOut('cs',scen,msel)      = cs(msel);
 aggOut('ps',scen,msel)      = SUM(size, ps(msel,size));

 out('ps',scen,msel, size)   = ps(msel,size);
* out('mr', scen, msel, size) = mr(msel, size);
* out('mc', scen, msel, size) = mc(msel, size);
* out('ar', scen, msel, size) = ar(msel, size);
* out('ac', scen, msel, size) = ac(msel, size);

 devObs('p',scen, msel)      = 100*(scenPbar(scen,msel) - P.L(msel))/scenPbar(scen,msel);
 devObs('q',scen, msel)      = 100*(scenProd(scen,msel) - Q.L(msel))/scenProd(scen,msel);

 implan('bc',scen,size)      = SUM(msel, out('bc', scen,msel, size)  ) / SUM(msel, out('rv',scen,msel ,size));
 implan('fc',scen,size)      = SUM(msel, out('fc', scen,msel, size)  ) / SUM(msel, out('rv',scen,msel ,size));
 implan('lc',scen,size)      = SUM(msel, out('lc', scen,msel, size)  ) / SUM(msel, out('rv',scen,msel ,size));
 
);

 del('k', msel,size) = 100* (out('k','hot',msel, size) - out('k','base',msel, size))/out('k','base',msel, size);
 del('l', msel,size) = 100* (out('l','hot',msel, size) - out('l','base',msel, size))/out('l','base',msel, size);
 del('x', msel,size) = 100* (out('x','hot',msel, size) - out('x','base',msel, size))/out('x','base',msel, size);
 del('ps', msel,size) = 100* (out('ps','hot',msel, size) - out('ps','base',msel, size))/out('ps','base',msel, size);

$ONTEXT
 del('mr', msel,size) = 100* (out('mr','hot',msel, size) - out('mr','base',msel, size))/out('mr','base',msel, size);
 del('mc', msel,size) = 100* (out('mc','hot',msel, size) - out('mc','base',msel, size))/out('mc','base',msel, size);
 del('ar', msel,size) = 100* (out('ar','hot',msel, size) - out('ar','base',msel, size))/out('ar','base',msel, size);
 del('ac', msel,size) = 100* (out('ac','hot',msel, size) - out('ac','base',msel, size))/out('ac','base',msel, size);
$OFFTEXT

 aggDel('q', msel)  =  100* (aggOut('q','hot',msel)    - aggOut('q','base', msel))/aggOut('q','base', msel)   ;
 aggDel('ld', msel) =  100* (aggOut('ld','hot',msel)   - aggOut('ld','base',msel))/aggOut('ld','base',msel)   ;
 aggDel('p', msel)  =  100* (aggOut('p','hot',msel)    - aggOut('p','base', msel))/aggOut('p','base', msel)   ;
 aggDel('w', msel)  =  100* (aggOut('w','hot',msel)    - aggOut('w','base', msel))/aggOut('w','base', msel)   ;
 aggDel('ts', msel) =  100* (aggOut('ts','hot',msel)   - aggOut('ts','base',msel))/aggOut('ts','base',msel);
 aggDel('ps', msel) =  100* (aggOut('ps','hot',msel)   - aggOut('ps','base',msel))/aggOut('ps','base',msel);
 aggDel('cs', msel) =  100* (aggOut('cs','hot',msel)   - aggOut('cs','base',msel))/aggOut('cs','base',msel);

DISPLAY del, aggDel, devObs, scenPbar;

**********************************************************************************************************************
Execute_Unload '4_Output.gdx', aggOut, out, del, aggDel,implan;
Execute 'GDXXRW.EXE 4_Output.gdx Output=4_Output.xls par=aggOut rng=aggOut!a1';
Execute 'GDXXRW.EXE 4_Output.gdx Output=4_Output.xls par=out    rng=out!a1';
Execute 'GDXXRW.EXE 4_Output.gdx Output=4_Output.xls par=aggDel rng=aggDel!a1';
Execute 'GDXXRW.EXE 4_Output.gdx Output=4_Output.xls par=del    rng=del!a1';
Execute 'GDXXRW.EXE 4_Output.gdx Output=4_Output.xls par=implan rng=implan!a1';
