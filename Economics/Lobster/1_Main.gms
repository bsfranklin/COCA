**************************************************************
*** MAIN
*
*
* OPTIONS *
$EOLCOM //

$INCLUDE "C:\SESYNC\COCA\Economics\2_Data.gms"
$INCLUDE "C:\SESYNC\COCA\Economics\3_Model.gms"
*$INCLUDE "C:\SESYNC\COCA\kg_per_tow_SVSPP_301_spring_scenarios_for_GAMS.csv"


* Justin's scenarios *
$ONTEXT

base,p1nodisp,p1full,p2nodisp,p2full

  zero                1
 +1C full dispersal   0.262460677
 +1C no dispersal     0.249060144
 +2C full dispersal   0.19510772
 +2C no dispersal     0.167636355
$OFFTEXT

PARAMETER biodata(bs)  data from the bio model - these are adjustments to baseline fish avail.;
biodata('zero') =         1           ;
biodata('p1nodisp') =     0.262460677 ;
biodata('p1full') =       0.249060144  ;
biodata('p2nodisp') =     0.19510772   ;
biodata('p2full') =       0.167636355  ;

*LOOP(scen,

 LOOP( bs,
* base,p1,p2,p1nodisp,p2nodisp

  catchPerTrip(mo,size)   =  biodata(bs) * scenCatch('base',mo,size);

  trips(mo, size)         = scenTrips('base',mo, size);
  participation(mo, size) = scenParticipation('base', mo, size);
  pbar(mo)                = scenPbar('base',mo);

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

 out('k',bs,msel, size)   =    K.L(msel,size);
 out('l',bs,msel, size)   =    L.L(msel,size);
 out('x',bs,msel, size)   =    X.L(msel,size);

 out('bc',bs,msel, size)   =    K.L(msel,size) *  baitCostPerTrip(msel,size)            * trips(msel,size) ;
 out('fc',bs,msel, size)   =    K.L(msel,size) *  fuelCostPerMile(size)*dist(msel,size) * trips(msel,size) ;
 out('lc',bs,msel, size)   =    W.L(msel) * L.L(msel,size) ;
 out('rv',bs,msel ,size)   =    X.L(msel,size)*P.L(msel);

 aggOut('q',bs,msel)      =    Q.L(msel);
 aggOut('ld',bs,msel)     =    LD.L(msel);
 aggOut('p',bs,msel)      =    P.L(msel);
 aggOut('rv',bs,msel)     =    aggOut('p',bs,msel) * aggOut('q',bs,msel);
 aggOut('w',bs,msel)      =    W.L(msel);
 aggOut('ts', bs, msel)   =    (a0(msel) + 0.5*a1(msel)* Q.L(msel))* Q.L(msel) - 0.5 * W.L(msel) * LD.L(msel) - SUM(size, NLC.L(msel,size)) ;

** REPORTING
$INCLUDE "C:\SESYNC\lobster\4_report.gms"

 aggOut('cs',bs,msel)      = cs(msel);
 aggOut('ps',bs,msel)      = SUM(size, ps(msel,size));

 out('ps',bs,msel, size)   = ps(msel,size);
* out('mr', scen, msel, size) = mr(msel, size);
* out('mc', scen, msel, size) = mc(msel, size);
* out('ar', scen, msel, size) = ar(msel, size);
* out('ac', scen, msel, size) = ac(msel, size);

 implan('bc',bs,size)      = SUM(msel, out('bc', bs,msel, size)  ) / SUM(msel, out('rv',bs,msel ,size));
 implan('fc',bs,size)      = SUM(msel, out('fc', bs,msel, size)  ) / SUM(msel, out('rv',bs,msel ,size));
 implan('lc',bs,size)      = SUM(msel, out('lc', bs,msel, size)  ) / SUM(msel, out('rv',bs,msel ,size));

$ONTEXT
 devObs('p','base', msel)      = 100*(scenPbar('base',msel) - P.L(msel))/scenPbar('base',msel);
 devObs('q','base', msel)      = 100*(scenProd('base',msel) - Q.L(msel))/scenProd('base',msel);

DISPLAY  del, aggDel, implan;
$OFFTEXT

); //bs
*);  //scen

DISPLAY out, aggout, implan;

PARAMETERS
         avgP(bs)
         avgQ(bs)
         avgS(bs)
;

 avgP(bs) = SUM(msel,         aggOut('p',bs,msel) )/CARD(msel);
 avgQ(bs) = SUM(msel,         aggOut('q',bs,msel) )/CARD(msel);
 avgS(bs) = SUM(msel,         aggOut('ps',bs,msel))/CARD(msel);


Execute_Unload '4_Output.gdx', avgP, avgQ,avgS;
Execute 'GDXXRW.EXE 4_Output.gdx Output=4_Output.xls par=avgP  rng=avgP!a1';
Execute 'GDXXRW.EXE 4_Output.gdx Output=4_Output.xls par=avgQ  rng=avgQ!a1';
Execute 'GDXXRW.EXE 4_Output.gdx Output=4_Output.xls par=avgS  rng=avgS!a1';




$ONTEXT
 del('k', msel,size) = 100* (out('k','hot',msel, size) - out('k','base',msel, size))/out('k','base',msel, size);
 del('l', msel,size) = 100* (out('l','hot',msel, size) - out('l','base',msel, size))/out('l','base',msel, size);
 del('x', msel,size) = 100* (out('x','hot',msel, size) - out('x','base',msel, size))/out('x','base',msel, size);
 del('ps', msel,size) = 100* (out('ps','hot',msel, size) - out('ps','base',msel, size))/out('ps','base',msel, size);


 del('mr', msel,size) = 100* (out('mr','hot',msel, size) - out('mr','base',msel, size))/out('mr','base',msel, size);
 del('mc', msel,size) = 100* (out('mc','hot',msel, size) - out('mc','base',msel, size))/out('mc','base',msel, size);
 del('ar', msel,size) = 100* (out('ar','hot',msel, size) - out('ar','base',msel, size))/out('ar','base',msel, size);
 del('ac', msel,size) = 100* (out('ac','hot',msel, size) - out('ac','base',msel, size))/out('ac','base',msel, size);


 aggDel('q', msel)  =  100* (aggOut('q','hot',msel)    - aggOut('q','base', msel))/aggOut('q','base', msel)   ;
 aggDel('ld', msel) =  100* (aggOut('ld','hot',msel)   - aggOut('ld','base',msel))/aggOut('ld','base',msel)   ;
 aggDel('p', msel)  =  100* (aggOut('p','hot',msel)    - aggOut('p','base', msel))/aggOut('p','base', msel)   ;
 aggDel('w', msel)  =  100* (aggOut('w','hot',msel)    - aggOut('w','base', msel))/aggOut('w','base', msel)   ;
 aggDel('ts', msel) =  100* (aggOut('ts','hot',msel)   - aggOut('ts','base',msel))/aggOut('ts','base',msel);
 aggDel('ps', msel) =  100* (aggOut('ps','hot',msel)   - aggOut('ps','base',msel))/aggOut('ps','base',msel);
 aggDel('cs', msel) =  100* (aggOut('cs','hot',msel)   - aggOut('cs','base',msel))/aggOut('cs','base',msel);

DISPLAY del, aggDel, devObs, scenPbar;

**********************************************************************************************************************

Execute 'GDXXRW.EXE 4_Output.gdx Output=4_Output.xls par=aggDel rng=aggDel!a1';
Execute 'GDXXRW.EXE 4_Output.gdx Output=4_Output.xls par=del    rng=del!a1';
$OFFTEXT

