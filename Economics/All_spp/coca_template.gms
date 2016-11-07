
SETS

* fishing activities
 i activity - defined as target spp group X gear type    /ds_scallop, dl_scallop,
                                                          gs_plaice_pollock, gl_plaice_pollock,
                                                          ll_mackerel,
                                                          ps_hagfish, pl_hagfish, ps_lobster, pl_lobster,
                                                          sn_herring, sn_mackerel,
                                                          ts_pollock_redfish, tl_pollock_redfish   /

* Don't have cost data for handline but would like to include it in model.
 j gear type            /ds dredge_small, dl dredge_large, gs gillnet_small, gl gillnet_large, ll longline, ps pot_small, pl pot_large, sn seine, ts trawl_small, tl trawl_large /

 sp species             /cod, dogfish, haddock, hagfish, hake, herring, lobster, mackerel, menhaden, monkfish, plaice, pollock, redfish, scallop, winter_fl, witch_fl, yellowtail_fl  /
 gf(sp)  groundfish spp /cod, haddock, hake, plaice, pollock, redfish, winter_fl, witch_fl, yellowtail_fl /

* spatial
 c county        //
 pt ports         //
 r region        //
 st state        //

* temporal
 t time period   //

* Selector sets
 isel(i)         selected activities     //
 psel(pt)        selected ports          //
 jsel(j)         selected gear           //
 ssel(s)         selected species        //
 tsel(t)         selected time periods   //

* Multi-sets
 i2j(i,j)        activity to gear type   //

 p2c(pt,c)       ports to county         //
 c2st(c,st)      county to state         //
 st2r(st,r)      state to region         //
;

PARAMETERS
 daysPerTrip(isel)           days per trip for given activitiy
 steamPerTrip(isel)          avg steam time per trip - use for fuel cost

 dr(isel, ssel)              discard rate for species s under activity i
 baseLanding(isel, ssel)     baseline landings numbers for spp s under activity i
 otherCatch(isel)            remaining baseline landings after accounting for all selected activities  - this matters for quota constraint
;


VARIABLES
 TOTPROFIT       total profit in PORT
 TOTWELFARE      total welfare in SPEQ
 PROFIT(isel)    profit by fishing activity
;

POSITIVE VARIABLES

 CATCH(isel, ssel  )     'catch by activity '
 BYCATCH(isel, ssel)     'by-catch of spp s in activity i'
 DISCARD(isel, ssel)     'discard of spp s in activity i   DISCARD = disc_rate *BYCATCH'
 LANDING(isel, ssel)     'landings of spp s in activity i  LANDING = CATCH - DISCARD '
;


EQUATIONS

 calibrate_obj           'objective function for calibration method - minimize sum of square errors'
 speq_obj                'objective function for spatial equilibrium model - welfare max '
 port_obj                'object function for port optimization - profit max             '

 sse_e(isel)             'SSE = f(observed landings, predicted landings')
 welfare_e(isel)         'welfare = CS + PS'
 profit_e(isel)          'profit by species and gear type'

 speq_price_e(ssel)      'function of total landings'
 port_price_e(ssel)      'exogenous parameter value '

 catch_e(isel, ssel)     'catch by activity'
 bycatch_e(isel, ssel)   'by-catch by activity'
 discard_e(isel, ssel)   'discards by activity'
 landings_e(isel, ssel)  'landings = catch - discards '

 totcost_e(jsel)
 baitcost_e(jsel)
 fuelcost_e(jsel)
 crewcost_e(jsel)

 das_e(ssel)             days at sea calculation

 das_con(ssel)           days at sea constraint'
 quota_con(ssel)         'quota constraint - catch leq ACE'




;


MODEL CALIBRATE//

MODEL SPEQ //

MODEL PORT //


SOLVE CALIBRATE Minimizing SSE Using NLP;

SOLVE SPEQ Maximizing TOTWELFARE USING NLP;

* Iterate over all desired ports
LOOP(psel,

* Set port-specific data here
*

SOLVE PORT Maximizing TOTPROFIT USING NLP;

* Save output from port
*

);


OUTPUT....
