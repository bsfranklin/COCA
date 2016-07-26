# COCA

# R

## GAMS
Have loaded the code from Maine lobster model that does the following:
+ Calibrates model to 2011 landings.
+ Uses 2012 data as a shock and then estimates fishermen response.

## Connecting bio to eco

+ R model should run for a given climate scenario.
+ R calls GAMS and passes in data using system() command.
+ GAMS runs and then sends output to CSV or similar.
+ R generates new map of ports with economics output (% change) by port.


