BACKGROUND:
QuinCe is currently assuming all raw data are dry, and therefore does these convertions:
- pCO2 dry Teq
-> pCO2 wet Teq
-> fCO2 Teq
-> fCO2 SST

Since at leaset one FOS is measured wet (station PAP), we need to make an R script that skips the 
"-> pCO2 wet Teq" step. I will make a script that uses the raw data to calculate pCO2-
-----------------------------
See my overview "overview_FOS_h20_oxygen_ph.xlsx" and see email steve 28 nov. 

FOS has not been processed correctly in QuinCamilla in terms of drying and h2o, however the effect of this is linear (just shifts the fco2 up a bit).
In terms of reporting (labelling reports) we want the data to be handeled correct, and therefore QuinCamilla will be updated to produce correct plots 
and values that we can insert into the othervise finished labelling reports.

-----------------------------



TODO:
make script that uses raw data as input; calculates fCO2, pH and oxygen using equations in the manuals in dropbox and plots these as output. 


EQUATION FOR pH:
see manual 

EQUATION FOR OXYGEN:
see manual 


EQUATION FROM  xCO2 to PCO2:
see manual about ProOceanus (equation 4.1 and 4.2, and ignore the million in 4.2)

(cell gas/1013.25) * xCO2 = pCO2 
(the cell gas pressure should be used as Peq for  the ProOceanus.


EQUATION FROM pCO2 to fCO2:
There's an R package called seacarb that has a function to do pCO2 -> fCO2
https://rdrr.io/cran/seacarb/man/p2fCO2.html
It needs the atmospheric pressure in Atmospheres (converted from hPa that the data is reported in)
I'll have to see if they report it
You can use the EqP column as the atmospheric pressure