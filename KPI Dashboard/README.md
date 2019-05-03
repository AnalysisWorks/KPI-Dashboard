# Dashboard improvements

TODO

1 Refactor code, this should be at most 200-300 lines
   any required session variables can be passed in and assigned after
   move these functions into othe rstructured folders

2 Format code consistently
   to be done after refactoring for simplicity

3 Checklist KPIs
   i) QA Standardize Data ( QA ADT Transactions )
       - admits/discharges over time, usually by site. Can looks at site and nu_program
   ii) QA - Utilization Trend (determine end of period automatically)
       - Coverage to end of cycle period for all sites. i.e. site utilization
       - Coverage to end of cycle period for all Programs and nursing units
       - Compare previous fiscal year against current for all sites (can do all sites/program/nu)
   iii) QA - Utilization Table
       - May want to look into ways to aggregate this data into a table
       - Occupancy Rates and off-service may be difficult
       - % ALC, will need to compare like KPIs across types
   iv) QA - Efficiency Module
       - Will need set off KPIs to handle Efficiencies

4 Order how anomalies are generated
   i) Start with site (authority anomalies don't really matter)
   ii) If some anomaly present, drill down and repeat
   iii) If no anomaly present, drill down only once, if still no anomalies, stop
   iv) May want to have special case for nu code

5 Other anomaly calculations, 
   i) largest difference between cycles, normalized for selection
       - will need to consider where periods dont over lap, ignored entirely? 
       - If added into calculation, long term units may be favoured i.e. more post record activity
   ii) largest difference year over year, normalized by selection
       - this will pick up new units and removed units
       - once the above cases are identified, need a process to ignore

   iii) largest prediction error, normalized (may require different normalization due to error metric)
       - Once problematic KPIs are discovered, if not removed will forever skew the model
       - Will need a process to remove previous KPIs
       - The goal of this model is similar to largest difference in cycles and may be redundant