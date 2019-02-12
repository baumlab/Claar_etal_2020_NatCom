1) data_w 

Each row is an individual coral colony.

Column Names
- Coral_Tag: The first coral tag name for that tagged coral. (e.g. coral 11 was retagged to be 11_633, but this column only includes its first name, "11")
- Coral_Tag_All: All coral tag names for that coral (e.g. coral 11 was retagged to be 11_633, and this column includes that full name, "11_633")
- SampleType: coral/water/sediment; all samples in this dataset are coral, but retained for future compatibility with other samples
- Coral_Species: coral species name, or genus+sp if not ID'd to species.
- Site: Kiritimati site number
- Tube_label.2014: Tube label for August 2014 field season (first tagging expedition)
- Tube_label.2015Jan: Tube label for January 2015 field season (stormy, but before heat stress)
- Tube_label.2015May: Tube label for May 2015 field season (before [or at the very onset] of heat stress)
- Tube_label.2015July: Tube label for July 2015 field season (during heat stress, DHW~11)
- Tube_label.2016March: Tube label for March 2016 field season (after peak heat stress, residual heat stress still present)
- Tube_label.2016Nov: Tube label for November 2016 field season (~6 months after heat stress)
- Tube_label.2017: Tube label for July 2017 field season (~>1 year after heat stress)
- status: of the coral colony in March 2016
  + alive: alive in March 2016
  + dead: dead in March 2016
  + UK: status unknown, and not knowable
  + gone_before: colony was missing before the heat stress; could be due to any number of reasons
  + gone_after: colony was sampled immediately before or during heat stress, but was missing in March 2016; likely dead, but not confirmed
  + gone_UK: sampled early on (e.g. 2014) and never found again; can't know whether it went missing before or after heat stress
- before: was the colony sampled before the heat stress? (incl. 2014, January 2015, May 2015)
- during: was the colony sampled during the heat stress? (incl. July 2015 only)
- after: was the colony sampled after peak heat stress? (incl. March 2016 only)
- recovery: was the colony sampled during the recovery period (incl. November 2016 and July 2017)

2) summary

Each row is a coral species.

Column Names
- n_species: total number of colonies for that species
- n_species_wbefore: number of colonies that were sampled at least once before the heat stress
- n_alive: number of colonies that survived (i.e. alive in March 2016)
- n_dead: number of colonies that died (i.e. dead in March 2016)
- n_UK: number of colonies with unknown outcome
- n_gone_before: number of colonies that were gone before the heat stress
- n_gone_after: number of colonies that were sampled immediately before or during the heat stress, but were gone after (March 2016)
- n_gone_UK: number of colonies that were sampled early on (e.g. 2014), and gone sometime after (unknown if they were lost before or after heat stress)