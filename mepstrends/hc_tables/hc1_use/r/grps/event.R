FYC <- FYC %>% mutate(
  HHTEXP.yy. = HHAEXP.yy. + HHNEXP.yy., # Home Health Agency + Independent providers
  ERTEXP.yy. = ERFEXP.yy. + ERDEXP.yy., # Doctor + Facility Expenses for OP, ER, IP events
  IPTEXP.yy. = IPFEXP.yy. + IPDEXP.yy.,
  OPTEXP.yy. = OPFEXP.yy. + OPDEXP.yy., # All Outpatient
  OPYEXP.yy. = OPVEXP.yy. + OPSEXP.yy., # Physician only
  OPZEXP.yy. = OPOEXP.yy. + OPPEXP.yy., # non-physician only
  OMAEXP.yy. = VISEXP.yy. + OTHEXP.yy.) # Other medical expenses

FYC <- FYC %>% mutate(
  TOTUSE.yy. = (DVTOT.yy. + RXTOT.yy. + OBTOTV.yy. + OPTOTV.yy. + 
                ERTOT.yy. + IPDIS.yy. + HHTOTD.yy. + OMAEXP.yy.)
)
