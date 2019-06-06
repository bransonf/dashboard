# timeline for cardiff

tvis <- data.frame(
  id      = 1:32,
  content = c("Clarence Harmon becomes St. Louis' police commissioner, replacing Robert Scheetz",
              "Weed & Seed is launched as a federal crime prevention initiative <a onclick=customHref('about');document.getElementById('wns').scrollIntoView();>(Read More)</a>",
              "SLMPD begins gun buyback program <a onclick=customHref('about');document.getElementById('gbb').scrollIntoView();>(Read More)</a>",
              "Edward Dowd becomes US Attorney for the Eastern District of Missouri",
              "Janet Reno confirmed as head of the Justice Department under President Bill Clinton",
              "Freeman Bosley Jr. becomes mayor of St. Louis, replacing Vincent C. Schoemehl",
              "The St. Louis Consent-to-Search program begins <a onclick=customHref('about');document.getElementById('sas').scrollIntoView();>(Read More)</a>",
              "The Brady Hangun Violence Prevention Act passes <a onclick=customHref('about');document.getElementById('bha').scrollIntoView();>(Read More)</a>",
              "1993 ends with record number of homicides in both St. Louis City and County",
              "SLMPD estimates there are 280 gangs with approximately 3,500 members in the City, half of which are estimated to be minors",
              "St. Louis receives its first grant from the Weed & Seed program",
              "The Office of Juvenile Justice and Deliquency Prevention begins five year grant to establish the SafeFutures program <a onclick=customHref('about');document.getElementById('saf').scrollIntoView();>(Read More)</a>",
              "Ron Henderson becomes the new police commissioner, replacing Clarence Harmon",
              "The St. Louis community forms a Ceasefire Task Force <a onclick=customHref('about');document.getElementById('cfi').scrollIntoView();>(Read More)</a>",
              "Clarence Harmon becomes mayor of St. Louis, replacing Freeman Bosley Jr.",
              "The Tower Grove East, Gravois Park and Vandeventer neighborhoods are added to the Weed & Seed program",
              "Strategic Approach to Community Safety Initiative (SACSI) Begins <a onclick=customHref('about');document.getElementById('sac').scrollIntoView();>(Read More)</a>",
              "Audrey G. Fleissig becomes US Attorney for the Eastern District of Missouri, replacing Edward Dowd",
              "The 5th District initiative increases police prescence in the 5th district, the highest-risk community <a onclick=customHref('about');document.getElementById('fdi').scrollIntoView();>(Read More)</a>",
              "John Ashcroft becomes the head of the Department of Justice under President George W. Bush",
              "Francis Slay becomes mayor, replacing Clarence Harmon",
              "Project Safe Neighborhoods launched by federal government <a onclick=customHref('about')document.getElementById('psn').scrollIntoView();;>(Read More)</a>",
              "Joe Mokwa becomes police comissioner, replacing Ron Henderson",
              "Hospital based trauma intervention program begins <a onclick=customHref('about');document.getElementById('hbt').scrollIntoView();>(Read More)</a>",
              "Raymond Gruender becomes US Attorney for the Eastern District of Missouri, replacing Audrey Fleissig",
              "Most Violent Offenders Program launches, targeting high-risk individuals <a onclick=customHref('about');document.getElementById('mvo').scrollIntoView();>(Read More)</a>",
              "Project Safe Neighborhoods launches in Missouri",
              "Weed & Seed program evaluation begins",
              "2003 Ends with the lowest number of homicides in over a decade, and will continue to be the lowest number today",
              "Wells-Goodfellow Neighborhood initiative begins <a onclick=customHref('about');document.getElementById('wgf').scrollIntoView();>(Read More)</a>",
              "Daniel Isom becomes police commissioner, replacing Joe Mokwa",
              "Weed & Seed is discontinued after funding runs out"
              ),
  start   = c("1991-01-01","1991-02-17","1991-05-15","1993-01-01","1993-03-11","1993-04-20","1993-09-01","1993-09-30","1994-01-01","1995-01-01",
              "1995-01-15","1995-02-23","1995-12-05","1996-06-01","1997-04-15","1999-06-15","2000-10-01","2001-01-01","2000-01-18","2001-02-02",
              "2001-04-17","2001-05-05","2001-05-11","2001-06-19","2001-10-01","2001-12-01","2002-01-05","2002-07-04","2004-01-01","2008-04-01",
              "2008-10-06","2012-05-23"),
  end     = c(rep_len(NA, 32)),
  stringsAsFactors = FALSE
)

# homicide count

n_homicides <- data.frame(
  year = c(1990:2018),
  Homicides = c(177, 260, 231, 267, 248, 204, 166,153,113,130,123,148,119,73,155,128,128,138,160,144,142,113,113,120,159,188,188,205,187)
)

# funding

vp_funding <- data.frame(
  year = c(1990:2018),
  WS =    c(NA, NA, NA, NA, NA, NA, NA,    287117,324755,NA, 283279,235310,                  361657,                  418500,             NA,292270,343822,389423,377813,374877,402118,420366,45023,NA,NA,NA,NA,NA,NA),
  PSN =   c(NA, NA, NA, NA, NA, NA, NA,    NA,    NA,    NA,     NA,    NA,170000 + 327141 + 149852,259845 + 204171 + 300000,128536 + 249500,105853, 60905,198369,141829,117863,124712,    NA,   NA,NA,NA,NA,NA,NA,NA),
  Other = c(NA, NA, NA, NA, NA, NA, 498134,NA,    NA,    NA, 230000,    NA,                      NA,                      NA,             NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,   NA,NA,NA,NA,NA,NA,NA),
  total = c(NA, NA, NA, NA, NA, NA, 498134,287117,324755,NA, 513279,235310,                 1008650,                 1182516,         378036,398123,404727,587792,519642,492740,526830,420366,45023,NA,NA,NA,NA,NA,NA)
)

save(vp_funding, file = "../dashboard/dashboard/funding.rda")
save(n_homicides, file = "../dashboard/dashboard/n_murder.rda")
save(tvis, file = "../dashboard/dashboard/tvis.rda")
