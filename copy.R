# Create copy for dashboard

cardiff <- HTML("<p class=sans> The U.S. Department of Justice found that more than half of violent crime in the United States goes unreported to law enforcement. This is a clear limitation to understanding where violence occurs. The Cardiff Model combines police and hospital data on violence. As a result, key stakeholders from policy makers to public health experts can more accurately assess violence in the community. <a href='https://www.cdc.gov/violenceprevention/publichealthissue/fundedprograms/cardiffmodel/whatis.html'>Read More at the CDC</a></p>")
methods <- HTML("<p class = sans>Currently, Demographic data comes from the 2017 American Community Survey 5 Year estimates (2013-2017 ACS5). Poverty was calculated as number of individuals living below the 2017 poverty line out of poverty calculable population. Educational Attainment was calculated for the population above the age of 18. Unemployment rate was calculated as number of individuals reporting unemployment out of total individuals reporting currently being in the workforce. Home ownership was calculated as number of individuals reporting ownership of the home they occupy out of total number of homes.</br>
                          Crime data comes from the City of Saint Louis and is only representative of 2018 in this demo. There were no cases of rape in the crime data for 2018. Assaults excluded those against police.</br>
                Environmental data for schools, parks and bus stops were provided by the City of Saint Louis. Other environmental data were scraped from various web sources.</br>
                Choropleth maps use Natural Jenks 5 class Breaks.</br>
                Weed & Seed Income from guidestar.org and Project Safe Neighborhood spending from usaspending.gov</p>")

vp_orgs <- HTML("<h3 id='wns'>Weed & Seed</h3>
                <p>Weed & Seed was a federally funded crime prevention inititative which encouraged a two-pronged approach:'weeding' - an increased policing effort, and 'seeding' - an increased community development investment.  The initial evaluations highlighted the lack of prosecution as weak link in the program's success, noting that increasing funding for police activities without also increasing funding for prosecution is non-sustainable and reduces long term impact effectiveness. <a href = 'https://www.ncjrs.gov/pdffiles1/176358.pdf'>Read More</a></p>
                <h3 id='gbb'>Gun Buyback Program</h3>
                <p>Primarily a symbolic intervention.  Staged by police forces, gun buyback programs are meant to encourage members of a community - particularly youth - to hand in illicit firearms in return for money.  SLMPD hold them periodically.</p>
                <h3 id='sas'>Search and Seize</h3>
                <p>Search and seize was a collaboration between police and community members to search homes where juvenilles were suspected to be hiding guns. The program occured in three main stages. <a href='https://www.ncjrs.gov/pdffiles1/nij/191332.pdf'>Read More</a></p>
                <h3 id='saf'>Safe Futures</h3>
                <p></p>
                <h3 id='cfi'><Ceasefire Initiative/h3>
                <p></p>
                <h3 id='sac'>Strategic Approaches to Community Safety Initiative</h3>
                <p></p>
                <h3 id='fdi'>5th District Initiative</h3>
                <p></p>
                <h3 id='psn'>Project Safe Neighborhoods</h3>
                <p></p>
                <h3 id='hbt'>Hospital Based Trauma Intervention</h3>
                <p></p>
                <h3 id='mvo'>Most Violent Offenders</h3>
                <p></p>
                <h3 id='wgf'>Wells Goodfellow Neighborhood Initiative</h3>
                <p></p>
                <h3 id='bha'>Brady Handgun Violence Prevention Act</h3>
                <p></p>
                ")


save(cardiff, file = "dashboard/copy.rda")