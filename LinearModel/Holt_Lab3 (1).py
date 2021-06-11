#Load necessary packages
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
import statsmodels.api as sm
import pandas as pd
from pandas import DataFrame
import xlrd
import requests
import bs4
from bs4 import BeautifulSoup
from operator import itemgetter
import numpy as np
import difflib
import fuzzywuzzy
import seaborn as sns
import matplotlib.pyplot as plt
from scipy.stats import uniform
import statsmodels.api as sm


#Set option to view data frame similar to View in R
pd.set_option('expand_frame_repr', False)

#Coaches data
#Load data
coaches = pd.read_excel('coaches.xlsx')
#Add a column to serve as the unqiue identifier to improve joins
coaches.insert(0, 'coachID', range(10001, 10001 + len(coaches)))
#Remove Jr. from coach names
coaches['Coach'] = coaches['Coach'].str.replace('Jr.', '')

#Prep for web scrapping
#Get a list of coach names to help with scrapping
coach_url_list = coaches['Coach']
#Add a dash between first and last name for url
coach_url_list = coach_url_list.str.replace(' ','-')
#Convert to a list
coach_url_list = list(coach_url_list)
#convert coach names to lower case
coach_url_list = [coach.lower() for coach in coach_url_list]
#Use a loop to create urls for scrapping
#First need to create a blank list
coach_url = []
for coach in coach_url_list:
    coach = "https://www.sports-reference.com/cfb/coaches/"+str(coach)+"-1.html"
    coach_url.append(coach)


#Load links
#Manual updates were needed as some urls where different than the base url.
troycalhoun = requests.get( coach_url[0])
terrybowden = requests.get( coach_url[1])
nicksaban = requests.get( coach_url[2])
billclark = requests.get( 'https://www.sports-reference.com/cfb/coaches/bill-clark-3.html')
scottsatterfield = requests.get( coach_url[4])
kevinsumlin = requests.get( coach_url[5])
hermedwards = requests.get( 'https://www.sports-reference.com/cfb/coaches/herman-edwards-1.html')
chadmorris = requests.get( coach_url[7])
blakeanderson = requests.get( coach_url[8])
jeffmonken = requests.get( coach_url[9])
gusmalzahn = requests.get( coach_url[10])
mikeneu = requests.get( coach_url[11])
mattrhule = requests.get( coach_url[12])
bryanharsin = requests.get( coach_url[13])
steveaddazio = requests.get( coach_url[14])
mikejinks = requests.get( coach_url[15])
kalanisitake = requests.get( coach_url[16])
lanceleipold = requests.get( coach_url[17])
justinwilcox = requests.get( coach_url[18])
joshheupel = requests.get( coach_url[19])
johnbonamego = requests.get( coach_url[20])
bradlambert = requests.get( coach_url[21])
lukefickell = requests.get( coach_url[22])
daboswinney = requests.get( coach_url[23])
joemoglia = requests.get( coach_url[24])
mikemacintyre = requests.get( coach_url[25])
mikebobo = requests.get( 'https://www.sports-reference.com/cfb/coaches/mike-bobo-2.html')
randyedsall = requests.get( coach_url[27])
davidcutcliffe = requests.get( coach_url[28])
scottiemontgomery = requests.get( coach_url[29])
chriscreighton = requests.get( coach_url[30])
danmullen = requests.get( coach_url[31])
lanekiffin = requests.get( coach_url[32])
butchdavis = requests.get( coach_url[33])
willietaggart = requests.get( coach_url[34])
jefftedford = requests.get( coach_url[35])
kirbysmart = requests.get( coach_url[36])
chadlunsford = requests.get( coach_url[37])
shawnelliott = requests.get( coach_url[38])
pauljohnson = requests.get( coach_url[39])
nickrolovich = requests.get( coach_url[40])
majorapplewhite = requests.get( coach_url[41])
loviesmith = requests.get( coach_url[42])
tomallen = requests.get( coach_url[43])
kirkferentz = requests.get( coach_url[44])
mattcampbell = requests.get( coach_url[45])
davidbeaty = requests.get( coach_url[46])
billsnyder = requests.get( coach_url[47])
seanlewis = requests.get( coach_url[48])
markstoops = requests.get( coach_url[49])
turnergill = requests.get( coach_url[50])
skipholtz = requests.get( coach_url[51])
billynapier = requests.get( coach_url[52])
mattviator = requests.get( coach_url[53])
bobbypetrino = requests.get( coach_url[54])
edorgeron = requests.get( coach_url[55])
docholliday = requests.get( coach_url[56])
djdurkin = requests.get( coach_url[57])
markwhipple = requests.get( coach_url[58])
mikenorvell = requests.get( coach_url[59])
markricht = requests.get( coach_url[60])
chuckmartin = requests.get( coach_url[61])
jimharbaugh = requests.get( coach_url[62])
markdantonio = requests.get( coach_url[63])
rickstockstill = requests.get( coach_url[64])
pjfleck = requests.get("https://www.sports-reference.com/cfb/coaches/pj-fleck-1.html")
mattluke = requests.get('https://www.sports-reference.com/cfb/coaches/matt-luke-2.html')
joemoorhead = requests.get( coach_url[67])
barryodom = requests.get( coach_url[68])
kenniumatalolo = requests.get( coach_url[69])
scottfrost = requests.get( coach_url[70])
jaynorvell = requests.get( coach_url[71])
tonysanchez = requests.get( coach_url[72])
bobdavie = requests.get( coach_url[73])
dougmartin = requests.get('https://www.sports-reference.com/cfb/coaches/doug-martin-2.html')
larryfedora = requests.get( coach_url[75])
davedoeren = requests.get( coach_url[76])
sethlittrell = requests.get( coach_url[77])
rodcarey = requests.get( coach_url[78])
patfitzgerald = requests.get( coach_url[79])
briankelly = requests.get('https://www.sports-reference.com/cfb/coaches/brian-kelly-2.html')
franksolich = requests.get( coach_url[81])
urbanmeyer = requests.get( coach_url[82])
lincolnriley = requests.get( coach_url[83])
mikegundy = requests.get( coach_url[84])
bobbywilder = requests.get( coach_url[85])
mariocristobal = requests.get( coach_url[86])
jonathansmith = requests.get('https://www.sports-reference.com/cfb/coaches/jonathan-smith-2.html')
jamesfranklin = requests.get('https://www.sports-reference.com/cfb/coaches/james-franklin-2.html')
patnarduzzi = requests.get( coach_url[89])
jeffbrohm = requests.get( coach_url[90])
mikebloomgren = requests.get( coach_url[91])
chrisash = requests.get( coach_url[92])
rockylong = requests.get( coach_url[93])
brentbrennan = requests.get( coach_url[94])
stevecampbell = requests.get('https://www.sports-reference.com/cfb/coaches/steve-campbell-3.html')
willmuschamp = requests.get( coach_url[96])
charliestrong = requests.get( coach_url[97])
clayhelton = requests.get( coach_url[98])
sonnydykes = requests.get( coach_url[99])
jayhopson = requests.get( coach_url[100])
davidshaw = requests.get( coach_url[101])
dinobabers = requests.get( coach_url[102])
jeremypruitt = requests.get( coach_url[103])
tomherman = requests.get('https://www.sports-reference.com/cfb/coaches/tom-herman-2.html')
jimbofisher = requests.get( coach_url[105])
garypatterson = requests.get( coach_url[106])
everettwithers = requests.get( coach_url[107])
kliffkingsbury = requests.get( coach_url[108])
danadimel = requests.get( coach_url[109])
frankwilson = requests.get('https://www.sports-reference.com/cfb/coaches/frank-wilson-7.html')
jasoncandle = requests.get( coach_url[111])
nealbrown = requests.get( coach_url[112])
williefritz = requests.get( coach_url[113])
philipmontgomery = requests.get( coach_url[114])
chipkelly = requests.get( coach_url[115])
kylewhittingham = requests.get( coach_url[116])
mattwells = requests.get( coach_url[117])
derekmason = requests.get('https://www.sports-reference.com/cfb/coaches/derek-mason-2.html')
broncomendenhall = requests.get( coach_url[119])
justinfuente = requests.get( coach_url[120])
daveclawson = requests.get( coach_url[121])
chrispetersen = requests.get( coach_url[122])
mikeleach = requests.get( coach_url[123])
danaholgorsen = requests.get( coach_url[124])
mikesanford = requests.get("https://www.sports-reference.com/cfb/coaches/mike-sanford-2.html")
timlester = requests.get( coach_url[126])
paulchryst = requests.get( coach_url[127])
craigbohl = requests.get( coach_url[128])

#Obtain text from site
troycalhoun_soup = bs4.BeautifulSoup(troycalhoun.text,'lxml')
terrybowden_soup = bs4.BeautifulSoup(terrybowden.text,'lxml')
nicksaban_soup = bs4.BeautifulSoup(nicksaban.text,'lxml')
billclark_soup = bs4.BeautifulSoup(billclark.text,'lxml')
scottsatterfield_soup = bs4.BeautifulSoup(scottsatterfield.text,'lxml')
kevinsumlin_soup = bs4.BeautifulSoup(kevinsumlin.text,'lxml')
hermedwards_soup = bs4.BeautifulSoup(hermedwards.text,'lxml')
chadmorris_soup = bs4.BeautifulSoup(chadmorris.text,'lxml')
blakeanderson_soup = bs4.BeautifulSoup(blakeanderson.text,'lxml')
jeffmonken_soup = bs4.BeautifulSoup(jeffmonken.text,'lxml')
gusmalzahn_soup = bs4.BeautifulSoup(gusmalzahn.text,'lxml')
mikeneu_soup = bs4.BeautifulSoup(mikeneu.text,'lxml')
mattrhule_soup = bs4.BeautifulSoup(mattrhule.text,'lxml')
bryanharsin_soup = bs4.BeautifulSoup(bryanharsin.text,'lxml')
steveaddazio_soup = bs4.BeautifulSoup(steveaddazio.text,'lxml')
mikejinks_soup = bs4.BeautifulSoup(mikejinks.text,'lxml')
kalanisitake_soup = bs4.BeautifulSoup(kalanisitake.text,'lxml')
lanceleipold_soup = bs4.BeautifulSoup(lanceleipold.text,'lxml')
justinwilcox_soup = bs4.BeautifulSoup(justinwilcox.text,'lxml')
joshheupel_soup = bs4.BeautifulSoup(joshheupel.text,'lxml')
johnbonamego_soup = bs4.BeautifulSoup(johnbonamego.text,'lxml')
bradlambert_soup = bs4.BeautifulSoup(bradlambert.text,'lxml')
lukefickell_soup = bs4.BeautifulSoup(lukefickell.text,'lxml')
daboswinney_soup = bs4.BeautifulSoup(daboswinney.text,'lxml')
joemoglia_soup = bs4.BeautifulSoup(joemoglia.text,'lxml')
mikemacintyre_soup = bs4.BeautifulSoup(mikemacintyre.text,'lxml')
mikebobo_soup = bs4.BeautifulSoup(mikebobo.text,'lxml')
randyedsall_soup = bs4.BeautifulSoup(randyedsall.text,'lxml')
davidcutcliffe_soup = bs4.BeautifulSoup(davidcutcliffe.text,'lxml')
scottiemontgomery_soup = bs4.BeautifulSoup(scottiemontgomery.text,'lxml')
chriscreighton_soup = bs4.BeautifulSoup(chriscreighton.text,'lxml')
danmullen_soup = bs4.BeautifulSoup(danmullen.text,'lxml')
lanekiffin_soup = bs4.BeautifulSoup(lanekiffin.text,'lxml')
butchdavis_soup = bs4.BeautifulSoup(butchdavis.text,'lxml')
willietaggart_soup = bs4.BeautifulSoup(willietaggart.text,'lxml')
jefftedford_soup = bs4.BeautifulSoup(jefftedford.text,'lxml')
kirbysmart_soup = bs4.BeautifulSoup(kirbysmart.text,'lxml')
chadlunsford_soup = bs4.BeautifulSoup(chadlunsford.text,'lxml')
shawnelliott_soup = bs4.BeautifulSoup(shawnelliott.text,'lxml')
pauljohnson_soup = bs4.BeautifulSoup(pauljohnson.text,'lxml')
nickrolovich_soup = bs4.BeautifulSoup(nickrolovich.text,'lxml')
majorapplewhite_soup = bs4.BeautifulSoup(majorapplewhite.text,'lxml')
loviesmith_soup = bs4.BeautifulSoup(loviesmith.text,'lxml')
tomallen_soup = bs4.BeautifulSoup(tomallen.text,'lxml')
kirkferentz_soup = bs4.BeautifulSoup(kirkferentz.text,'lxml')
mattcampbell_soup = bs4.BeautifulSoup(mattcampbell.text,'lxml')
davidbeaty_soup = bs4.BeautifulSoup(davidbeaty.text,'lxml')
billsnyder_soup = bs4.BeautifulSoup(billsnyder.text,'lxml')
seanlewis_soup = bs4.BeautifulSoup(seanlewis.text,'lxml')
markstoops_soup = bs4.BeautifulSoup(markstoops.text,'lxml')
turnergill_soup = bs4.BeautifulSoup(turnergill.text,'lxml')
skipholtz_soup = bs4.BeautifulSoup(skipholtz.text,'lxml')
billynapier_soup = bs4.BeautifulSoup(billynapier.text,'lxml')
mattviator_soup = bs4.BeautifulSoup(mattviator.text,'lxml')
bobbypetrino_soup = bs4.BeautifulSoup(bobbypetrino.text,'lxml')
edorgeron_soup = bs4.BeautifulSoup(edorgeron.text,'lxml')
docholliday_soup = bs4.BeautifulSoup(docholliday.text,'lxml')
djdurkin_soup = bs4.BeautifulSoup(djdurkin.text,'lxml')
markwhipple_soup = bs4.BeautifulSoup(markwhipple.text,'lxml')
mikenorvell_soup = bs4.BeautifulSoup(mikenorvell.text,'lxml')
markricht_soup = bs4.BeautifulSoup(markricht.text,'lxml')
chuckmartin_soup = bs4.BeautifulSoup(chuckmartin.text,'lxml')
jimharbaugh_soup = bs4.BeautifulSoup(jimharbaugh.text,'lxml')
markdantonio_soup = bs4.BeautifulSoup(markdantonio.text,'lxml')
rickstockstill_soup = bs4.BeautifulSoup(rickstockstill.text,'lxml')
pjfleck_soup = bs4.BeautifulSoup(pjfleck.text,'lxml')
mattluke_soup = bs4.BeautifulSoup(mattluke.text,'lxml')
joemoorhead_soup = bs4.BeautifulSoup(joemoorhead.text,'lxml')
barryodom_soup = bs4.BeautifulSoup(barryodom.text,'lxml')
kenniumatalolo_soup = bs4.BeautifulSoup(kenniumatalolo.text,'lxml')
scottfrost_soup = bs4.BeautifulSoup(scottfrost.text,'lxml')
jaynorvell_soup = bs4.BeautifulSoup(jaynorvell.text,'lxml')
tonysanchez_soup = bs4.BeautifulSoup(tonysanchez.text,'lxml')
bobdavie_soup = bs4.BeautifulSoup(bobdavie.text,'lxml')
dougmartin_soup = bs4.BeautifulSoup(dougmartin.text,'lxml')
larryfedora_soup = bs4.BeautifulSoup(larryfedora.text,'lxml')
davedoeren_soup = bs4.BeautifulSoup(davedoeren.text,'lxml')
sethlittrell_soup = bs4.BeautifulSoup(sethlittrell.text,'lxml')
rodcarey_soup = bs4.BeautifulSoup(rodcarey.text,'lxml')
patfitzgerald_soup = bs4.BeautifulSoup(patfitzgerald.text,'lxml')
briankelly_soup = bs4.BeautifulSoup(briankelly.text,'lxml')
franksolich_soup = bs4.BeautifulSoup(franksolich.text,'lxml')
urbanmeyer_soup = bs4.BeautifulSoup(urbanmeyer.text,'lxml')
lincolnriley_soup = bs4.BeautifulSoup(lincolnriley.text,'lxml')
mikegundy_soup = bs4.BeautifulSoup(mikegundy.text,'lxml')
bobbywilder_soup = bs4.BeautifulSoup(bobbywilder.text,'lxml')
mariocristobal_soup = bs4.BeautifulSoup(mariocristobal.text,'lxml')
jonathansmith_soup = bs4.BeautifulSoup(jonathansmith.text,'lxml')
jamesfranklin_soup = bs4.BeautifulSoup(jamesfranklin.text,'lxml')
patnarduzzi_soup = bs4.BeautifulSoup(patnarduzzi.text,'lxml')
jeffbrohm_soup = bs4.BeautifulSoup(jeffbrohm.text,'lxml')
mikebloomgren_soup = bs4.BeautifulSoup(mikebloomgren.text,'lxml')
chrisash_soup = bs4.BeautifulSoup(chrisash.text,'lxml')
rockylong_soup = bs4.BeautifulSoup(rockylong.text,'lxml')
brentbrennan_soup = bs4.BeautifulSoup(brentbrennan.text,'lxml')
stevecampbell_soup = bs4.BeautifulSoup(stevecampbell.text,'lxml')
willmuschamp_soup = bs4.BeautifulSoup(willmuschamp.text,'lxml')
charliestrong_soup = bs4.BeautifulSoup(charliestrong.text,'lxml')
clayhelton_soup = bs4.BeautifulSoup(clayhelton.text,'lxml')
sonnydykes_soup = bs4.BeautifulSoup(sonnydykes.text,'lxml')
jayhopson_soup = bs4.BeautifulSoup(jayhopson.text,'lxml')
davidshaw_soup = bs4.BeautifulSoup(davidshaw.text,'lxml')
dinobabers_soup = bs4.BeautifulSoup(dinobabers.text,'lxml')
jeremypruitt_soup = bs4.BeautifulSoup(jeremypruitt.text,'lxml')
tomherman_soup = bs4.BeautifulSoup(tomherman.text,'lxml')
jimbofisher_soup = bs4.BeautifulSoup(jimbofisher.text,'lxml')
garypatterson_soup = bs4.BeautifulSoup(garypatterson.text,'lxml')
everettwithers_soup = bs4.BeautifulSoup(everettwithers.text,'lxml')
kliffkingsbury_soup = bs4.BeautifulSoup(kliffkingsbury.text,'lxml')
danadimel_soup = bs4.BeautifulSoup(danadimel.text,'lxml')
frankwilson_soup = bs4.BeautifulSoup(frankwilson.text,'lxml')
jasoncandle_soup = bs4.BeautifulSoup(jasoncandle.text,'lxml')
nealbrown_soup = bs4.BeautifulSoup(nealbrown.text,'lxml')
williefritz_soup = bs4.BeautifulSoup(williefritz.text,'lxml')
philipmontgomery_soup = bs4.BeautifulSoup(philipmontgomery.text,'lxml')
chipkelly_soup = bs4.BeautifulSoup(chipkelly.text,'lxml')
kylewhittingham_soup = bs4.BeautifulSoup(kylewhittingham.text,'lxml')
mattwells_soup = bs4.BeautifulSoup(mattwells.text,'lxml')
derekmason_soup = bs4.BeautifulSoup(derekmason.text,'lxml')
broncomendenhall_soup = bs4.BeautifulSoup(broncomendenhall.text,'lxml')
justinfuente_soup = bs4.BeautifulSoup(justinfuente.text,'lxml')
daveclawson_soup = bs4.BeautifulSoup(daveclawson.text,'lxml')
chrispetersen_soup = bs4.BeautifulSoup(chrispetersen.text,'lxml')
mikeleach_soup = bs4.BeautifulSoup(mikeleach.text,'lxml')
danaholgorsen_soup = bs4.BeautifulSoup(danaholgorsen.text,'lxml')
mikesanford_soup = bs4.BeautifulSoup(mikesanford.text,'lxml')
timlester_soup = bs4.BeautifulSoup(timlester.text,'lxml')
paulchryst_soup = bs4.BeautifulSoup(paulchryst.text,'lxml')
craigbohl_soup = bs4.BeautifulSoup(craigbohl.text,'lxml')

#Extract the table that contains the coach's records
troycalhoun_table = troycalhoun_soup.select('table')
terrybowden_table = terrybowden_soup.select('table')
nicksaban_table = nicksaban_soup.select('table')
billclark_table = billclark_soup.select('table')
scottsatterfield_table = scottsatterfield_soup.select('table')
kevinsumlin_table = kevinsumlin_soup.select('table')
hermedwards_table = hermedwards_soup.select('table')
chadmorris_table = chadmorris_soup.select('table')
blakeanderson_table = blakeanderson_soup.select('table')
jeffmonken_table = jeffmonken_soup.select('table')
gusmalzahn_table = gusmalzahn_soup.select('table')
mikeneu_table = mikeneu_soup.select('table')
mattrhule_table = mattrhule_soup.select('table')
bryanharsin_table = bryanharsin_soup.select('table')
steveaddazio_table = steveaddazio_soup.select('table')
mikejinks_table = mikejinks_soup.select('table')
kalanisitake_table = kalanisitake_soup.select('table')
lanceleipold_table = lanceleipold_soup.select('table')
justinwilcox_table = justinwilcox_soup.select('table')
joshheupel_table = joshheupel_soup.select('table')
johnbonamego_table = johnbonamego_soup.select('table')
bradlambert_table = bradlambert_soup.select('table')
lukefickell_table = lukefickell_soup.select('table')
daboswinney_table = daboswinney_soup.select('table')
joemoglia_table = joemoglia_soup.select('table')
mikemacintyre_table = mikemacintyre_soup.select('table')
mikebobo_table = mikebobo_soup.select('table')
randyedsall_table = randyedsall_soup.select('table')
davidcutcliffe_table = davidcutcliffe_soup.select('table')
scottiemontgomery_table = scottiemontgomery_soup.select('table')
chriscreighton_table = chriscreighton_soup.select('table')
danmullen_table = danmullen_soup.select('table')
lanekiffin_table = lanekiffin_soup.select('table')
butchdavis_table = butchdavis_soup.select('table')
willietaggart_table = willietaggart_soup.select('table')
jefftedford_table = jefftedford_soup.select('table')
kirbysmart_table = kirbysmart_soup.select('table')
chadlunsford_table = chadlunsford_soup.select('table')
shawnelliott_table = shawnelliott_soup.select('table')
pauljohnson_table = pauljohnson_soup.select('table')
nickrolovich_table = nickrolovich_soup.select('table')
majorapplewhite_table = majorapplewhite_soup.select('table')
loviesmith_table = loviesmith_soup.select('table')
tomallen_table = tomallen_soup.select('table')
kirkferentz_table = kirkferentz_soup.select('table')
mattcampbell_table = mattcampbell_soup.select('table')
davidbeaty_table = davidbeaty_soup.select('table')
billsnyder_table = billsnyder_soup.select('table')
seanlewis_table = seanlewis_soup.select('table')
markstoops_table = markstoops_soup.select('table')
turnergill_table = turnergill_soup.select('table')
skipholtz_table = skipholtz_soup.select('table')
billynapier_table = billynapier_soup.select('table')
mattviator_table = mattviator_soup.select('table')
bobbypetrino_table = bobbypetrino_soup.select('table')
edorgeron_table = edorgeron_soup.select('table')
docholliday_table = docholliday_soup.select('table')
djdurkin_table = djdurkin_soup.select('table')
markwhipple_table = markwhipple_soup.select('table')
mikenorvell_table = mikenorvell_soup.select('table')
markricht_table = markricht_soup.select('table')
chuckmartin_table = chuckmartin_soup.select('table')
jimharbaugh_table = jimharbaugh_soup.select('table')
markdantonio_table = markdantonio_soup.select('table')
rickstockstill_table = rickstockstill_soup.select('table')
pjfleck_table = pjfleck_soup.select('table')
mattluke_table = mattluke_soup.select('table')
joemoorhead_table = joemoorhead_soup.select('table')
barryodom_table = barryodom_soup.select('table')
kenniumatalolo_table = kenniumatalolo_soup.select('table')
scottfrost_table = scottfrost_soup.select('table')
jaynorvell_table = jaynorvell_soup.select('table')
tonysanchez_table = tonysanchez_soup.select('table')
bobdavie_table = bobdavie_soup.select('table')
dougmartin_table = dougmartin_soup.select('table')
larryfedora_table = larryfedora_soup.select('table')
davedoeren_table = davedoeren_soup.select('table')
sethlittrell_table = sethlittrell_soup.select('table')
rodcarey_table = rodcarey_soup.select('table')
patfitzgerald_table = patfitzgerald_soup.select('table')
briankelly_table = briankelly_soup.select('table')
franksolich_table = franksolich_soup.select('table')
urbanmeyer_table = urbanmeyer_soup.select('table')
lincolnriley_table = lincolnriley_soup.select('table')
mikegundy_table = mikegundy_soup.select('table')
bobbywilder_table = bobbywilder_soup.select('table')
mariocristobal_table = mariocristobal_soup.select('table')
jonathansmith_table = jonathansmith_soup.select('table')
jamesfranklin_table = jamesfranklin_soup.select('table')
patnarduzzi_table = patnarduzzi_soup.select('table')
jeffbrohm_table = jeffbrohm_soup.select('table')
mikebloomgren_table = mikebloomgren_soup.select('table')
chrisash_table = chrisash_soup.select('table')
rockylong_table = rockylong_soup.select('table')
brentbrennan_table = brentbrennan_soup.select('table')
stevecampbell_table = stevecampbell_soup.select('table')
willmuschamp_table = willmuschamp_soup.select('table')
charliestrong_table = charliestrong_soup.select('table')
clayhelton_table = clayhelton_soup.select('table')
sonnydykes_table = sonnydykes_soup.select('table')
jayhopson_table = jayhopson_soup.select('table')
davidshaw_table = davidshaw_soup.select('table')
dinobabers_table = dinobabers_soup.select('table')
jeremypruitt_table = jeremypruitt_soup.select('table')
tomherman_table = tomherman_soup.select('table')
jimbofisher_table = jimbofisher_soup.select('table')
garypatterson_table = garypatterson_soup.select('table')
everettwithers_table = everettwithers_soup.select('table')
kliffkingsbury_table = kliffkingsbury_soup.select('table')
danadimel_table = danadimel_soup.select('table')
frankwilson_table = frankwilson_soup.select('table')
jasoncandle_table = jasoncandle_soup.select('table')
nealbrown_table = nealbrown_soup.select('table')
williefritz_table = williefritz_soup.select('table')
philipmontgomery_table = philipmontgomery_soup.select('table')
chipkelly_table = chipkelly_soup.select('table')
kylewhittingham_table = kylewhittingham_soup.select('table')
mattwells_table = mattwells_soup.select('table')
derekmason_table = derekmason_soup.select('table')
broncomendenhall_table = broncomendenhall_soup.select('table')
justinfuente_table = justinfuente_soup.select('table')
daveclawson_table = daveclawson_soup.select('table')
chrispetersen_table = chrispetersen_soup.select('table')
mikeleach_table = mikeleach_soup.select('table')
danaholgorsen_table = danaholgorsen_soup.select('table')
mikesanford_table = mikesanford_soup.select('table')
timlester_table = timlester_soup.select('table')
paulchryst_table = paulchryst_soup.select('table')
craigbohl_table = craigbohl_soup.select('table')

#Read table and convert to a data frame
troycalhoun_df = pd.read_html(str(troycalhoun_table))[0]
terrybowden_df = pd.read_html(str(terrybowden_table))[0]
nicksaban_df = pd.read_html(str(nicksaban_table))[0]
billclark_df = pd.read_html(str(billclark_table))[0]
scottsatterfield_df = pd.read_html(str(scottsatterfield_table))[0]
kevinsumlin_df = pd.read_html(str(kevinsumlin_table))[0]
hermedwards_df = pd.read_html(str(hermedwards_table))[0]
chadmorris_df = pd.read_html(str(chadmorris_table))[0]
blakeanderson_df = pd.read_html(str(blakeanderson_table))[0]
jeffmonken_df = pd.read_html(str(jeffmonken_table))[0]
gusmalzahn_df = pd.read_html(str(gusmalzahn_table))[0]
mikeneu_df = pd.read_html(str(mikeneu_table))[0]
mattrhule_df = pd.read_html(str(mattrhule_table))[0]
bryanharsin_df = pd.read_html(str(bryanharsin_table))[0]
steveaddazio_df = pd.read_html(str(steveaddazio_table))[0]
mikejinks_df = pd.read_html(str(mikejinks_table))[0]
kalanisitake_df = pd.read_html(str(kalanisitake_table))[0]
lanceleipold_df = pd.read_html(str(lanceleipold_table))[0]
justinwilcox_df = pd.read_html(str(justinwilcox_table))[0]
joshheupel_df = pd.read_html(str(joshheupel_table))[0]
johnbonamego_df = pd.read_html(str(johnbonamego_table))[0]
bradlambert_df = pd.read_html(str(bradlambert_table))[0]
lukefickell_df = pd.read_html(str(lukefickell_table))[0]
daboswinney_df = pd.read_html(str(daboswinney_table))[0]
joemoglia_df = pd.read_html(str(joemoglia_table))[0]
mikemacintyre_df = pd.read_html(str(mikemacintyre_table))[0]
mikebobo_df = pd.read_html(str(mikebobo_table))[0]
randyedsall_df = pd.read_html(str(randyedsall_table))[0]
davidcutcliffe_df = pd.read_html(str(davidcutcliffe_table))[0]
scottiemontgomery_df = pd.read_html(str(scottiemontgomery_table))[0]
chriscreighton_df = pd.read_html(str(chriscreighton_table))[0]
danmullen_df = pd.read_html(str(danmullen_table))[0]
lanekiffin_df = pd.read_html(str(lanekiffin_table))[0]
butchdavis_df = pd.read_html(str(butchdavis_table))[0]
willietaggart_df = pd.read_html(str(willietaggart_table))[0]
jefftedford_df = pd.read_html(str(jefftedford_table))[0]
kirbysmart_df = pd.read_html(str(kirbysmart_table))[0]
chadlunsford_df = pd.read_html(str(chadlunsford_table))[0]
shawnelliott_df = pd.read_html(str(shawnelliott_table))[0]
pauljohnson_df = pd.read_html(str(pauljohnson_table))[0]
nickrolovich_df = pd.read_html(str(nickrolovich_table))[0]
majorapplewhite_df = pd.read_html(str(majorapplewhite_table))[0]
loviesmith_df = pd.read_html(str(loviesmith_table))[0]
tomallen_df = pd.read_html(str(tomallen_table))[0]
kirkferentz_df = pd.read_html(str(kirkferentz_table))[0]
mattcampbell_df = pd.read_html(str(mattcampbell_table))[0]
davidbeaty_df = pd.read_html(str(davidbeaty_table))[0]
billsnyder_df = pd.read_html(str(billsnyder_table))[0]
seanlewis_df = pd.read_html(str(seanlewis_table))[0]
markstoops_df = pd.read_html(str(markstoops_table))[0]
turnergill_df = pd.read_html(str(turnergill_table))[0]
skipholtz_df = pd.read_html(str(skipholtz_table))[0]
billynapier_df = pd.read_html(str(billynapier_table))[0]
mattviator_df = pd.read_html(str(mattviator_table))[0]
bobbypetrino_df = pd.read_html(str(bobbypetrino_table))[0]
edorgeron_df = pd.read_html(str(edorgeron_table))[0]
docholliday_df = pd.read_html(str(docholliday_table))[0]
djdurkin_df = pd.read_html(str(djdurkin_table))[0]
markwhipple_df = pd.read_html(str(markwhipple_table))[0]
mikenorvell_df = pd.read_html(str(mikenorvell_table))[0]
markricht_df = pd.read_html(str(markricht_table))[0]
chuckmartin_df = pd.read_html(str(chuckmartin_table))[0]
jimharbaugh_df = pd.read_html(str(jimharbaugh_table))[0]
markdantonio_df = pd.read_html(str(markdantonio_table))[0]
rickstockstill_df = pd.read_html(str(rickstockstill_table))[0]
pjfleck_df = pd.read_html(str(pjfleck_table))[0]
mattluke_df = pd.read_html(str(mattluke_table))[0]
joemoorhead_df = pd.read_html(str(joemoorhead_table))[0]
barryodom_df = pd.read_html(str(barryodom_table))[0]
kenniumatalolo_df = pd.read_html(str(kenniumatalolo_table))[0]
scottfrost_df = pd.read_html(str(scottfrost_table))[0]
jaynorvell_df = pd.read_html(str(jaynorvell_table))[0]
tonysanchez_df = pd.read_html(str(tonysanchez_table))[0]
bobdavie_df = pd.read_html(str(bobdavie_table))[0]
dougmartin_df = pd.read_html(str(dougmartin_table))[0]
larryfedora_df = pd.read_html(str(larryfedora_table))[0]
davedoeren_df = pd.read_html(str(davedoeren_table))[0]
sethlittrell_df = pd.read_html(str(sethlittrell_table))[0]
rodcarey_df = pd.read_html(str(rodcarey_table))[0]
patfitzgerald_df = pd.read_html(str(patfitzgerald_table))[0]
briankelly_df = pd.read_html(str(briankelly_table))[0]
franksolich_df = pd.read_html(str(franksolich_table))[0]
urbanmeyer_df = pd.read_html(str(urbanmeyer_table))[0]
lincolnriley_df = pd.read_html(str(lincolnriley_table))[0]
mikegundy_df = pd.read_html(str(mikegundy_table))[0]
bobbywilder_df = pd.read_html(str(bobbywilder_table))[0]
mariocristobal_df = pd.read_html(str(mariocristobal_table))[0]
jonathansmith_df = pd.read_html(str(jonathansmith_table))[0]
jamesfranklin_df = pd.read_html(str(jamesfranklin_table))[0]
patnarduzzi_df = pd.read_html(str(patnarduzzi_table))[0]
jeffbrohm_df = pd.read_html(str(jeffbrohm_table))[0]
mikebloomgren_df = pd.read_html(str(mikebloomgren_table))[0]
chrisash_df = pd.read_html(str(chrisash_table))[0]
rockylong_df = pd.read_html(str(rockylong_table))[0]
brentbrennan_df = pd.read_html(str(brentbrennan_table))[0]
stevecampbell_df = pd.read_html(str(stevecampbell_table))[0]
willmuschamp_df = pd.read_html(str(willmuschamp_table))[0]
charliestrong_df = pd.read_html(str(charliestrong_table))[0]
clayhelton_df = pd.read_html(str(clayhelton_table))[0]
sonnydykes_df = pd.read_html(str(sonnydykes_table))[0]
jayhopson_df = pd.read_html(str(jayhopson_table))[0]
davidshaw_df = pd.read_html(str(davidshaw_table))[0]
dinobabers_df = pd.read_html(str(dinobabers_table))[0]
jeremypruitt_df = pd.read_html(str(jeremypruitt_table))[0]
tomherman_df = pd.read_html(str(tomherman_table))[0]
jimbofisher_df = pd.read_html(str(jimbofisher_table))[0]
garypatterson_df = pd.read_html(str(garypatterson_table))[0]
everettwithers_df = pd.read_html(str(everettwithers_table))[0]
kliffkingsbury_df = pd.read_html(str(kliffkingsbury_table))[0]
danadimel_df = pd.read_html(str(danadimel_table))[0]
frankwilson_df = pd.read_html(str(frankwilson_table))[0]
jasoncandle_df = pd.read_html(str(jasoncandle_table))[0]
nealbrown_df = pd.read_html(str(nealbrown_table))[0]
williefritz_df = pd.read_html(str(williefritz_table))[0]
philipmontgomery_df = pd.read_html(str(philipmontgomery_table))[0]
chipkelly_df = pd.read_html(str(chipkelly_table))[0]
kylewhittingham_df = pd.read_html(str(kylewhittingham_table))[0]
mattwells_df = pd.read_html(str(mattwells_table))[0]
derekmason_df = pd.read_html(str(derekmason_table))[0]
broncomendenhall_df = pd.read_html(str(broncomendenhall_table))[0]
justinfuente_df = pd.read_html(str(justinfuente_table))[0]
daveclawson_df = pd.read_html(str(daveclawson_table))[0]
chrispetersen_df = pd.read_html(str(chrispetersen_table))[0]
mikeleach_df = pd.read_html(str(mikeleach_table))[0]
danaholgorsen_df = pd.read_html(str(danaholgorsen_table))[0]
mikesanford_df = pd.read_html(str(mikesanford_table))[0]
timlester_df = pd.read_html(str(timlester_table))[0]
paulchryst_df = pd.read_html(str(paulchryst_table))[0]
craigbohl_df = pd.read_html(str(craigbohl_table))[0]

#Create list of years to pass into isin function
#This allows you to select a subset of the data
coachingyears = ['1980','1981','1982','1983','1984','1985','1986','1987','1988','1989',
                 '1990','1991', '1992','1993','1994','1995','1996','1997','1998','1999',
                 '2000', '2001','2002', '2003','2004','2005','2006','2007','2008','2009',
                 '2010','2011','2012','2013','2014','2015','2016','2017']

#Reduce each coach's df to years prior to 2018 since we think the dataset goes up to and includes 2017
troycalhoun_df = troycalhoun_df [troycalhoun_df.Year.isin(coachingyears)]
terrybowden_df = terrybowden_df [terrybowden_df.Year.isin(coachingyears)]
nicksaban_df = nicksaban_df [nicksaban_df.Year.isin(coachingyears)]
billclark_df = billclark_df [billclark_df.Year.isin(coachingyears)]
scottsatterfield_df = scottsatterfield_df [scottsatterfield_df.Year.isin(coachingyears)]
kevinsumlin_df = kevinsumlin_df [kevinsumlin_df.Year.isin(coachingyears)]
hermedwards_df = hermedwards_df [hermedwards_df.Year.isin(coachingyears)]
chadmorris_df = chadmorris_df [chadmorris_df.Year.isin(coachingyears)]
blakeanderson_df = blakeanderson_df [blakeanderson_df.Year.isin(coachingyears)]
jeffmonken_df = jeffmonken_df [jeffmonken_df.Year.isin(coachingyears)]
gusmalzahn_df = gusmalzahn_df [gusmalzahn_df.Year.isin(coachingyears)]
mikeneu_df = mikeneu_df [mikeneu_df.Year.isin(coachingyears)]
mattrhule_df = mattrhule_df [mattrhule_df.Year.isin(coachingyears)]
bryanharsin_df = bryanharsin_df [bryanharsin_df.Year.isin(coachingyears)]
steveaddazio_df = steveaddazio_df [steveaddazio_df.Year.isin(coachingyears)]
mikejinks_df = mikejinks_df [mikejinks_df.Year.isin(coachingyears)]
kalanisitake_df = kalanisitake_df [kalanisitake_df.Year.isin(coachingyears)]
lanceleipold_df = lanceleipold_df [lanceleipold_df.Year.isin(coachingyears)]
justinwilcox_df = justinwilcox_df [justinwilcox_df.Year.isin(coachingyears)]
joshheupel_df = joshheupel_df [joshheupel_df.Year.isin(coachingyears)]
johnbonamego_df = johnbonamego_df [johnbonamego_df.Year.isin(coachingyears)]
bradlambert_df = bradlambert_df [bradlambert_df.Year.isin(coachingyears)]
lukefickell_df = lukefickell_df [lukefickell_df.Year.isin(coachingyears)]
daboswinney_df = daboswinney_df [daboswinney_df.Year.isin(coachingyears)]
joemoglia_df = joemoglia_df [joemoglia_df.Year.isin(coachingyears)]
mikemacintyre_df = mikemacintyre_df [mikemacintyre_df.Year.isin(coachingyears)]
mikebobo_df = mikebobo_df [mikebobo_df.Year.isin(coachingyears)]
randyedsall_df = randyedsall_df [randyedsall_df.Year.isin(coachingyears)]
davidcutcliffe_df = davidcutcliffe_df [davidcutcliffe_df.Year.isin(coachingyears)]
scottiemontgomery_df = scottiemontgomery_df [scottiemontgomery_df.Year.isin(coachingyears)]
chriscreighton_df = chriscreighton_df [chriscreighton_df.Year.isin(coachingyears)]
danmullen_df = danmullen_df [danmullen_df.Year.isin(coachingyears)]
lanekiffin_df = lanekiffin_df [lanekiffin_df.Year.isin(coachingyears)]
butchdavis_df = butchdavis_df [butchdavis_df.Year.isin(coachingyears)]
willietaggart_df = willietaggart_df [willietaggart_df.Year.isin(coachingyears)]
jefftedford_df = jefftedford_df [jefftedford_df.Year.isin(coachingyears)]
kirbysmart_df = kirbysmart_df [kirbysmart_df.Year.isin(coachingyears)]
chadlunsford_df = chadlunsford_df [chadlunsford_df.Year.isin(coachingyears)]
shawnelliott_df = shawnelliott_df [shawnelliott_df.Year.isin(coachingyears)]
pauljohnson_df = pauljohnson_df [pauljohnson_df.Year.isin(coachingyears)]
nickrolovich_df = nickrolovich_df [nickrolovich_df.Year.isin(coachingyears)]
majorapplewhite_df = majorapplewhite_df [majorapplewhite_df.Year.isin(coachingyears)]
loviesmith_df = loviesmith_df [loviesmith_df.Year.isin(coachingyears)]
tomallen_df = tomallen_df [tomallen_df.Year.isin(coachingyears)]
kirkferentz_df = kirkferentz_df [kirkferentz_df.Year.isin(coachingyears)]
mattcampbell_df = mattcampbell_df [mattcampbell_df.Year.isin(coachingyears)]
davidbeaty_df = davidbeaty_df [davidbeaty_df.Year.isin(coachingyears)]
billsnyder_df = billsnyder_df [billsnyder_df.Year.isin(coachingyears)]
seanlewis_df = seanlewis_df [seanlewis_df.Year.isin(coachingyears)]
markstoops_df = markstoops_df [markstoops_df.Year.isin(coachingyears)]
turnergill_df = turnergill_df [turnergill_df.Year.isin(coachingyears)]
skipholtz_df = skipholtz_df [skipholtz_df.Year.isin(coachingyears)]
billynapier_df = billynapier_df [billynapier_df.Year.isin(coachingyears)]
mattviator_df = mattviator_df [mattviator_df.Year.isin(coachingyears)]
bobbypetrino_df = bobbypetrino_df [bobbypetrino_df.Year.isin(coachingyears)]
edorgeron_df = edorgeron_df [edorgeron_df.Year.isin(coachingyears)]
docholliday_df = docholliday_df [docholliday_df.Year.isin(coachingyears)]
djdurkin_df = djdurkin_df [djdurkin_df.Year.isin(coachingyears)]
markwhipple_df = markwhipple_df [markwhipple_df.Year.isin(coachingyears)]
mikenorvell_df = mikenorvell_df [mikenorvell_df.Year.isin(coachingyears)]
markricht_df = markricht_df [markricht_df.Year.isin(coachingyears)]
chuckmartin_df = chuckmartin_df [chuckmartin_df.Year.isin(coachingyears)]
jimharbaugh_df = jimharbaugh_df [jimharbaugh_df.Year.isin(coachingyears)]
markdantonio_df = markdantonio_df [markdantonio_df.Year.isin(coachingyears)]
rickstockstill_df = rickstockstill_df [rickstockstill_df.Year.isin(coachingyears)]
pjfleck_df = pjfleck_df [pjfleck_df.Year.isin(coachingyears)]
mattluke_df = mattluke_df [mattluke_df.Year.isin(coachingyears)]
joemoorhead_df = joemoorhead_df [joemoorhead_df.Year.isin(coachingyears)]
barryodom_df = barryodom_df [barryodom_df.Year.isin(coachingyears)]
kenniumatalolo_df = kenniumatalolo_df [kenniumatalolo_df.Year.isin(coachingyears)]
scottfrost_df = scottfrost_df [scottfrost_df.Year.isin(coachingyears)]
jaynorvell_df = jaynorvell_df [jaynorvell_df.Year.isin(coachingyears)]
tonysanchez_df = tonysanchez_df [tonysanchez_df.Year.isin(coachingyears)]
bobdavie_df = bobdavie_df [bobdavie_df.Year.isin(coachingyears)]
dougmartin_df = dougmartin_df [dougmartin_df.Year.isin(coachingyears)]
larryfedora_df = larryfedora_df [larryfedora_df.Year.isin(coachingyears)]
davedoeren_df = davedoeren_df [davedoeren_df.Year.isin(coachingyears)]
sethlittrell_df = sethlittrell_df [sethlittrell_df.Year.isin(coachingyears)]
rodcarey_df = rodcarey_df [rodcarey_df.Year.isin(coachingyears)]
patfitzgerald_df = patfitzgerald_df [patfitzgerald_df.Year.isin(coachingyears)]
briankelly_df = briankelly_df [briankelly_df.Year.isin(coachingyears)]
franksolich_df = franksolich_df [franksolich_df.Year.isin(coachingyears)]
urbanmeyer_df = urbanmeyer_df [urbanmeyer_df.Year.isin(coachingyears)]
lincolnriley_df = lincolnriley_df [lincolnriley_df.Year.isin(coachingyears)]
mikegundy_df = mikegundy_df [mikegundy_df.Year.isin(coachingyears)]
bobbywilder_df = bobbywilder_df [bobbywilder_df.Year.isin(coachingyears)]
mariocristobal_df = mariocristobal_df [mariocristobal_df.Year.isin(coachingyears)]
jonathansmith_df = jonathansmith_df [jonathansmith_df.Year.isin(coachingyears)]
jamesfranklin_df = jamesfranklin_df [jamesfranklin_df.Year.isin(coachingyears)]
patnarduzzi_df = patnarduzzi_df [patnarduzzi_df.Year.isin(coachingyears)]
jeffbrohm_df = jeffbrohm_df [jeffbrohm_df.Year.isin(coachingyears)]
mikebloomgren_df = mikebloomgren_df [mikebloomgren_df.Year.isin(coachingyears)]
chrisash_df = chrisash_df [chrisash_df.Year.isin(coachingyears)]
rockylong_df = rockylong_df [rockylong_df.Year.isin(coachingyears)]
brentbrennan_df = brentbrennan_df [brentbrennan_df.Year.isin(coachingyears)]
stevecampbell_df = stevecampbell_df [stevecampbell_df.Year.isin(coachingyears)]
willmuschamp_df = willmuschamp_df [willmuschamp_df.Year.isin(coachingyears)]
charliestrong_df = charliestrong_df [charliestrong_df.Year.isin(coachingyears)]
clayhelton_df = clayhelton_df [clayhelton_df.Year.isin(coachingyears)]
sonnydykes_df = sonnydykes_df [sonnydykes_df.Year.isin(coachingyears)]
jayhopson_df = jayhopson_df [jayhopson_df.Year.isin(coachingyears)]
davidshaw_df = davidshaw_df [davidshaw_df.Year.isin(coachingyears)]
dinobabers_df = dinobabers_df [dinobabers_df.Year.isin(coachingyears)]
jeremypruitt_df = jeremypruitt_df [jeremypruitt_df.Year.isin(coachingyears)]
tomherman_df = tomherman_df [tomherman_df.Year.isin(coachingyears)]
jimbofisher_df = jimbofisher_df [jimbofisher_df.Year.isin(coachingyears)]
garypatterson_df = garypatterson_df [garypatterson_df.Year.isin(coachingyears)]
everettwithers_df = everettwithers_df [everettwithers_df.Year.isin(coachingyears)]
kliffkingsbury_df = kliffkingsbury_df [kliffkingsbury_df.Year.isin(coachingyears)]
danadimel_df = danadimel_df [danadimel_df.Year.isin(coachingyears)]
frankwilson_df = frankwilson_df [frankwilson_df.Year.isin(coachingyears)]
jasoncandle_df = jasoncandle_df [jasoncandle_df.Year.isin(coachingyears)]
nealbrown_df = nealbrown_df [nealbrown_df.Year.isin(coachingyears)]
williefritz_df = williefritz_df [williefritz_df.Year.isin(coachingyears)]
philipmontgomery_df = philipmontgomery_df [philipmontgomery_df.Year.isin(coachingyears)]
chipkelly_df = chipkelly_df [chipkelly_df.Year.isin(coachingyears)]
kylewhittingham_df = kylewhittingham_df [kylewhittingham_df.Year.isin(coachingyears)]
mattwells_df = mattwells_df [mattwells_df.Year.isin(coachingyears)]
derekmason_df = derekmason_df [derekmason_df.Year.isin(coachingyears)]
broncomendenhall_df = broncomendenhall_df [broncomendenhall_df.Year.isin(coachingyears)]
justinfuente_df = justinfuente_df [justinfuente_df.Year.isin(coachingyears)]
daveclawson_df = daveclawson_df [daveclawson_df.Year.isin(coachingyears)]
chrispetersen_df = chrispetersen_df [chrispetersen_df.Year.isin(coachingyears)]
mikeleach_df = mikeleach_df [mikeleach_df.Year.isin(coachingyears)]
danaholgorsen_df = danaholgorsen_df [danaholgorsen_df.Year.isin(coachingyears)]
mikesanford_df = mikesanford_df [mikesanford_df.Year.isin(coachingyears)]
timlester_df = timlester_df [timlester_df.Year.isin(coachingyears)]
paulchryst_df = paulchryst_df [paulchryst_df.Year.isin(coachingyears)]
craigbohl_df = craigbohl_df [craigbohl_df.Year.isin(coachingyears)]

#Retain specific columns
troycalhoun_df = troycalhoun_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
terrybowden_df = terrybowden_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
nicksaban_df = nicksaban_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
billclark_df = billclark_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
scottsatterfield_df = scottsatterfield_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
kevinsumlin_df = kevinsumlin_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
hermedwards_df = hermedwards_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
chadmorris_df = chadmorris_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
blakeanderson_df = blakeanderson_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
jeffmonken_df = jeffmonken_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
gusmalzahn_df = gusmalzahn_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
mikeneu_df = mikeneu_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
mattrhule_df = mattrhule_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
bryanharsin_df = bryanharsin_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
steveaddazio_df = steveaddazio_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
mikejinks_df = mikejinks_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
kalanisitake_df = kalanisitake_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
lanceleipold_df = lanceleipold_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
justinwilcox_df = justinwilcox_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
joshheupel_df = joshheupel_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
johnbonamego_df = johnbonamego_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
bradlambert_df = bradlambert_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
lukefickell_df = lukefickell_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
daboswinney_df = daboswinney_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
joemoglia_df = joemoglia_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
mikemacintyre_df = mikemacintyre_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
mikebobo_df = mikebobo_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
randyedsall_df = randyedsall_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
davidcutcliffe_df = davidcutcliffe_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
scottiemontgomery_df = scottiemontgomery_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
chriscreighton_df = chriscreighton_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
danmullen_df = danmullen_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
lanekiffin_df = lanekiffin_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
butchdavis_df = butchdavis_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
willietaggart_df = willietaggart_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
jefftedford_df = jefftedford_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
kirbysmart_df = kirbysmart_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
chadlunsford_df = chadlunsford_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
shawnelliott_df = shawnelliott_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
pauljohnson_df = pauljohnson_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
nickrolovich_df = nickrolovich_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
majorapplewhite_df = majorapplewhite_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
loviesmith_df = loviesmith_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
tomallen_df = tomallen_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
kirkferentz_df = kirkferentz_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
mattcampbell_df = mattcampbell_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
davidbeaty_df = davidbeaty_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
billsnyder_df = billsnyder_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
seanlewis_df = seanlewis_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
markstoops_df = markstoops_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
turnergill_df = turnergill_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
skipholtz_df = skipholtz_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
billynapier_df = billynapier_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
mattviator_df = mattviator_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
bobbypetrino_df = bobbypetrino_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
edorgeron_df = edorgeron_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
docholliday_df = docholliday_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
djdurkin_df = djdurkin_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
markwhipple_df = markwhipple_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
mikenorvell_df = mikenorvell_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
markricht_df = markricht_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
chuckmartin_df = chuckmartin_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
jimharbaugh_df = jimharbaugh_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
markdantonio_df = markdantonio_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
rickstockstill_df = rickstockstill_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
pjfleck_df = pjfleck_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
mattluke_df = mattluke_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
joemoorhead_df = joemoorhead_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
barryodom_df = barryodom_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
kenniumatalolo_df = kenniumatalolo_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
scottfrost_df = scottfrost_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
jaynorvell_df = jaynorvell_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
tonysanchez_df = tonysanchez_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
bobdavie_df = bobdavie_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
dougmartin_df = dougmartin_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
larryfedora_df = larryfedora_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
davedoeren_df = davedoeren_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
sethlittrell_df = sethlittrell_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
rodcarey_df = rodcarey_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
patfitzgerald_df = patfitzgerald_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
briankelly_df = briankelly_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
franksolich_df = franksolich_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
urbanmeyer_df = urbanmeyer_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
lincolnriley_df = lincolnriley_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
mikegundy_df = mikegundy_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
bobbywilder_df = bobbywilder_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
mariocristobal_df = mariocristobal_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
jonathansmith_df = jonathansmith_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
jamesfranklin_df = jamesfranklin_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
patnarduzzi_df = patnarduzzi_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
jeffbrohm_df = jeffbrohm_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
mikebloomgren_df = mikebloomgren_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
chrisash_df = chrisash_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
rockylong_df = rockylong_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
brentbrennan_df = brentbrennan_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
stevecampbell_df = stevecampbell_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
willmuschamp_df = willmuschamp_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
charliestrong_df = charliestrong_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
clayhelton_df = clayhelton_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
sonnydykes_df = sonnydykes_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
jayhopson_df = jayhopson_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
davidshaw_df = davidshaw_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
dinobabers_df = dinobabers_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
jeremypruitt_df = jeremypruitt_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
tomherman_df = tomherman_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
jimbofisher_df = jimbofisher_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
garypatterson_df = garypatterson_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
everettwithers_df = everettwithers_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
kliffkingsbury_df = kliffkingsbury_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
danadimel_df = danadimel_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
frankwilson_df = frankwilson_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
jasoncandle_df = jasoncandle_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
nealbrown_df = nealbrown_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
williefritz_df = williefritz_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
philipmontgomery_df = philipmontgomery_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
chipkelly_df = chipkelly_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
kylewhittingham_df = kylewhittingham_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
mattwells_df = mattwells_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
derekmason_df = derekmason_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
broncomendenhall_df = broncomendenhall_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
justinfuente_df = justinfuente_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
daveclawson_df = daveclawson_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
chrispetersen_df = chrispetersen_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
mikeleach_df = mikeleach_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
danaholgorsen_df = danaholgorsen_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
mikesanford_df = mikesanford_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
timlester_df = timlester_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
paulchryst_df = paulchryst_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]
craigbohl_df = craigbohl_df.loc[ :, ['Year', 'School', 'W', 'L', 'T', 'Pct', 'SRS', 'SOS', 'AP Post']]

#Rename columns
troycalhoun_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
terrybowden_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
nicksaban_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
billclark_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
scottsatterfield_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
kevinsumlin_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
hermedwards_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
chadmorris_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
blakeanderson_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
jeffmonken_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
gusmalzahn_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
mikeneu_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
mattrhule_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
bryanharsin_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
steveaddazio_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
mikejinks_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
kalanisitake_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
lanceleipold_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
justinwilcox_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
joshheupel_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
johnbonamego_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
bradlambert_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
lukefickell_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
daboswinney_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
joemoglia_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
mikemacintyre_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
mikebobo_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
randyedsall_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
davidcutcliffe_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
scottiemontgomery_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
chriscreighton_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
danmullen_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
lanekiffin_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
butchdavis_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
willietaggart_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
jefftedford_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
kirbysmart_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
chadlunsford_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
shawnelliott_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
pauljohnson_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
nickrolovich_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
majorapplewhite_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
loviesmith_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
tomallen_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
kirkferentz_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
mattcampbell_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
davidbeaty_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
billsnyder_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
seanlewis_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
markstoops_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
turnergill_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
skipholtz_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
billynapier_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
mattviator_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
bobbypetrino_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
edorgeron_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
docholliday_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
djdurkin_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
markwhipple_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
mikenorvell_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
markricht_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
chuckmartin_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
jimharbaugh_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
markdantonio_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
rickstockstill_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
pjfleck_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
mattluke_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
joemoorhead_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
barryodom_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
kenniumatalolo_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
scottfrost_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
jaynorvell_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
tonysanchez_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
bobdavie_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
dougmartin_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
larryfedora_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
davedoeren_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
sethlittrell_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
rodcarey_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
patfitzgerald_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
briankelly_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
franksolich_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
urbanmeyer_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
lincolnriley_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
mikegundy_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
bobbywilder_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
mariocristobal_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
jonathansmith_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
jamesfranklin_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
patnarduzzi_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
jeffbrohm_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
mikebloomgren_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
chrisash_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
rockylong_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
brentbrennan_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
stevecampbell_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
willmuschamp_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
charliestrong_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
clayhelton_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
sonnydykes_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
jayhopson_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
davidshaw_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
dinobabers_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
jeremypruitt_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
tomherman_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
jimbofisher_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
garypatterson_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
everettwithers_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
kliffkingsbury_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
danadimel_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
frankwilson_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
jasoncandle_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
nealbrown_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
williefritz_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
philipmontgomery_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
chipkelly_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
kylewhittingham_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
mattwells_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
derekmason_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
broncomendenhall_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
justinfuente_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
daveclawson_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
chrispetersen_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
mikeleach_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
danaholgorsen_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
mikesanford_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
timlester_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
paulchryst_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)
craigbohl_df.rename({'W': 'Wins', 'L': 'Losses', 'T':'Ties', 'AP Post':'APPost'}, axis=1, inplace=True)


#####################
###  AGGREGATION  ###
#####################

#Get sums of W,L,T and averages of SRS, SOS, and APPost
troycalhoun_totals = [troycalhoun_df.Wins.sum(),troycalhoun_df.Losses.sum(),troycalhoun_df.Ties.sum(),troycalhoun_df.SRS.mean(),troycalhoun_df.SOS.mean(),(troycalhoun_df.APPost <26).sum()]
terrybowden_totals = [terrybowden_df.Wins.sum(),terrybowden_df.Losses.sum(),terrybowden_df.Ties.sum(),terrybowden_df.SRS.mean(),terrybowden_df.SOS.mean(),(terrybowden_df.APPost <26).sum()]
nicksaban_totals = [nicksaban_df.Wins.sum(),nicksaban_df.Losses.sum(),nicksaban_df.Ties.sum(),nicksaban_df.SRS.mean(),nicksaban_df.SOS.mean(),(nicksaban_df.APPost <26).sum()]
billclark_totals = [billclark_df.Wins.sum(),billclark_df.Losses.sum(),billclark_df.Ties.sum(),billclark_df.SRS.mean(),billclark_df.SOS.mean(),(billclark_df.APPost <26).sum()]
scottsatterfield_totals = [scottsatterfield_df.Wins.sum(),scottsatterfield_df.Losses.sum(),scottsatterfield_df.Ties.sum(),scottsatterfield_df.SRS.mean(),scottsatterfield_df.SOS.mean(),(scottsatterfield_df.APPost <26).sum()]
kevinsumlin_totals = [kevinsumlin_df.Wins.sum(),kevinsumlin_df.Losses.sum(),kevinsumlin_df.Ties.sum(),kevinsumlin_df.SRS.mean(),kevinsumlin_df.SOS.mean(),(kevinsumlin_df.APPost <26).sum()]
hermedwards_totals = [hermedwards_df.Wins.sum(),hermedwards_df.Losses.sum(),hermedwards_df.Ties.sum(),hermedwards_df.SRS.mean(),hermedwards_df.SOS.mean(),(hermedwards_df.APPost <26).sum()]
chadmorris_totals = [chadmorris_df.Wins.sum(),chadmorris_df.Losses.sum(),chadmorris_df.Ties.sum(),chadmorris_df.SRS.mean(),chadmorris_df.SOS.mean(),(chadmorris_df.APPost <26).sum()]
blakeanderson_totals = [blakeanderson_df.Wins.sum(),blakeanderson_df.Losses.sum(),blakeanderson_df.Ties.sum(),blakeanderson_df.SRS.mean(),blakeanderson_df.SOS.mean(),(blakeanderson_df.APPost <26).sum()]
jeffmonken_totals = [jeffmonken_df.Wins.sum(),jeffmonken_df.Losses.sum(),jeffmonken_df.Ties.sum(),jeffmonken_df.SRS.mean(),jeffmonken_df.SOS.mean(),(jeffmonken_df.APPost <26).sum()]
gusmalzahn_totals = [gusmalzahn_df.Wins.sum(),gusmalzahn_df.Losses.sum(),gusmalzahn_df.Ties.sum(),gusmalzahn_df.SRS.mean(),gusmalzahn_df.SOS.mean(),(gusmalzahn_df.APPost <26).sum()]
mikeneu_totals = [mikeneu_df.Wins.sum(),mikeneu_df.Losses.sum(),mikeneu_df.Ties.sum(),mikeneu_df.SRS.mean(),mikeneu_df.SOS.mean(),(mikeneu_df.APPost <26).sum()]
mattrhule_totals = [mattrhule_df.Wins.sum(),mattrhule_df.Losses.sum(),mattrhule_df.Ties.sum(),mattrhule_df.SRS.mean(),mattrhule_df.SOS.mean(),(mattrhule_df.APPost <26).sum()]
bryanharsin_totals = [bryanharsin_df.Wins.sum(),bryanharsin_df.Losses.sum(),bryanharsin_df.Ties.sum(),bryanharsin_df.SRS.mean(),bryanharsin_df.SOS.mean(),(bryanharsin_df.APPost <26).sum()]
steveaddazio_totals = [steveaddazio_df.Wins.sum(),steveaddazio_df.Losses.sum(),steveaddazio_df.Ties.sum(),steveaddazio_df.SRS.mean(),steveaddazio_df.SOS.mean(),(steveaddazio_df.APPost <26).sum()]
mikejinks_totals = [mikejinks_df.Wins.sum(),mikejinks_df.Losses.sum(),mikejinks_df.Ties.sum(),mikejinks_df.SRS.mean(),mikejinks_df.SOS.mean(),(mikejinks_df.APPost <26).sum()]
kalanisitake_totals = [kalanisitake_df.Wins.sum(),kalanisitake_df.Losses.sum(),kalanisitake_df.Ties.sum(),kalanisitake_df.SRS.mean(),kalanisitake_df.SOS.mean(),(kalanisitake_df.APPost <26).sum()]
lanceleipold_totals = [lanceleipold_df.Wins.sum(),lanceleipold_df.Losses.sum(),lanceleipold_df.Ties.sum(),lanceleipold_df.SRS.mean(),lanceleipold_df.SOS.mean(),(lanceleipold_df.APPost <26).sum()]
justinwilcox_totals = [justinwilcox_df.Wins.sum(),justinwilcox_df.Losses.sum(),justinwilcox_df.Ties.sum(),justinwilcox_df.SRS.mean(),justinwilcox_df.SOS.mean(),(justinwilcox_df.APPost <26).sum()]
joshheupel_totals = [joshheupel_df.Wins.sum(),joshheupel_df.Losses.sum(),joshheupel_df.Ties.sum(),joshheupel_df.SRS.mean(),joshheupel_df.SOS.mean(),(joshheupel_df.APPost <26).sum()]
johnbonamego_totals = [johnbonamego_df.Wins.sum(),johnbonamego_df.Losses.sum(),johnbonamego_df.Ties.sum(),johnbonamego_df.SRS.mean(),johnbonamego_df.SOS.mean(),(johnbonamego_df.APPost <26).sum()]
bradlambert_totals = [bradlambert_df.Wins.sum(),bradlambert_df.Losses.sum(),bradlambert_df.Ties.sum(),bradlambert_df.SRS.mean(),bradlambert_df.SOS.mean(),(bradlambert_df.APPost <26).sum()]
lukefickell_totals = [lukefickell_df.Wins.sum(),lukefickell_df.Losses.sum(),lukefickell_df.Ties.sum(),lukefickell_df.SRS.mean(),lukefickell_df.SOS.mean(),(lukefickell_df.APPost <26).sum()]
daboswinney_totals = [daboswinney_df.Wins.sum(),daboswinney_df.Losses.sum(),daboswinney_df.Ties.sum(),daboswinney_df.SRS.mean(),daboswinney_df.SOS.mean(),(daboswinney_df.APPost <26).sum()]
joemoglia_totals = [joemoglia_df.Wins.sum(),joemoglia_df.Losses.sum(),joemoglia_df.Ties.sum(),joemoglia_df.SRS.mean(),joemoglia_df.SOS.mean(),(joemoglia_df.APPost <26).sum()]
mikemacintyre_totals = [mikemacintyre_df.Wins.sum(),mikemacintyre_df.Losses.sum(),mikemacintyre_df.Ties.sum(),mikemacintyre_df.SRS.mean(),mikemacintyre_df.SOS.mean(),(mikemacintyre_df.APPost <26).sum()]
mikebobo_totals = [mikebobo_df.Wins.sum(),mikebobo_df.Losses.sum(),mikebobo_df.Ties.sum(),mikebobo_df.SRS.mean(),mikebobo_df.SOS.mean(),(mikebobo_df.APPost <26).sum()]
randyedsall_totals = [randyedsall_df.Wins.sum(),randyedsall_df.Losses.sum(),randyedsall_df.Ties.sum(),randyedsall_df.SRS.mean(),randyedsall_df.SOS.mean(),(randyedsall_df.APPost <26).sum()]
davidcutcliffe_totals = [davidcutcliffe_df.Wins.sum(),davidcutcliffe_df.Losses.sum(),davidcutcliffe_df.Ties.sum(),davidcutcliffe_df.SRS.mean(),davidcutcliffe_df.SOS.mean(),(davidcutcliffe_df.APPost <26).sum()]
scottiemontgomery_totals = [scottiemontgomery_df.Wins.sum(),scottiemontgomery_df.Losses.sum(),scottiemontgomery_df.Ties.sum(),scottiemontgomery_df.SRS.mean(),scottiemontgomery_df.SOS.mean(),(scottiemontgomery_df.APPost <26).sum()]
chriscreighton_totals = [chriscreighton_df.Wins.sum(),chriscreighton_df.Losses.sum(),chriscreighton_df.Ties.sum(),chriscreighton_df.SRS.mean(),chriscreighton_df.SOS.mean(),(chriscreighton_df.APPost <26).sum()]
danmullen_totals = [danmullen_df.Wins.sum(),danmullen_df.Losses.sum(),danmullen_df.Ties.sum(),danmullen_df.SRS.mean(),danmullen_df.SOS.mean(),(danmullen_df.APPost <26).sum()]
lanekiffin_totals = [lanekiffin_df.Wins.sum(),lanekiffin_df.Losses.sum(),lanekiffin_df.Ties.sum(),lanekiffin_df.SRS.mean(),lanekiffin_df.SOS.mean(),(lanekiffin_df.APPost <26).sum()]
butchdavis_totals = [butchdavis_df.Wins.sum(),butchdavis_df.Losses.sum(),butchdavis_df.Ties.sum(),butchdavis_df.SRS.mean(),butchdavis_df.SOS.mean(),(butchdavis_df.APPost <26).sum()]
willietaggart_totals = [willietaggart_df.Wins.sum(),willietaggart_df.Losses.sum(),willietaggart_df.Ties.sum(),willietaggart_df.SRS.mean(),willietaggart_df.SOS.mean(),(willietaggart_df.APPost <26).sum()]
jefftedford_totals = [jefftedford_df.Wins.sum(),jefftedford_df.Losses.sum(),jefftedford_df.Ties.sum(),jefftedford_df.SRS.mean(),jefftedford_df.SOS.mean(),(jefftedford_df.APPost <26).sum()]
kirbysmart_totals = [kirbysmart_df.Wins.sum(),kirbysmart_df.Losses.sum(),kirbysmart_df.Ties.sum(),kirbysmart_df.SRS.mean(),kirbysmart_df.SOS.mean(),(kirbysmart_df.APPost <26).sum()]
chadlunsford_totals = [chadlunsford_df.Wins.sum(),chadlunsford_df.Losses.sum(),chadlunsford_df.Ties.sum(),chadlunsford_df.SRS.mean(),chadlunsford_df.SOS.mean(),(chadlunsford_df.APPost <26).sum()]
shawnelliott_totals = [shawnelliott_df.Wins.sum(),shawnelliott_df.Losses.sum(),shawnelliott_df.Ties.sum(),shawnelliott_df.SRS.mean(),shawnelliott_df.SOS.mean(),(shawnelliott_df.APPost <26).sum()]
pauljohnson_totals = [pauljohnson_df.Wins.sum(),pauljohnson_df.Losses.sum(),pauljohnson_df.Ties.sum(),pauljohnson_df.SRS.mean(),pauljohnson_df.SOS.mean(),(pauljohnson_df.APPost <26).sum()]
nickrolovich_totals = [nickrolovich_df.Wins.sum(),nickrolovich_df.Losses.sum(),nickrolovich_df.Ties.sum(),nickrolovich_df.SRS.mean(),nickrolovich_df.SOS.mean(),(nickrolovich_df.APPost <26).sum()]
majorapplewhite_totals = [majorapplewhite_df.Wins.sum(),majorapplewhite_df.Losses.sum(),majorapplewhite_df.Ties.sum(),majorapplewhite_df.SRS.mean(),majorapplewhite_df.SOS.mean(),(majorapplewhite_df.APPost <26).sum()]
loviesmith_totals = [loviesmith_df.Wins.sum(),loviesmith_df.Losses.sum(),loviesmith_df.Ties.sum(),loviesmith_df.SRS.mean(),loviesmith_df.SOS.mean(),(loviesmith_df.APPost <26).sum()]
tomallen_totals = [tomallen_df.Wins.sum(),tomallen_df.Losses.sum(),tomallen_df.Ties.sum(),tomallen_df.SRS.mean(),tomallen_df.SOS.mean(),(tomallen_df.APPost <26).sum()]
kirkferentz_totals = [kirkferentz_df.Wins.sum(),kirkferentz_df.Losses.sum(),kirkferentz_df.Ties.sum(),kirkferentz_df.SRS.mean(),kirkferentz_df.SOS.mean(),(kirkferentz_df.APPost <26).sum()]
mattcampbell_totals = [mattcampbell_df.Wins.sum(),mattcampbell_df.Losses.sum(),mattcampbell_df.Ties.sum(),mattcampbell_df.SRS.mean(),mattcampbell_df.SOS.mean(),(mattcampbell_df.APPost <26).sum()]
davidbeaty_totals = [davidbeaty_df.Wins.sum(),davidbeaty_df.Losses.sum(),davidbeaty_df.Ties.sum(),davidbeaty_df.SRS.mean(),davidbeaty_df.SOS.mean(),(davidbeaty_df.APPost <26).sum()]
billsnyder_totals = [billsnyder_df.Wins.sum(),billsnyder_df.Losses.sum(),billsnyder_df.Ties.sum(),billsnyder_df.SRS.mean(),billsnyder_df.SOS.mean(),(billsnyder_df.APPost <26).sum()]
seanlewis_totals = [seanlewis_df.Wins.sum(),seanlewis_df.Losses.sum(),seanlewis_df.Ties.sum(),seanlewis_df.SRS.mean(),seanlewis_df.SOS.mean(),(seanlewis_df.APPost <26).sum()]
markstoops_totals = [markstoops_df.Wins.sum(),markstoops_df.Losses.sum(),markstoops_df.Ties.sum(),markstoops_df.SRS.mean(),markstoops_df.SOS.mean(),(markstoops_df.APPost <26).sum()]
turnergill_totals = [turnergill_df.Wins.sum(),turnergill_df.Losses.sum(),turnergill_df.Ties.sum(),turnergill_df.SRS.mean(),turnergill_df.SOS.mean(),(turnergill_df.APPost <26).sum()]
skipholtz_totals = [skipholtz_df.Wins.sum(),skipholtz_df.Losses.sum(),skipholtz_df.Ties.sum(),skipholtz_df.SRS.mean(),skipholtz_df.SOS.mean(),(skipholtz_df.APPost <26).sum()]
billynapier_totals = [billynapier_df.Wins.sum(),billynapier_df.Losses.sum(),billynapier_df.Ties.sum(),billynapier_df.SRS.mean(),billynapier_df.SOS.mean(),(billynapier_df.APPost <26).sum()]
mattviator_totals = [mattviator_df.Wins.sum(),mattviator_df.Losses.sum(),mattviator_df.Ties.sum(),mattviator_df.SRS.mean(),mattviator_df.SOS.mean(),(mattviator_df.APPost <26).sum()]
bobbypetrino_totals = [bobbypetrino_df.Wins.sum(),bobbypetrino_df.Losses.sum(),bobbypetrino_df.Ties.sum(),bobbypetrino_df.SRS.mean(),bobbypetrino_df.SOS.mean(),(bobbypetrino_df.APPost <26).sum()]
edorgeron_totals = [edorgeron_df.Wins.sum(),edorgeron_df.Losses.sum(),edorgeron_df.Ties.sum(),edorgeron_df.SRS.mean(),edorgeron_df.SOS.mean(),(edorgeron_df.APPost <26).sum()]
docholliday_totals = [docholliday_df.Wins.sum(),docholliday_df.Losses.sum(),docholliday_df.Ties.sum(),docholliday_df.SRS.mean(),docholliday_df.SOS.mean(),(docholliday_df.APPost <26).sum()]
djdurkin_totals = [djdurkin_df.Wins.sum(),djdurkin_df.Losses.sum(),djdurkin_df.Ties.sum(),djdurkin_df.SRS.mean(),djdurkin_df.SOS.mean(),(djdurkin_df.APPost <26).sum()]
markwhipple_totals = [markwhipple_df.Wins.sum(),markwhipple_df.Losses.sum(),markwhipple_df.Ties.sum(),markwhipple_df.SRS.mean(),markwhipple_df.SOS.mean(),(markwhipple_df.APPost <26).sum()]
mikenorvell_totals = [mikenorvell_df.Wins.sum(),mikenorvell_df.Losses.sum(),mikenorvell_df.Ties.sum(),mikenorvell_df.SRS.mean(),mikenorvell_df.SOS.mean(),(mikenorvell_df.APPost <26).sum()]
markricht_totals = [markricht_df.Wins.sum(),markricht_df.Losses.sum(),markricht_df.Ties.sum(),markricht_df.SRS.mean(),markricht_df.SOS.mean(),(markricht_df.APPost <26).sum()]
chuckmartin_totals = [chuckmartin_df.Wins.sum(),chuckmartin_df.Losses.sum(),chuckmartin_df.Ties.sum(),chuckmartin_df.SRS.mean(),chuckmartin_df.SOS.mean(),(chuckmartin_df.APPost <26).sum()]
jimharbaugh_totals = [jimharbaugh_df.Wins.sum(),jimharbaugh_df.Losses.sum(),jimharbaugh_df.Ties.sum(),jimharbaugh_df.SRS.mean(),jimharbaugh_df.SOS.mean(),(jimharbaugh_df.APPost <26).sum()]
markdantonio_totals = [markdantonio_df.Wins.sum(),markdantonio_df.Losses.sum(),markdantonio_df.Ties.sum(),markdantonio_df.SRS.mean(),markdantonio_df.SOS.mean(),(markdantonio_df.APPost <26).sum()]
rickstockstill_totals = [rickstockstill_df.Wins.sum(),rickstockstill_df.Losses.sum(),rickstockstill_df.Ties.sum(),rickstockstill_df.SRS.mean(),rickstockstill_df.SOS.mean(),(rickstockstill_df.APPost <26).sum()]
pjfleck_totals = [pjfleck_df.Wins.sum(),pjfleck_df.Losses.sum(),pjfleck_df.Ties.sum(),pjfleck_df.SRS.mean(),pjfleck_df.SOS.mean(),(pjfleck_df.APPost <26).sum()]
mattluke_totals = [mattluke_df.Wins.sum(),mattluke_df.Losses.sum(),mattluke_df.Ties.sum(),mattluke_df.SRS.mean(),mattluke_df.SOS.mean(),(mattluke_df.APPost <26).sum()]
joemoorhead_totals = [joemoorhead_df.Wins.sum(),joemoorhead_df.Losses.sum(),joemoorhead_df.Ties.sum(),joemoorhead_df.SRS.mean(),joemoorhead_df.SOS.mean(),(joemoorhead_df.APPost <26).sum()]
barryodom_totals = [barryodom_df.Wins.sum(),barryodom_df.Losses.sum(),barryodom_df.Ties.sum(),barryodom_df.SRS.mean(),barryodom_df.SOS.mean(),(barryodom_df.APPost <26).sum()]
kenniumatalolo_totals = [kenniumatalolo_df.Wins.sum(),kenniumatalolo_df.Losses.sum(),kenniumatalolo_df.Ties.sum(),kenniumatalolo_df.SRS.mean(),kenniumatalolo_df.SOS.mean(),(kenniumatalolo_df.APPost <26).sum()]
scottfrost_totals = [scottfrost_df.Wins.sum(),scottfrost_df.Losses.sum(),scottfrost_df.Ties.sum(),scottfrost_df.SRS.mean(),scottfrost_df.SOS.mean(),(scottfrost_df.APPost <26).sum()]
jaynorvell_totals = [jaynorvell_df.Wins.sum(),jaynorvell_df.Losses.sum(),jaynorvell_df.Ties.sum(),jaynorvell_df.SRS.mean(),jaynorvell_df.SOS.mean(),(jaynorvell_df.APPost <26).sum()]
tonysanchez_totals = [tonysanchez_df.Wins.sum(),tonysanchez_df.Losses.sum(),tonysanchez_df.Ties.sum(),tonysanchez_df.SRS.mean(),tonysanchez_df.SOS.mean(),(tonysanchez_df.APPost <26).sum()]
bobdavie_totals = [bobdavie_df.Wins.sum(),bobdavie_df.Losses.sum(),bobdavie_df.Ties.sum(),bobdavie_df.SRS.mean(),bobdavie_df.SOS.mean(),(bobdavie_df.APPost <26).sum()]
dougmartin_totals = [dougmartin_df.Wins.sum(),dougmartin_df.Losses.sum(),dougmartin_df.Ties.sum(),dougmartin_df.SRS.mean(),dougmartin_df.SOS.mean(),(dougmartin_df.APPost <26).sum()]
larryfedora_totals = [larryfedora_df.Wins.sum(),larryfedora_df.Losses.sum(),larryfedora_df.Ties.sum(),larryfedora_df.SRS.mean(),larryfedora_df.SOS.mean(),(larryfedora_df.APPost <26).sum()]
davedoeren_totals = [davedoeren_df.Wins.sum(),davedoeren_df.Losses.sum(),davedoeren_df.Ties.sum(),davedoeren_df.SRS.mean(),davedoeren_df.SOS.mean(),(davedoeren_df.APPost <26).sum()]
sethlittrell_totals = [sethlittrell_df.Wins.sum(),sethlittrell_df.Losses.sum(),sethlittrell_df.Ties.sum(),sethlittrell_df.SRS.mean(),sethlittrell_df.SOS.mean(),(sethlittrell_df.APPost <26).sum()]
rodcarey_totals = [rodcarey_df.Wins.sum(),rodcarey_df.Losses.sum(),rodcarey_df.Ties.sum(),rodcarey_df.SRS.mean(),rodcarey_df.SOS.mean(),(rodcarey_df.APPost <26).sum()]
patfitzgerald_totals = [patfitzgerald_df.Wins.sum(),patfitzgerald_df.Losses.sum(),patfitzgerald_df.Ties.sum(),patfitzgerald_df.SRS.mean(),patfitzgerald_df.SOS.mean(),(patfitzgerald_df.APPost <26).sum()]
briankelly_totals = [briankelly_df.Wins.sum(),briankelly_df.Losses.sum(),briankelly_df.Ties.sum(),briankelly_df.SRS.mean(),briankelly_df.SOS.mean(),(briankelly_df.APPost <26).sum()]
franksolich_totals = [franksolich_df.Wins.sum(),franksolich_df.Losses.sum(),franksolich_df.Ties.sum(),franksolich_df.SRS.mean(),franksolich_df.SOS.mean(),(franksolich_df.APPost <26).sum()]
urbanmeyer_totals = [urbanmeyer_df.Wins.sum(),urbanmeyer_df.Losses.sum(),urbanmeyer_df.Ties.sum(),urbanmeyer_df.SRS.mean(),urbanmeyer_df.SOS.mean(),(urbanmeyer_df.APPost <26).sum()]
lincolnriley_totals = [lincolnriley_df.Wins.sum(),lincolnriley_df.Losses.sum(),lincolnriley_df.Ties.sum(),lincolnriley_df.SRS.mean(),lincolnriley_df.SOS.mean(),(lincolnriley_df.APPost <26).sum()]
mikegundy_totals = [mikegundy_df.Wins.sum(),mikegundy_df.Losses.sum(),mikegundy_df.Ties.sum(),mikegundy_df.SRS.mean(),mikegundy_df.SOS.mean(),(mikegundy_df.APPost <26).sum()]
bobbywilder_totals = [bobbywilder_df.Wins.sum(),bobbywilder_df.Losses.sum(),bobbywilder_df.Ties.sum(),bobbywilder_df.SRS.mean(),bobbywilder_df.SOS.mean(),(bobbywilder_df.APPost <26).sum()]
mariocristobal_totals = [mariocristobal_df.Wins.sum(),mariocristobal_df.Losses.sum(),mariocristobal_df.Ties.sum(),mariocristobal_df.SRS.mean(),mariocristobal_df.SOS.mean(),(mariocristobal_df.APPost <26).sum()]
jonathansmith_totals = [jonathansmith_df.Wins.sum(),jonathansmith_df.Losses.sum(),jonathansmith_df.Ties.sum(),jonathansmith_df.SRS.mean(),jonathansmith_df.SOS.mean(),(jonathansmith_df.APPost <26).sum()]
jamesfranklin_totals = [jamesfranklin_df.Wins.sum(),jamesfranklin_df.Losses.sum(),jamesfranklin_df.Ties.sum(),jamesfranklin_df.SRS.mean(),jamesfranklin_df.SOS.mean(),(jamesfranklin_df.APPost <26).sum()]
patnarduzzi_totals = [patnarduzzi_df.Wins.sum(),patnarduzzi_df.Losses.sum(),patnarduzzi_df.Ties.sum(),patnarduzzi_df.SRS.mean(),patnarduzzi_df.SOS.mean(),(patnarduzzi_df.APPost <26).sum()]
jeffbrohm_totals = [jeffbrohm_df.Wins.sum(),jeffbrohm_df.Losses.sum(),jeffbrohm_df.Ties.sum(),jeffbrohm_df.SRS.mean(),jeffbrohm_df.SOS.mean(),(jeffbrohm_df.APPost <26).sum()]
mikebloomgren_totals = [mikebloomgren_df.Wins.sum(),mikebloomgren_df.Losses.sum(),mikebloomgren_df.Ties.sum(),mikebloomgren_df.SRS.mean(),mikebloomgren_df.SOS.mean(),(mikebloomgren_df.APPost <26).sum()]
chrisash_totals = [chrisash_df.Wins.sum(),chrisash_df.Losses.sum(),chrisash_df.Ties.sum(),chrisash_df.SRS.mean(),chrisash_df.SOS.mean(),(chrisash_df.APPost <26).sum()]
rockylong_totals = [rockylong_df.Wins.sum(),rockylong_df.Losses.sum(),rockylong_df.Ties.sum(),rockylong_df.SRS.mean(),rockylong_df.SOS.mean(),(rockylong_df.APPost <26).sum()]
brentbrennan_totals = [brentbrennan_df.Wins.sum(),brentbrennan_df.Losses.sum(),brentbrennan_df.Ties.sum(),brentbrennan_df.SRS.mean(),brentbrennan_df.SOS.mean(),(brentbrennan_df.APPost <26).sum()]
stevecampbell_totals = [stevecampbell_df.Wins.sum(),stevecampbell_df.Losses.sum(),stevecampbell_df.Ties.sum(),stevecampbell_df.SRS.mean(),stevecampbell_df.SOS.mean(),(stevecampbell_df.APPost <26).sum()]
willmuschamp_totals = [willmuschamp_df.Wins.sum(),willmuschamp_df.Losses.sum(),willmuschamp_df.Ties.sum(),willmuschamp_df.SRS.mean(),willmuschamp_df.SOS.mean(),(willmuschamp_df.APPost <26).sum()]
charliestrong_totals = [charliestrong_df.Wins.sum(),charliestrong_df.Losses.sum(),charliestrong_df.Ties.sum(),charliestrong_df.SRS.mean(),charliestrong_df.SOS.mean(),(charliestrong_df.APPost <26).sum()]
clayhelton_totals = [clayhelton_df.Wins.sum(),clayhelton_df.Losses.sum(),clayhelton_df.Ties.sum(),clayhelton_df.SRS.mean(),clayhelton_df.SOS.mean(),(clayhelton_df.APPost <26).sum()]
sonnydykes_totals = [sonnydykes_df.Wins.sum(),sonnydykes_df.Losses.sum(),sonnydykes_df.Ties.sum(),sonnydykes_df.SRS.mean(),sonnydykes_df.SOS.mean(),(sonnydykes_df.APPost <26).sum()]
jayhopson_totals = [jayhopson_df.Wins.sum(),jayhopson_df.Losses.sum(),jayhopson_df.Ties.sum(),jayhopson_df.SRS.mean(),jayhopson_df.SOS.mean(),(jayhopson_df.APPost <26).sum()]
davidshaw_totals = [davidshaw_df.Wins.sum(),davidshaw_df.Losses.sum(),davidshaw_df.Ties.sum(),davidshaw_df.SRS.mean(),davidshaw_df.SOS.mean(),(davidshaw_df.APPost <26).sum()]
dinobabers_totals = [dinobabers_df.Wins.sum(),dinobabers_df.Losses.sum(),dinobabers_df.Ties.sum(),dinobabers_df.SRS.mean(),dinobabers_df.SOS.mean(),(dinobabers_df.APPost <26).sum()]
jeremypruitt_totals = [jeremypruitt_df.Wins.sum(),jeremypruitt_df.Losses.sum(),jeremypruitt_df.Ties.sum(),jeremypruitt_df.SRS.mean(),jeremypruitt_df.SOS.mean(),(jeremypruitt_df.APPost <26).sum()]
tomherman_totals = [tomherman_df.Wins.sum(),tomherman_df.Losses.sum(),tomherman_df.Ties.sum(),tomherman_df.SRS.mean(),tomherman_df.SOS.mean(),(tomherman_df.APPost <26).sum()]
jimbofisher_totals = [jimbofisher_df.Wins.sum(),jimbofisher_df.Losses.sum(),jimbofisher_df.Ties.sum(),jimbofisher_df.SRS.mean(),jimbofisher_df.SOS.mean(),(jimbofisher_df.APPost <26).sum()]
garypatterson_totals = [garypatterson_df.Wins.sum(),garypatterson_df.Losses.sum(),garypatterson_df.Ties.sum(),garypatterson_df.SRS.mean(),garypatterson_df.SOS.mean(),(garypatterson_df.APPost <26).sum()]
everettwithers_totals = [everettwithers_df.Wins.sum(),everettwithers_df.Losses.sum(),everettwithers_df.Ties.sum(),everettwithers_df.SRS.mean(),everettwithers_df.SOS.mean(),(everettwithers_df.APPost <26).sum()]
kliffkingsbury_totals = [kliffkingsbury_df.Wins.sum(),kliffkingsbury_df.Losses.sum(),kliffkingsbury_df.Ties.sum(),kliffkingsbury_df.SRS.mean(),kliffkingsbury_df.SOS.mean(),(kliffkingsbury_df.APPost <26).sum()]
danadimel_totals = [danadimel_df.Wins.sum(),danadimel_df.Losses.sum(),danadimel_df.Ties.sum(),danadimel_df.SRS.mean(),danadimel_df.SOS.mean(),(danadimel_df.APPost <26).sum()]
frankwilson_totals = [frankwilson_df.Wins.sum(),frankwilson_df.Losses.sum(),frankwilson_df.Ties.sum(),frankwilson_df.SRS.mean(),frankwilson_df.SOS.mean(),(frankwilson_df.APPost <26).sum()]
jasoncandle_totals = [jasoncandle_df.Wins.sum(),jasoncandle_df.Losses.sum(),jasoncandle_df.Ties.sum(),jasoncandle_df.SRS.mean(),jasoncandle_df.SOS.mean(),(jasoncandle_df.APPost <26).sum()]
nealbrown_totals = [nealbrown_df.Wins.sum(),nealbrown_df.Losses.sum(),nealbrown_df.Ties.sum(),nealbrown_df.SRS.mean(),nealbrown_df.SOS.mean(),(nealbrown_df.APPost <26).sum()]
williefritz_totals = [williefritz_df.Wins.sum(),williefritz_df.Losses.sum(),williefritz_df.Ties.sum(),williefritz_df.SRS.mean(),williefritz_df.SOS.mean(),(williefritz_df.APPost <26).sum()]
philipmontgomery_totals = [philipmontgomery_df.Wins.sum(),philipmontgomery_df.Losses.sum(),philipmontgomery_df.Ties.sum(),philipmontgomery_df.SRS.mean(),philipmontgomery_df.SOS.mean(),(philipmontgomery_df.APPost <26).sum()]
chipkelly_totals = [chipkelly_df.Wins.sum(),chipkelly_df.Losses.sum(),chipkelly_df.Ties.sum(),chipkelly_df.SRS.mean(),chipkelly_df.SOS.mean(),(chipkelly_df.APPost <26).sum()]
kylewhittingham_totals = [kylewhittingham_df.Wins.sum(),kylewhittingham_df.Losses.sum(),kylewhittingham_df.Ties.sum(),kylewhittingham_df.SRS.mean(),kylewhittingham_df.SOS.mean(),(kylewhittingham_df.APPost <26).sum()]
mattwells_totals = [mattwells_df.Wins.sum(),mattwells_df.Losses.sum(),mattwells_df.Ties.sum(),mattwells_df.SRS.mean(),mattwells_df.SOS.mean(),(mattwells_df.APPost <26).sum()]
derekmason_totals = [derekmason_df.Wins.sum(),derekmason_df.Losses.sum(),derekmason_df.Ties.sum(),derekmason_df.SRS.mean(),derekmason_df.SOS.mean(),(derekmason_df.APPost <26).sum()]
broncomendenhall_totals = [broncomendenhall_df.Wins.sum(),broncomendenhall_df.Losses.sum(),broncomendenhall_df.Ties.sum(),broncomendenhall_df.SRS.mean(),broncomendenhall_df.SOS.mean(),(broncomendenhall_df.APPost <26).sum()]
justinfuente_totals = [justinfuente_df.Wins.sum(),justinfuente_df.Losses.sum(),justinfuente_df.Ties.sum(),justinfuente_df.SRS.mean(),justinfuente_df.SOS.mean(),(justinfuente_df.APPost <26).sum()]
daveclawson_totals = [daveclawson_df.Wins.sum(),daveclawson_df.Losses.sum(),daveclawson_df.Ties.sum(),daveclawson_df.SRS.mean(),daveclawson_df.SOS.mean(),(daveclawson_df.APPost <26).sum()]
chrispetersen_totals = [chrispetersen_df.Wins.sum(),chrispetersen_df.Losses.sum(),chrispetersen_df.Ties.sum(),chrispetersen_df.SRS.mean(),chrispetersen_df.SOS.mean(),(chrispetersen_df.APPost <26).sum()]
mikeleach_totals = [mikeleach_df.Wins.sum(),mikeleach_df.Losses.sum(),mikeleach_df.Ties.sum(),mikeleach_df.SRS.mean(),mikeleach_df.SOS.mean(),(mikeleach_df.APPost <26).sum()]
danaholgorsen_totals = [danaholgorsen_df.Wins.sum(),danaholgorsen_df.Losses.sum(),danaholgorsen_df.Ties.sum(),danaholgorsen_df.SRS.mean(),danaholgorsen_df.SOS.mean(),(danaholgorsen_df.APPost < 16).sum()]
mikesanford_totals = [mikesanford_df.Wins.sum(),mikesanford_df.Losses.sum(),mikesanford_df.Ties.sum(),mikesanford_df.SRS.mean(),mikesanford_df.SOS.mean(),(mikesanford_df.APPost <26).sum()]
timlester_totals = [timlester_df.Wins.sum(),timlester_df.Losses.sum(),timlester_df.Ties.sum(),timlester_df.SRS.mean(),timlester_df.SOS.mean(),(timlester_df.APPost <26).sum()]
paulchryst_totals = [paulchryst_df.Wins.sum(),paulchryst_df.Losses.sum(),paulchryst_df.Ties.sum(),paulchryst_df.SRS.mean(),paulchryst_df.SOS.mean(),(paulchryst_df.APPost <26).sum()]
craigbohl_totals = [craigbohl_df.Wins.sum(),craigbohl_df.Losses.sum(),craigbohl_df.Ties.sum(),craigbohl_df.SRS.mean(),craigbohl_df.SOS.mean(),(craigbohl_df.APPost <26).sum()]

#Convert to a dataframe
troycalhoun_totals = DataFrame(troycalhoun_totals)
terrybowden_totals = DataFrame(terrybowden_totals)
nicksaban_totals = DataFrame(nicksaban_totals)
billclark_totals = DataFrame(billclark_totals)
scottsatterfield_totals = DataFrame(scottsatterfield_totals)
kevinsumlin_totals = DataFrame(kevinsumlin_totals)
hermedwards_totals = DataFrame(hermedwards_totals)
chadmorris_totals = DataFrame(chadmorris_totals)
blakeanderson_totals = DataFrame(blakeanderson_totals)
jeffmonken_totals = DataFrame(jeffmonken_totals)
gusmalzahn_totals = DataFrame(gusmalzahn_totals)
mikeneu_totals = DataFrame(mikeneu_totals)
mattrhule_totals = DataFrame(mattrhule_totals)
bryanharsin_totals = DataFrame(bryanharsin_totals)
steveaddazio_totals = DataFrame(steveaddazio_totals)
mikejinks_totals = DataFrame(mikejinks_totals)
kalanisitake_totals = DataFrame(kalanisitake_totals)
lanceleipold_totals = DataFrame(lanceleipold_totals)
justinwilcox_totals = DataFrame(justinwilcox_totals)
joshheupel_totals = DataFrame(joshheupel_totals)
johnbonamego_totals = DataFrame(johnbonamego_totals)
bradlambert_totals = DataFrame(bradlambert_totals)
lukefickell_totals = DataFrame(lukefickell_totals)
daboswinney_totals = DataFrame(daboswinney_totals)
joemoglia_totals = DataFrame(joemoglia_totals)
mikemacintyre_totals = DataFrame(mikemacintyre_totals)
mikebobo_totals = DataFrame(mikebobo_totals)
randyedsall_totals = DataFrame(randyedsall_totals)
davidcutcliffe_totals = DataFrame(davidcutcliffe_totals)
scottiemontgomery_totals = DataFrame(scottiemontgomery_totals)
chriscreighton_totals = DataFrame(chriscreighton_totals)
danmullen_totals = DataFrame(danmullen_totals)
lanekiffin_totals = DataFrame(lanekiffin_totals)
butchdavis_totals = DataFrame(butchdavis_totals)
willietaggart_totals = DataFrame(willietaggart_totals)
jefftedford_totals = DataFrame(jefftedford_totals)
kirbysmart_totals = DataFrame(kirbysmart_totals)
chadlunsford_totals = DataFrame(chadlunsford_totals)
shawnelliott_totals = DataFrame(shawnelliott_totals)
pauljohnson_totals = DataFrame(pauljohnson_totals)
nickrolovich_totals = DataFrame(nickrolovich_totals)
majorapplewhite_totals = DataFrame(majorapplewhite_totals)
loviesmith_totals = DataFrame(loviesmith_totals)
tomallen_totals = DataFrame(tomallen_totals)
kirkferentz_totals = DataFrame(kirkferentz_totals)
mattcampbell_totals = DataFrame(mattcampbell_totals)
davidbeaty_totals = DataFrame(davidbeaty_totals)
billsnyder_totals = DataFrame(billsnyder_totals)
seanlewis_totals = DataFrame(seanlewis_totals)
markstoops_totals = DataFrame(markstoops_totals)
turnergill_totals = DataFrame(turnergill_totals)
skipholtz_totals = DataFrame(skipholtz_totals)
billynapier_totals = DataFrame(billynapier_totals)
mattviator_totals = DataFrame(mattviator_totals)
bobbypetrino_totals = DataFrame(bobbypetrino_totals)
edorgeron_totals = DataFrame(edorgeron_totals)
docholliday_totals = DataFrame(docholliday_totals)
djdurkin_totals = DataFrame(djdurkin_totals)
markwhipple_totals = DataFrame(markwhipple_totals)
mikenorvell_totals = DataFrame(mikenorvell_totals)
markricht_totals = DataFrame(markricht_totals)
chuckmartin_totals = DataFrame(chuckmartin_totals)
jimharbaugh_totals = DataFrame(jimharbaugh_totals)
markdantonio_totals = DataFrame(markdantonio_totals)
rickstockstill_totals = DataFrame(rickstockstill_totals)
pjfleck_totals = DataFrame(pjfleck_totals)
mattluke_totals = DataFrame(mattluke_totals)
joemoorhead_totals = DataFrame(joemoorhead_totals)
barryodom_totals = DataFrame(barryodom_totals)
kenniumatalolo_totals = DataFrame(kenniumatalolo_totals)
scottfrost_totals = DataFrame(scottfrost_totals)
jaynorvell_totals = DataFrame(jaynorvell_totals)
tonysanchez_totals = DataFrame(tonysanchez_totals)
bobdavie_totals = DataFrame(bobdavie_totals)
dougmartin_totals = DataFrame(dougmartin_totals)
larryfedora_totals = DataFrame(larryfedora_totals)
davedoeren_totals = DataFrame(davedoeren_totals)
sethlittrell_totals = DataFrame(sethlittrell_totals)
rodcarey_totals = DataFrame(rodcarey_totals)
patfitzgerald_totals = DataFrame(patfitzgerald_totals)
briankelly_totals = DataFrame(briankelly_totals)
franksolich_totals = DataFrame(franksolich_totals)
urbanmeyer_totals = DataFrame(urbanmeyer_totals)
lincolnriley_totals = DataFrame(lincolnriley_totals)
mikegundy_totals = DataFrame(mikegundy_totals)
bobbywilder_totals = DataFrame(bobbywilder_totals)
mariocristobal_totals = DataFrame(mariocristobal_totals)
jonathansmith_totals = DataFrame(jonathansmith_totals)
jamesfranklin_totals = DataFrame(jamesfranklin_totals)
patnarduzzi_totals = DataFrame(patnarduzzi_totals)
jeffbrohm_totals = DataFrame(jeffbrohm_totals)
mikebloomgren_totals = DataFrame(mikebloomgren_totals)
chrisash_totals = DataFrame(chrisash_totals)
rockylong_totals = DataFrame(rockylong_totals)
brentbrennan_totals = DataFrame(brentbrennan_totals)
stevecampbell_totals = DataFrame(stevecampbell_totals)
willmuschamp_totals = DataFrame(willmuschamp_totals)
charliestrong_totals = DataFrame(charliestrong_totals)
clayhelton_totals = DataFrame(clayhelton_totals)
sonnydykes_totals = DataFrame(sonnydykes_totals)
jayhopson_totals = DataFrame(jayhopson_totals)
davidshaw_totals = DataFrame(davidshaw_totals)
dinobabers_totals = DataFrame(dinobabers_totals)
jeremypruitt_totals = DataFrame(jeremypruitt_totals)
tomherman_totals = DataFrame(tomherman_totals)
jimbofisher_totals = DataFrame(jimbofisher_totals)
garypatterson_totals = DataFrame(garypatterson_totals)
everettwithers_totals = DataFrame(everettwithers_totals)
kliffkingsbury_totals = DataFrame(kliffkingsbury_totals)
danadimel_totals = DataFrame(danadimel_totals)
frankwilson_totals = DataFrame(frankwilson_totals)
jasoncandle_totals = DataFrame(jasoncandle_totals)
nealbrown_totals = DataFrame(nealbrown_totals)
williefritz_totals = DataFrame(williefritz_totals)
philipmontgomery_totals = DataFrame(philipmontgomery_totals)
chipkelly_totals = DataFrame(chipkelly_totals)
kylewhittingham_totals = DataFrame(kylewhittingham_totals)
mattwells_totals = DataFrame(mattwells_totals)
derekmason_totals = DataFrame(derekmason_totals)
broncomendenhall_totals = DataFrame(broncomendenhall_totals)
justinfuente_totals = DataFrame(justinfuente_totals)
daveclawson_totals = DataFrame(daveclawson_totals)
chrispetersen_totals = DataFrame(chrispetersen_totals)
mikeleach_totals = DataFrame(mikeleach_totals)
danaholgorsen_totals = DataFrame(danaholgorsen_totals)
mikesanford_totals = DataFrame(mikesanford_totals)
timlester_totals = DataFrame(timlester_totals)
paulchryst_totals = DataFrame(paulchryst_totals)
craigbohl_totals = DataFrame(craigbohl_totals)

#Transpose to make long format
troycalhoun_totals = troycalhoun_totals.transpose()
terrybowden_totals = terrybowden_totals.transpose()
nicksaban_totals = nicksaban_totals.transpose()
billclark_totals = billclark_totals.transpose()
scottsatterfield_totals = scottsatterfield_totals.transpose()
kevinsumlin_totals = kevinsumlin_totals.transpose()
hermedwards_totals = hermedwards_totals.transpose()
chadmorris_totals = chadmorris_totals.transpose()
blakeanderson_totals = blakeanderson_totals.transpose()
jeffmonken_totals = jeffmonken_totals.transpose()
gusmalzahn_totals = gusmalzahn_totals.transpose()
mikeneu_totals = mikeneu_totals.transpose()
mattrhule_totals = mattrhule_totals.transpose()
bryanharsin_totals = bryanharsin_totals.transpose()
steveaddazio_totals = steveaddazio_totals.transpose()
mikejinks_totals = mikejinks_totals.transpose()
kalanisitake_totals = kalanisitake_totals.transpose()
lanceleipold_totals = lanceleipold_totals.transpose()
justinwilcox_totals = justinwilcox_totals.transpose()
joshheupel_totals = joshheupel_totals.transpose()
johnbonamego_totals = johnbonamego_totals.transpose()
bradlambert_totals = bradlambert_totals.transpose()
lukefickell_totals = lukefickell_totals.transpose()
daboswinney_totals = daboswinney_totals.transpose()
joemoglia_totals = joemoglia_totals.transpose()
mikemacintyre_totals = mikemacintyre_totals.transpose()
mikebobo_totals = mikebobo_totals.transpose()
randyedsall_totals = randyedsall_totals.transpose()
davidcutcliffe_totals = davidcutcliffe_totals.transpose()
scottiemontgomery_totals = scottiemontgomery_totals.transpose()
chriscreighton_totals = chriscreighton_totals.transpose()
danmullen_totals = danmullen_totals.transpose()
lanekiffin_totals = lanekiffin_totals.transpose()
butchdavis_totals = butchdavis_totals.transpose()
willietaggart_totals = willietaggart_totals.transpose()
jefftedford_totals = jefftedford_totals.transpose()
kirbysmart_totals = kirbysmart_totals.transpose()
chadlunsford_totals = chadlunsford_totals.transpose()
shawnelliott_totals = shawnelliott_totals.transpose()
pauljohnson_totals = pauljohnson_totals.transpose()
nickrolovich_totals = nickrolovich_totals.transpose()
majorapplewhite_totals = majorapplewhite_totals.transpose()
loviesmith_totals = loviesmith_totals.transpose()
tomallen_totals = tomallen_totals.transpose()
kirkferentz_totals = kirkferentz_totals.transpose()
mattcampbell_totals = mattcampbell_totals.transpose()
davidbeaty_totals = davidbeaty_totals.transpose()
billsnyder_totals = billsnyder_totals.transpose()
seanlewis_totals = seanlewis_totals.transpose()
markstoops_totals = markstoops_totals.transpose()
turnergill_totals = turnergill_totals.transpose()
skipholtz_totals = skipholtz_totals.transpose()
billynapier_totals = billynapier_totals.transpose()
mattviator_totals = mattviator_totals.transpose()
bobbypetrino_totals = bobbypetrino_totals.transpose()
edorgeron_totals = edorgeron_totals.transpose()
docholliday_totals = docholliday_totals.transpose()
djdurkin_totals = djdurkin_totals.transpose()
markwhipple_totals = markwhipple_totals.transpose()
mikenorvell_totals = mikenorvell_totals.transpose()
markricht_totals = markricht_totals.transpose()
chuckmartin_totals = chuckmartin_totals.transpose()
jimharbaugh_totals = jimharbaugh_totals.transpose()
markdantonio_totals = markdantonio_totals.transpose()
rickstockstill_totals = rickstockstill_totals.transpose()
pjfleck_totals = pjfleck_totals.transpose()
mattluke_totals = mattluke_totals.transpose()
joemoorhead_totals = joemoorhead_totals.transpose()
barryodom_totals = barryodom_totals.transpose()
kenniumatalolo_totals = kenniumatalolo_totals.transpose()
scottfrost_totals = scottfrost_totals.transpose()
jaynorvell_totals = jaynorvell_totals.transpose()
tonysanchez_totals = tonysanchez_totals.transpose()
bobdavie_totals = bobdavie_totals.transpose()
dougmartin_totals = dougmartin_totals.transpose()
larryfedora_totals = larryfedora_totals.transpose()
davedoeren_totals = davedoeren_totals.transpose()
sethlittrell_totals = sethlittrell_totals.transpose()
rodcarey_totals = rodcarey_totals.transpose()
patfitzgerald_totals = patfitzgerald_totals.transpose()
briankelly_totals = briankelly_totals.transpose()
franksolich_totals = franksolich_totals.transpose()
urbanmeyer_totals = urbanmeyer_totals.transpose()
lincolnriley_totals = lincolnriley_totals.transpose()
mikegundy_totals = mikegundy_totals.transpose()
bobbywilder_totals = bobbywilder_totals.transpose()
mariocristobal_totals = mariocristobal_totals.transpose()
jonathansmith_totals = jonathansmith_totals.transpose()
jamesfranklin_totals = jamesfranklin_totals.transpose()
patnarduzzi_totals = patnarduzzi_totals.transpose()
jeffbrohm_totals = jeffbrohm_totals.transpose()
mikebloomgren_totals = mikebloomgren_totals.transpose()
chrisash_totals = chrisash_totals.transpose()
rockylong_totals = rockylong_totals.transpose()
brentbrennan_totals = brentbrennan_totals.transpose()
stevecampbell_totals = stevecampbell_totals.transpose()
willmuschamp_totals = willmuschamp_totals.transpose()
charliestrong_totals = charliestrong_totals.transpose()
clayhelton_totals = clayhelton_totals.transpose()
sonnydykes_totals = sonnydykes_totals.transpose()
jayhopson_totals = jayhopson_totals.transpose()
davidshaw_totals = davidshaw_totals.transpose()
dinobabers_totals = dinobabers_totals.transpose()
jeremypruitt_totals = jeremypruitt_totals.transpose()
tomherman_totals = tomherman_totals.transpose()
jimbofisher_totals = jimbofisher_totals.transpose()
garypatterson_totals = garypatterson_totals.transpose()
everettwithers_totals = everettwithers_totals.transpose()
kliffkingsbury_totals = kliffkingsbury_totals.transpose()
danadimel_totals = danadimel_totals.transpose()
frankwilson_totals = frankwilson_totals.transpose()
jasoncandle_totals = jasoncandle_totals.transpose()
nealbrown_totals = nealbrown_totals.transpose()
williefritz_totals = williefritz_totals.transpose()
philipmontgomery_totals = philipmontgomery_totals.transpose()
chipkelly_totals = chipkelly_totals.transpose()
kylewhittingham_totals = kylewhittingham_totals.transpose()
mattwells_totals = mattwells_totals.transpose()
derekmason_totals = derekmason_totals.transpose()
broncomendenhall_totals = broncomendenhall_totals.transpose()
justinfuente_totals = justinfuente_totals.transpose()
daveclawson_totals = daveclawson_totals.transpose()
chrispetersen_totals = chrispetersen_totals.transpose()
mikeleach_totals = mikeleach_totals.transpose()
danaholgorsen_totals = danaholgorsen_totals.transpose()
mikesanford_totals = mikesanford_totals.transpose()
timlester_totals = timlester_totals.transpose()
paulchryst_totals = paulchryst_totals.transpose()
craigbohl_totals = craigbohl_totals.transpose()

#Rename columns
troycalhoun_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
terrybowden_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
nicksaban_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
billclark_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
scottsatterfield_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
kevinsumlin_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
hermedwards_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
chadmorris_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
blakeanderson_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
jeffmonken_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
gusmalzahn_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
mikeneu_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
mattrhule_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
bryanharsin_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
steveaddazio_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
mikejinks_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
kalanisitake_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
lanceleipold_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
justinwilcox_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
joshheupel_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
johnbonamego_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
bradlambert_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
lukefickell_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
daboswinney_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
joemoglia_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
mikemacintyre_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
mikebobo_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
randyedsall_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
davidcutcliffe_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
scottiemontgomery_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
chriscreighton_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
danmullen_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
lanekiffin_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
butchdavis_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
willietaggart_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
jefftedford_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
kirbysmart_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
chadlunsford_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
shawnelliott_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
pauljohnson_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
nickrolovich_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
majorapplewhite_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
loviesmith_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
tomallen_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
kirkferentz_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
mattcampbell_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
davidbeaty_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
billsnyder_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
seanlewis_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
markstoops_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
turnergill_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
skipholtz_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
billynapier_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
mattviator_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
bobbypetrino_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
edorgeron_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
docholliday_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
djdurkin_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
markwhipple_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
mikenorvell_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
markricht_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
chuckmartin_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
jimharbaugh_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
markdantonio_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
rickstockstill_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
pjfleck_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
mattluke_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
joemoorhead_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
barryodom_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
kenniumatalolo_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
scottfrost_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
jaynorvell_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
tonysanchez_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
bobdavie_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
dougmartin_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
larryfedora_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
davedoeren_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
sethlittrell_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
rodcarey_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
patfitzgerald_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
briankelly_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
franksolich_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
urbanmeyer_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
lincolnriley_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
mikegundy_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
bobbywilder_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
mariocristobal_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
jonathansmith_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
jamesfranklin_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
patnarduzzi_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
jeffbrohm_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
mikebloomgren_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
chrisash_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
rockylong_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
brentbrennan_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
stevecampbell_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
willmuschamp_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
charliestrong_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
clayhelton_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
sonnydykes_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
jayhopson_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
davidshaw_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
dinobabers_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
jeremypruitt_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
tomherman_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
jimbofisher_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
garypatterson_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
everettwithers_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
kliffkingsbury_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
danadimel_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
frankwilson_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
jasoncandle_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
nealbrown_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
williefritz_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
philipmontgomery_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
chipkelly_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
kylewhittingham_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
mattwells_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
derekmason_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
broncomendenhall_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
justinfuente_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
daveclawson_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
chrispetersen_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
mikeleach_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
danaholgorsen_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
mikesanford_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
timlester_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
paulchryst_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']
craigbohl_totals.columns = ['CareerW', 'CareerL', 'CareerT','AvgSRS','AvgSOS','SeasonsRanked']

#Insert column for career win percentage
troycalhoun_totals.insert(loc=3, column='CareerWinPct', value = (troycalhoun_totals.CareerW + (troycalhoun_totals.CareerT * 0.5)) / (troycalhoun_totals.CareerW + troycalhoun_totals.CareerL))
terrybowden_totals.insert(loc=3, column='CareerWinPct', value = (terrybowden_totals.CareerW + (terrybowden_totals.CareerT * 0.5)) / (terrybowden_totals.CareerW + terrybowden_totals.CareerL))
nicksaban_totals.insert(loc=3, column='CareerWinPct', value = (nicksaban_totals.CareerW + (nicksaban_totals.CareerT * 0.5)) / (nicksaban_totals.CareerW + nicksaban_totals.CareerL))
billclark_totals.insert(loc=3, column='CareerWinPct', value = (billclark_totals.CareerW + (billclark_totals.CareerT * 0.5)) / (billclark_totals.CareerW + billclark_totals.CareerL))
scottsatterfield_totals.insert(loc=3, column='CareerWinPct', value = (scottsatterfield_totals.CareerW + (scottsatterfield_totals.CareerT * 0.5)) / (scottsatterfield_totals.CareerW + scottsatterfield_totals.CareerL))
kevinsumlin_totals.insert(loc=3, column='CareerWinPct', value = (kevinsumlin_totals.CareerW + (kevinsumlin_totals.CareerT * 0.5)) / (kevinsumlin_totals.CareerW + kevinsumlin_totals.CareerL))
hermedwards_totals.insert(loc=3, column='CareerWinPct', value = (hermedwards_totals.CareerW + (hermedwards_totals.CareerT * 0.5)) / (hermedwards_totals.CareerW + hermedwards_totals.CareerL))
chadmorris_totals.insert(loc=3, column='CareerWinPct', value = (chadmorris_totals.CareerW + (chadmorris_totals.CareerT * 0.5)) / (chadmorris_totals.CareerW + chadmorris_totals.CareerL))
blakeanderson_totals.insert(loc=3, column='CareerWinPct', value = (blakeanderson_totals.CareerW + (blakeanderson_totals.CareerT * 0.5)) / (blakeanderson_totals.CareerW + blakeanderson_totals.CareerL))
jeffmonken_totals.insert(loc=3, column='CareerWinPct', value = (jeffmonken_totals.CareerW + (jeffmonken_totals.CareerT * 0.5)) / (jeffmonken_totals.CareerW + jeffmonken_totals.CareerL))
gusmalzahn_totals.insert(loc=3, column='CareerWinPct', value = (gusmalzahn_totals.CareerW + (gusmalzahn_totals.CareerT * 0.5)) / (gusmalzahn_totals.CareerW + gusmalzahn_totals.CareerL))
mikeneu_totals.insert(loc=3, column='CareerWinPct', value = (mikeneu_totals.CareerW + (mikeneu_totals.CareerT * 0.5)) / (mikeneu_totals.CareerW + mikeneu_totals.CareerL))
mattrhule_totals.insert(loc=3, column='CareerWinPct', value = (mattrhule_totals.CareerW + (mattrhule_totals.CareerT * 0.5)) / (mattrhule_totals.CareerW + mattrhule_totals.CareerL))
bryanharsin_totals.insert(loc=3, column='CareerWinPct', value = (bryanharsin_totals.CareerW + (bryanharsin_totals.CareerT * 0.5)) / (bryanharsin_totals.CareerW + bryanharsin_totals.CareerL))
steveaddazio_totals.insert(loc=3, column='CareerWinPct', value = (steveaddazio_totals.CareerW + (steveaddazio_totals.CareerT * 0.5)) / (steveaddazio_totals.CareerW + steveaddazio_totals.CareerL))
mikejinks_totals.insert(loc=3, column='CareerWinPct', value = (mikejinks_totals.CareerW + (mikejinks_totals.CareerT * 0.5)) / (mikejinks_totals.CareerW + mikejinks_totals.CareerL))
kalanisitake_totals.insert(loc=3, column='CareerWinPct', value = (kalanisitake_totals.CareerW + (kalanisitake_totals.CareerT * 0.5)) / (kalanisitake_totals.CareerW + kalanisitake_totals.CareerL))
lanceleipold_totals.insert(loc=3, column='CareerWinPct', value = (lanceleipold_totals.CareerW + (lanceleipold_totals.CareerT * 0.5)) / (lanceleipold_totals.CareerW + lanceleipold_totals.CareerL))
justinwilcox_totals.insert(loc=3, column='CareerWinPct', value = (justinwilcox_totals.CareerW + (justinwilcox_totals.CareerT * 0.5)) / (justinwilcox_totals.CareerW + justinwilcox_totals.CareerL))
joshheupel_totals.insert(loc=3, column='CareerWinPct', value = (joshheupel_totals.CareerW + (joshheupel_totals.CareerT * 0.5)) / (joshheupel_totals.CareerW + joshheupel_totals.CareerL))
johnbonamego_totals.insert(loc=3, column='CareerWinPct', value = (johnbonamego_totals.CareerW + (johnbonamego_totals.CareerT * 0.5)) / (johnbonamego_totals.CareerW + johnbonamego_totals.CareerL))
bradlambert_totals.insert(loc=3, column='CareerWinPct', value = (bradlambert_totals.CareerW + (bradlambert_totals.CareerT * 0.5)) / (bradlambert_totals.CareerW + bradlambert_totals.CareerL))
lukefickell_totals.insert(loc=3, column='CareerWinPct', value = (lukefickell_totals.CareerW + (lukefickell_totals.CareerT * 0.5)) / (lukefickell_totals.CareerW + lukefickell_totals.CareerL))
daboswinney_totals.insert(loc=3, column='CareerWinPct', value = (daboswinney_totals.CareerW + (daboswinney_totals.CareerT * 0.5)) / (daboswinney_totals.CareerW + daboswinney_totals.CareerL))
joemoglia_totals.insert(loc=3, column='CareerWinPct', value = (joemoglia_totals.CareerW + (joemoglia_totals.CareerT * 0.5)) / (joemoglia_totals.CareerW + joemoglia_totals.CareerL))
mikemacintyre_totals.insert(loc=3, column='CareerWinPct', value = (mikemacintyre_totals.CareerW + (mikemacintyre_totals.CareerT * 0.5)) / (mikemacintyre_totals.CareerW + mikemacintyre_totals.CareerL))
mikebobo_totals.insert(loc=3, column='CareerWinPct', value = (mikebobo_totals.CareerW + (mikebobo_totals.CareerT * 0.5)) / (mikebobo_totals.CareerW + mikebobo_totals.CareerL))
randyedsall_totals.insert(loc=3, column='CareerWinPct', value = (randyedsall_totals.CareerW + (randyedsall_totals.CareerT * 0.5)) / (randyedsall_totals.CareerW + randyedsall_totals.CareerL))
davidcutcliffe_totals.insert(loc=3, column='CareerWinPct', value = (davidcutcliffe_totals.CareerW + (davidcutcliffe_totals.CareerT * 0.5)) / (davidcutcliffe_totals.CareerW + davidcutcliffe_totals.CareerL))
scottiemontgomery_totals.insert(loc=3, column='CareerWinPct', value = (scottiemontgomery_totals.CareerW + (scottiemontgomery_totals.CareerT * 0.5)) / (scottiemontgomery_totals.CareerW + scottiemontgomery_totals.CareerL))
chriscreighton_totals.insert(loc=3, column='CareerWinPct', value = (chriscreighton_totals.CareerW + (chriscreighton_totals.CareerT * 0.5)) / (chriscreighton_totals.CareerW + chriscreighton_totals.CareerL))
danmullen_totals.insert(loc=3, column='CareerWinPct', value = (danmullen_totals.CareerW + (danmullen_totals.CareerT * 0.5)) / (danmullen_totals.CareerW + danmullen_totals.CareerL))
lanekiffin_totals.insert(loc=3, column='CareerWinPct', value = (lanekiffin_totals.CareerW + (lanekiffin_totals.CareerT * 0.5)) / (lanekiffin_totals.CareerW + lanekiffin_totals.CareerL))
butchdavis_totals.insert(loc=3, column='CareerWinPct', value = (butchdavis_totals.CareerW + (butchdavis_totals.CareerT * 0.5)) / (butchdavis_totals.CareerW + butchdavis_totals.CareerL))
willietaggart_totals.insert(loc=3, column='CareerWinPct', value = (willietaggart_totals.CareerW + (willietaggart_totals.CareerT * 0.5)) / (willietaggart_totals.CareerW + willietaggart_totals.CareerL))
jefftedford_totals.insert(loc=3, column='CareerWinPct', value = (jefftedford_totals.CareerW + (jefftedford_totals.CareerT * 0.5)) / (jefftedford_totals.CareerW + jefftedford_totals.CareerL))
kirbysmart_totals.insert(loc=3, column='CareerWinPct', value = (kirbysmart_totals.CareerW + (kirbysmart_totals.CareerT * 0.5)) / (kirbysmart_totals.CareerW + kirbysmart_totals.CareerL))
chadlunsford_totals.insert(loc=3, column='CareerWinPct', value = (chadlunsford_totals.CareerW + (chadlunsford_totals.CareerT * 0.5)) / (chadlunsford_totals.CareerW + chadlunsford_totals.CareerL))
shawnelliott_totals.insert(loc=3, column='CareerWinPct', value = (shawnelliott_totals.CareerW + (shawnelliott_totals.CareerT * 0.5)) / (shawnelliott_totals.CareerW + shawnelliott_totals.CareerL))
pauljohnson_totals.insert(loc=3, column='CareerWinPct', value = (pauljohnson_totals.CareerW + (pauljohnson_totals.CareerT * 0.5)) / (pauljohnson_totals.CareerW + pauljohnson_totals.CareerL))
nickrolovich_totals.insert(loc=3, column='CareerWinPct', value = (nickrolovich_totals.CareerW + (nickrolovich_totals.CareerT * 0.5)) / (nickrolovich_totals.CareerW + nickrolovich_totals.CareerL))
majorapplewhite_totals.insert(loc=3, column='CareerWinPct', value = (majorapplewhite_totals.CareerW + (majorapplewhite_totals.CareerT * 0.5)) / (majorapplewhite_totals.CareerW + majorapplewhite_totals.CareerL))
loviesmith_totals.insert(loc=3, column='CareerWinPct', value = (loviesmith_totals.CareerW + (loviesmith_totals.CareerT * 0.5)) / (loviesmith_totals.CareerW + loviesmith_totals.CareerL))
tomallen_totals.insert(loc=3, column='CareerWinPct', value = (tomallen_totals.CareerW + (tomallen_totals.CareerT * 0.5)) / (tomallen_totals.CareerW + tomallen_totals.CareerL))
kirkferentz_totals.insert(loc=3, column='CareerWinPct', value = (kirkferentz_totals.CareerW + (kirkferentz_totals.CareerT * 0.5)) / (kirkferentz_totals.CareerW + kirkferentz_totals.CareerL))
mattcampbell_totals.insert(loc=3, column='CareerWinPct', value = (mattcampbell_totals.CareerW + (mattcampbell_totals.CareerT * 0.5)) / (mattcampbell_totals.CareerW + mattcampbell_totals.CareerL))
davidbeaty_totals.insert(loc=3, column='CareerWinPct', value = (davidbeaty_totals.CareerW + (davidbeaty_totals.CareerT * 0.5)) / (davidbeaty_totals.CareerW + davidbeaty_totals.CareerL))
billsnyder_totals.insert(loc=3, column='CareerWinPct', value = (billsnyder_totals.CareerW + (billsnyder_totals.CareerT * 0.5)) / (billsnyder_totals.CareerW + billsnyder_totals.CareerL))
seanlewis_totals.insert(loc=3, column='CareerWinPct', value = (seanlewis_totals.CareerW + (seanlewis_totals.CareerT * 0.5)) / (seanlewis_totals.CareerW + seanlewis_totals.CareerL))
markstoops_totals.insert(loc=3, column='CareerWinPct', value = (markstoops_totals.CareerW + (markstoops_totals.CareerT * 0.5)) / (markstoops_totals.CareerW + markstoops_totals.CareerL))
turnergill_totals.insert(loc=3, column='CareerWinPct', value = (turnergill_totals.CareerW + (turnergill_totals.CareerT * 0.5)) / (turnergill_totals.CareerW + turnergill_totals.CareerL))
skipholtz_totals.insert(loc=3, column='CareerWinPct', value = (skipholtz_totals.CareerW + (skipholtz_totals.CareerT * 0.5)) / (skipholtz_totals.CareerW + skipholtz_totals.CareerL))
billynapier_totals.insert(loc=3, column='CareerWinPct', value = (billynapier_totals.CareerW + (billynapier_totals.CareerT * 0.5)) / (billynapier_totals.CareerW + billynapier_totals.CareerL))
mattviator_totals.insert(loc=3, column='CareerWinPct', value = (mattviator_totals.CareerW + (mattviator_totals.CareerT * 0.5)) / (mattviator_totals.CareerW + mattviator_totals.CareerL))
bobbypetrino_totals.insert(loc=3, column='CareerWinPct', value = (bobbypetrino_totals.CareerW + (bobbypetrino_totals.CareerT * 0.5)) / (bobbypetrino_totals.CareerW + bobbypetrino_totals.CareerL))
edorgeron_totals.insert(loc=3, column='CareerWinPct', value = (edorgeron_totals.CareerW + (edorgeron_totals.CareerT * 0.5)) / (edorgeron_totals.CareerW + edorgeron_totals.CareerL))
docholliday_totals.insert(loc=3, column='CareerWinPct', value = (docholliday_totals.CareerW + (docholliday_totals.CareerT * 0.5)) / (docholliday_totals.CareerW + docholliday_totals.CareerL))
djdurkin_totals.insert(loc=3, column='CareerWinPct', value = (djdurkin_totals.CareerW + (djdurkin_totals.CareerT * 0.5)) / (djdurkin_totals.CareerW + djdurkin_totals.CareerL))
markwhipple_totals.insert(loc=3, column='CareerWinPct', value = (markwhipple_totals.CareerW + (markwhipple_totals.CareerT * 0.5)) / (markwhipple_totals.CareerW + markwhipple_totals.CareerL))
mikenorvell_totals.insert(loc=3, column='CareerWinPct', value = (mikenorvell_totals.CareerW + (mikenorvell_totals.CareerT * 0.5)) / (mikenorvell_totals.CareerW + mikenorvell_totals.CareerL))
markricht_totals.insert(loc=3, column='CareerWinPct', value = (markricht_totals.CareerW + (markricht_totals.CareerT * 0.5)) / (markricht_totals.CareerW + markricht_totals.CareerL))
chuckmartin_totals.insert(loc=3, column='CareerWinPct', value = (chuckmartin_totals.CareerW + (chuckmartin_totals.CareerT * 0.5)) / (chuckmartin_totals.CareerW + chuckmartin_totals.CareerL))
jimharbaugh_totals.insert(loc=3, column='CareerWinPct', value = (jimharbaugh_totals.CareerW + (jimharbaugh_totals.CareerT * 0.5)) / (jimharbaugh_totals.CareerW + jimharbaugh_totals.CareerL))
markdantonio_totals.insert(loc=3, column='CareerWinPct', value = (markdantonio_totals.CareerW + (markdantonio_totals.CareerT * 0.5)) / (markdantonio_totals.CareerW + markdantonio_totals.CareerL))
rickstockstill_totals.insert(loc=3, column='CareerWinPct', value = (rickstockstill_totals.CareerW + (rickstockstill_totals.CareerT * 0.5)) / (rickstockstill_totals.CareerW + rickstockstill_totals.CareerL))
pjfleck_totals.insert(loc=3, column='CareerWinPct', value = (pjfleck_totals.CareerW + (pjfleck_totals.CareerT * 0.5)) / (pjfleck_totals.CareerW + pjfleck_totals.CareerL))
mattluke_totals.insert(loc=3, column='CareerWinPct', value = (mattluke_totals.CareerW + (mattluke_totals.CareerT * 0.5)) / (mattluke_totals.CareerW + mattluke_totals.CareerL))
joemoorhead_totals.insert(loc=3, column='CareerWinPct', value = (joemoorhead_totals.CareerW + (joemoorhead_totals.CareerT * 0.5)) / (joemoorhead_totals.CareerW + joemoorhead_totals.CareerL))
barryodom_totals.insert(loc=3, column='CareerWinPct', value = (barryodom_totals.CareerW + (barryodom_totals.CareerT * 0.5)) / (barryodom_totals.CareerW + barryodom_totals.CareerL))
kenniumatalolo_totals.insert(loc=3, column='CareerWinPct', value = (kenniumatalolo_totals.CareerW + (kenniumatalolo_totals.CareerT * 0.5)) / (kenniumatalolo_totals.CareerW + kenniumatalolo_totals.CareerL))
scottfrost_totals.insert(loc=3, column='CareerWinPct', value = (scottfrost_totals.CareerW + (scottfrost_totals.CareerT * 0.5)) / (scottfrost_totals.CareerW + scottfrost_totals.CareerL))
jaynorvell_totals.insert(loc=3, column='CareerWinPct', value = (jaynorvell_totals.CareerW + (jaynorvell_totals.CareerT * 0.5)) / (jaynorvell_totals.CareerW + jaynorvell_totals.CareerL))
tonysanchez_totals.insert(loc=3, column='CareerWinPct', value = (tonysanchez_totals.CareerW + (tonysanchez_totals.CareerT * 0.5)) / (tonysanchez_totals.CareerW + tonysanchez_totals.CareerL))
bobdavie_totals.insert(loc=3, column='CareerWinPct', value = (bobdavie_totals.CareerW + (bobdavie_totals.CareerT * 0.5)) / (bobdavie_totals.CareerW + bobdavie_totals.CareerL))
dougmartin_totals.insert(loc=3, column='CareerWinPct', value = (dougmartin_totals.CareerW + (dougmartin_totals.CareerT * 0.5)) / (dougmartin_totals.CareerW + dougmartin_totals.CareerL))
larryfedora_totals.insert(loc=3, column='CareerWinPct', value = (larryfedora_totals.CareerW + (larryfedora_totals.CareerT * 0.5)) / (larryfedora_totals.CareerW + larryfedora_totals.CareerL))
davedoeren_totals.insert(loc=3, column='CareerWinPct', value = (davedoeren_totals.CareerW + (davedoeren_totals.CareerT * 0.5)) / (davedoeren_totals.CareerW + davedoeren_totals.CareerL))
sethlittrell_totals.insert(loc=3, column='CareerWinPct', value = (sethlittrell_totals.CareerW + (sethlittrell_totals.CareerT * 0.5)) / (sethlittrell_totals.CareerW + sethlittrell_totals.CareerL))
rodcarey_totals.insert(loc=3, column='CareerWinPct', value = (rodcarey_totals.CareerW + (rodcarey_totals.CareerT * 0.5)) / (rodcarey_totals.CareerW + rodcarey_totals.CareerL))
patfitzgerald_totals.insert(loc=3, column='CareerWinPct', value = (patfitzgerald_totals.CareerW + (patfitzgerald_totals.CareerT * 0.5)) / (patfitzgerald_totals.CareerW + patfitzgerald_totals.CareerL))
briankelly_totals.insert(loc=3, column='CareerWinPct', value = (briankelly_totals.CareerW + (briankelly_totals.CareerT * 0.5)) / (briankelly_totals.CareerW + briankelly_totals.CareerL))
franksolich_totals.insert(loc=3, column='CareerWinPct', value = (franksolich_totals.CareerW + (franksolich_totals.CareerT * 0.5)) / (franksolich_totals.CareerW + franksolich_totals.CareerL))
urbanmeyer_totals.insert(loc=3, column='CareerWinPct', value = (urbanmeyer_totals.CareerW + (urbanmeyer_totals.CareerT * 0.5)) / (urbanmeyer_totals.CareerW + urbanmeyer_totals.CareerL))
lincolnriley_totals.insert(loc=3, column='CareerWinPct', value = (lincolnriley_totals.CareerW + (lincolnriley_totals.CareerT * 0.5)) / (lincolnriley_totals.CareerW + lincolnriley_totals.CareerL))
mikegundy_totals.insert(loc=3, column='CareerWinPct', value = (mikegundy_totals.CareerW + (mikegundy_totals.CareerT * 0.5)) / (mikegundy_totals.CareerW + mikegundy_totals.CareerL))
bobbywilder_totals.insert(loc=3, column='CareerWinPct', value = (bobbywilder_totals.CareerW + (bobbywilder_totals.CareerT * 0.5)) / (bobbywilder_totals.CareerW + bobbywilder_totals.CareerL))
mariocristobal_totals.insert(loc=3, column='CareerWinPct', value = (mariocristobal_totals.CareerW + (mariocristobal_totals.CareerT * 0.5)) / (mariocristobal_totals.CareerW + mariocristobal_totals.CareerL))
jonathansmith_totals.insert(loc=3, column='CareerWinPct', value = (jonathansmith_totals.CareerW + (jonathansmith_totals.CareerT * 0.5)) / (jonathansmith_totals.CareerW + jonathansmith_totals.CareerL))
jamesfranklin_totals.insert(loc=3, column='CareerWinPct', value = (jamesfranklin_totals.CareerW + (jamesfranklin_totals.CareerT * 0.5)) / (jamesfranklin_totals.CareerW + jamesfranklin_totals.CareerL))
patnarduzzi_totals.insert(loc=3, column='CareerWinPct', value = (patnarduzzi_totals.CareerW + (patnarduzzi_totals.CareerT * 0.5)) / (patnarduzzi_totals.CareerW + patnarduzzi_totals.CareerL))
jeffbrohm_totals.insert(loc=3, column='CareerWinPct', value = (jeffbrohm_totals.CareerW + (jeffbrohm_totals.CareerT * 0.5)) / (jeffbrohm_totals.CareerW + jeffbrohm_totals.CareerL))
mikebloomgren_totals.insert(loc=3, column='CareerWinPct', value = (mikebloomgren_totals.CareerW + (mikebloomgren_totals.CareerT * 0.5)) / (mikebloomgren_totals.CareerW + mikebloomgren_totals.CareerL))
chrisash_totals.insert(loc=3, column='CareerWinPct', value = (chrisash_totals.CareerW + (chrisash_totals.CareerT * 0.5)) / (chrisash_totals.CareerW + chrisash_totals.CareerL))
rockylong_totals.insert(loc=3, column='CareerWinPct', value = (rockylong_totals.CareerW + (rockylong_totals.CareerT * 0.5)) / (rockylong_totals.CareerW + rockylong_totals.CareerL))
brentbrennan_totals.insert(loc=3, column='CareerWinPct', value = (brentbrennan_totals.CareerW + (brentbrennan_totals.CareerT * 0.5)) / (brentbrennan_totals.CareerW + brentbrennan_totals.CareerL))
stevecampbell_totals.insert(loc=3, column='CareerWinPct', value = (stevecampbell_totals.CareerW + (stevecampbell_totals.CareerT * 0.5)) / (stevecampbell_totals.CareerW + stevecampbell_totals.CareerL))
willmuschamp_totals.insert(loc=3, column='CareerWinPct', value = (willmuschamp_totals.CareerW + (willmuschamp_totals.CareerT * 0.5)) / (willmuschamp_totals.CareerW + willmuschamp_totals.CareerL))
charliestrong_totals.insert(loc=3, column='CareerWinPct', value = (charliestrong_totals.CareerW + (charliestrong_totals.CareerT * 0.5)) / (charliestrong_totals.CareerW + charliestrong_totals.CareerL))
clayhelton_totals.insert(loc=3, column='CareerWinPct', value = (clayhelton_totals.CareerW + (clayhelton_totals.CareerT * 0.5)) / (clayhelton_totals.CareerW + clayhelton_totals.CareerL))
sonnydykes_totals.insert(loc=3, column='CareerWinPct', value = (sonnydykes_totals.CareerW + (sonnydykes_totals.CareerT * 0.5)) / (sonnydykes_totals.CareerW + sonnydykes_totals.CareerL))
jayhopson_totals.insert(loc=3, column='CareerWinPct', value = (jayhopson_totals.CareerW + (jayhopson_totals.CareerT * 0.5)) / (jayhopson_totals.CareerW + jayhopson_totals.CareerL))
davidshaw_totals.insert(loc=3, column='CareerWinPct', value = (davidshaw_totals.CareerW + (davidshaw_totals.CareerT * 0.5)) / (davidshaw_totals.CareerW + davidshaw_totals.CareerL))
dinobabers_totals.insert(loc=3, column='CareerWinPct', value = (dinobabers_totals.CareerW + (dinobabers_totals.CareerT * 0.5)) / (dinobabers_totals.CareerW + dinobabers_totals.CareerL))
jeremypruitt_totals.insert(loc=3, column='CareerWinPct', value = (jeremypruitt_totals.CareerW + (jeremypruitt_totals.CareerT * 0.5)) / (jeremypruitt_totals.CareerW + jeremypruitt_totals.CareerL))
tomherman_totals.insert(loc=3, column='CareerWinPct', value = (tomherman_totals.CareerW + (tomherman_totals.CareerT * 0.5)) / (tomherman_totals.CareerW + tomherman_totals.CareerL))
jimbofisher_totals.insert(loc=3, column='CareerWinPct', value = (jimbofisher_totals.CareerW + (jimbofisher_totals.CareerT * 0.5)) / (jimbofisher_totals.CareerW + jimbofisher_totals.CareerL))
garypatterson_totals.insert(loc=3, column='CareerWinPct', value = (garypatterson_totals.CareerW + (garypatterson_totals.CareerT * 0.5)) / (garypatterson_totals.CareerW + garypatterson_totals.CareerL))
everettwithers_totals.insert(loc=3, column='CareerWinPct', value = (everettwithers_totals.CareerW + (everettwithers_totals.CareerT * 0.5)) / (everettwithers_totals.CareerW + everettwithers_totals.CareerL))
kliffkingsbury_totals.insert(loc=3, column='CareerWinPct', value = (kliffkingsbury_totals.CareerW + (kliffkingsbury_totals.CareerT * 0.5)) / (kliffkingsbury_totals.CareerW + kliffkingsbury_totals.CareerL))
danadimel_totals.insert(loc=3, column='CareerWinPct', value = (danadimel_totals.CareerW + (danadimel_totals.CareerT * 0.5)) / (danadimel_totals.CareerW + danadimel_totals.CareerL))
frankwilson_totals.insert(loc=3, column='CareerWinPct', value = (frankwilson_totals.CareerW + (frankwilson_totals.CareerT * 0.5)) / (frankwilson_totals.CareerW + frankwilson_totals.CareerL))
jasoncandle_totals.insert(loc=3, column='CareerWinPct', value = (jasoncandle_totals.CareerW + (jasoncandle_totals.CareerT * 0.5)) / (jasoncandle_totals.CareerW + jasoncandle_totals.CareerL))
nealbrown_totals.insert(loc=3, column='CareerWinPct', value = (nealbrown_totals.CareerW + (nealbrown_totals.CareerT * 0.5)) / (nealbrown_totals.CareerW + nealbrown_totals.CareerL))
williefritz_totals.insert(loc=3, column='CareerWinPct', value = (williefritz_totals.CareerW + (williefritz_totals.CareerT * 0.5)) / (williefritz_totals.CareerW + williefritz_totals.CareerL))
philipmontgomery_totals.insert(loc=3, column='CareerWinPct', value = (philipmontgomery_totals.CareerW + (philipmontgomery_totals.CareerT * 0.5)) / (philipmontgomery_totals.CareerW + philipmontgomery_totals.CareerL))
chipkelly_totals.insert(loc=3, column='CareerWinPct', value = (chipkelly_totals.CareerW + (chipkelly_totals.CareerT * 0.5)) / (chipkelly_totals.CareerW + chipkelly_totals.CareerL))
kylewhittingham_totals.insert(loc=3, column='CareerWinPct', value = (kylewhittingham_totals.CareerW + (kylewhittingham_totals.CareerT * 0.5)) / (kylewhittingham_totals.CareerW + kylewhittingham_totals.CareerL))
mattwells_totals.insert(loc=3, column='CareerWinPct', value = (mattwells_totals.CareerW + (mattwells_totals.CareerT * 0.5)) / (mattwells_totals.CareerW + mattwells_totals.CareerL))
derekmason_totals.insert(loc=3, column='CareerWinPct', value = (derekmason_totals.CareerW + (derekmason_totals.CareerT * 0.5)) / (derekmason_totals.CareerW + derekmason_totals.CareerL))
broncomendenhall_totals.insert(loc=3, column='CareerWinPct', value = (broncomendenhall_totals.CareerW + (broncomendenhall_totals.CareerT * 0.5)) / (broncomendenhall_totals.CareerW + broncomendenhall_totals.CareerL))
justinfuente_totals.insert(loc=3, column='CareerWinPct', value = (justinfuente_totals.CareerW + (justinfuente_totals.CareerT * 0.5)) / (justinfuente_totals.CareerW + justinfuente_totals.CareerL))
daveclawson_totals.insert(loc=3, column='CareerWinPct', value = (daveclawson_totals.CareerW + (daveclawson_totals.CareerT * 0.5)) / (daveclawson_totals.CareerW + daveclawson_totals.CareerL))
chrispetersen_totals.insert(loc=3, column='CareerWinPct', value = (chrispetersen_totals.CareerW + (chrispetersen_totals.CareerT * 0.5)) / (chrispetersen_totals.CareerW + chrispetersen_totals.CareerL))
mikeleach_totals.insert(loc=3, column='CareerWinPct', value = (mikeleach_totals.CareerW + (mikeleach_totals.CareerT * 0.5)) / (mikeleach_totals.CareerW + mikeleach_totals.CareerL))
danaholgorsen_totals.insert(loc=3, column='CareerWinPct', value = (danaholgorsen_totals.CareerW + (danaholgorsen_totals.CareerT * 0.5)) / (danaholgorsen_totals.CareerW + danaholgorsen_totals.CareerL))
mikesanford_totals.insert(loc=3, column='CareerWinPct', value = (mikesanford_totals.CareerW + (mikesanford_totals.CareerT * 0.5)) / (mikesanford_totals.CareerW + mikesanford_totals.CareerL))
timlester_totals.insert(loc=3, column='CareerWinPct', value = (timlester_totals.CareerW + (timlester_totals.CareerT * 0.5)) / (timlester_totals.CareerW + timlester_totals.CareerL))
paulchryst_totals.insert(loc=3, column='CareerWinPct', value = (paulchryst_totals.CareerW + (paulchryst_totals.CareerT * 0.5)) / (paulchryst_totals.CareerW + paulchryst_totals.CareerL))
craigbohl_totals.insert(loc=3, column='CareerWinPct', value = (craigbohl_totals.CareerW + (craigbohl_totals.CareerT * 0.5)) / (craigbohl_totals.CareerW + craigbohl_totals.CareerL))

#append data frames
coaches2 = troycalhoun_totals.append([
terrybowden_totals,
nicksaban_totals,
billclark_totals,
scottsatterfield_totals,
kevinsumlin_totals,
hermedwards_totals,
chadmorris_totals,
blakeanderson_totals,
jeffmonken_totals,
gusmalzahn_totals,
mikeneu_totals,
mattrhule_totals,
bryanharsin_totals,
steveaddazio_totals,
mikejinks_totals,
kalanisitake_totals,
lanceleipold_totals,
justinwilcox_totals,
joshheupel_totals,
johnbonamego_totals,
bradlambert_totals,
lukefickell_totals,
daboswinney_totals,
joemoglia_totals,
mikemacintyre_totals,
mikebobo_totals,
randyedsall_totals,
davidcutcliffe_totals,
scottiemontgomery_totals,
chriscreighton_totals,
danmullen_totals,
lanekiffin_totals,
butchdavis_totals,
willietaggart_totals,
jefftedford_totals,
kirbysmart_totals,
chadlunsford_totals,
shawnelliott_totals,
pauljohnson_totals,
nickrolovich_totals,
majorapplewhite_totals,
loviesmith_totals,
tomallen_totals,
kirkferentz_totals,
mattcampbell_totals,
davidbeaty_totals,
billsnyder_totals,
seanlewis_totals,
markstoops_totals,
turnergill_totals,
skipholtz_totals,
billynapier_totals,
mattviator_totals,
bobbypetrino_totals,
edorgeron_totals,
docholliday_totals,
djdurkin_totals,
markwhipple_totals,
mikenorvell_totals,
markricht_totals,
chuckmartin_totals,
jimharbaugh_totals,
markdantonio_totals,
rickstockstill_totals,
pjfleck_totals,
mattluke_totals,
joemoorhead_totals,
barryodom_totals,
kenniumatalolo_totals,
scottfrost_totals,
jaynorvell_totals,
tonysanchez_totals,
bobdavie_totals,
dougmartin_totals,
larryfedora_totals,
davedoeren_totals,
sethlittrell_totals,
rodcarey_totals,
patfitzgerald_totals,
briankelly_totals,
franksolich_totals,
urbanmeyer_totals,
lincolnriley_totals,
mikegundy_totals,
bobbywilder_totals,
mariocristobal_totals,
jonathansmith_totals,
jamesfranklin_totals,
patnarduzzi_totals,
jeffbrohm_totals,
mikebloomgren_totals,
chrisash_totals,
rockylong_totals,
brentbrennan_totals,
stevecampbell_totals,
willmuschamp_totals,
charliestrong_totals,
clayhelton_totals,
sonnydykes_totals,
jayhopson_totals,
davidshaw_totals,
dinobabers_totals,
jeremypruitt_totals,
tomherman_totals,
jimbofisher_totals,
garypatterson_totals,
everettwithers_totals,
kliffkingsbury_totals,
danadimel_totals,
frankwilson_totals,
jasoncandle_totals,
nealbrown_totals,
williefritz_totals,
philipmontgomery_totals,
chipkelly_totals,
kylewhittingham_totals,
mattwells_totals,
derekmason_totals,
broncomendenhall_totals,
justinfuente_totals,
daveclawson_totals,
chrispetersen_totals,
mikeleach_totals,
danaholgorsen_totals,
mikesanford_totals,
timlester_totals,
paulchryst_totals,
craigbohl_totals \
])

###############
### Merge 1 ###
###############
#Create index to match coaches
coaches2.insert(0, 'coachID', range(10001, 10001 + len(coaches2)))

#merge coaches and coaches2
coaches_final = coaches.merge(coaches2, how='left', on='coachID')

####################
####  STADIUMS  ####
####################
#Read in Stadium Sizes
stadiums = requests.get("https://www.collegegridirons.com/comparisons-by-capacity/")
stadiums_soup = bs4.BeautifulSoup(stadiums.text, 'lxml')
stadiums_table = stadiums_soup.select('table')
stadiums_df = pd.read_html(str(stadiums_table))[0]

#Change School names in coaches so that they match stadiums
coaches_final = coaches_final.replace({'School': {
'Alabama at Birmingham':'UAB',
'Brigham Young':'BYU',
'Charlotte':'UNC Charlotte',
'Massachusetts':'UMass',
'Miami (Fla.)':'Miami',
'Miami (Ohio)':'Miami-OH',
'Nevada-Las Vegas':'UNLV',
'North Carolina State':'NC State',
'Southern California':'USC',
'Southern Methodist':'SMU',
'Southern Mississippi':'Southern Miss',
'Texas Christian':'TCU',
'Texas-El Paso':'UTEP',
'Texas-San Antonio':'UTSA'}})

#Change college in staidums to school
stadiums_df = stadiums_df.rename({'College':'School'}, axis=1)

###############
### Merge 2 ###
###############
#Merge coaches_final with stadiums
coaches_final = coaches_final.merge(stadiums_df, how='left', on='School')

###########################
### Cleanup after merge ###
###########################
#Drop columns from coaches
coaches_final.drop(['Stadium','Conference_y','Opened'],axis =1, inplace=True)
#Rename Capacity
coaches_final = coaches_final.rename({'Capacity':'StadiumCapacity'}, axis=1)

###########
### GSR ###
###########
#read in GSR data
gsr_df = pd.read_excel('GSR.xlsx')
gsr_df['School'] = gsr_df['School'].str.replace('University of', '')
gsr_df['School'] = gsr_df['School'].str.replace('University', '')
gsr_df['School'] = gsr_df['School'].str.strip()

#Rename schools in gsr df
gsr_df = gsr_df.replace({'School': {
'U.S. Air Force Academy':'Air Force',
'U.S. Military Academy':'Army',
'Alabama at Birmingham':'UAB',
'Arkansas, Fayetteville':'Arkansas',
'Bowling Green State':'Bowling Green',
'Brigham Young':'BYU',
'at Buffalo, the State  New York':'Buffalo',
'California, Berkeley':'California',
'The  North Carolina at Charlotte':'UNC Charlotte',
'Colorado, Boulder':'Colorado',
'California State , Fresno':'Fresno State',
'Georgia Institute of Technology':'Georgia Tech',
'Hawaii, Manoa':'Hawaii',
'Illinois Urbana-Champaign':'Illinois',
'Indiana , Bloomington':'Indiana',
'Louisiana at Lafayette':'Louisiana-Lafayette',
'Louisiana Monroe':'Louisiana-Monroe',
'Louisiana State':'LSU',
'Maryland, College Park':'Maryland',
'Massachusetts, Amherst':'UMass',
'Miami (Florida)':'Miami',
'Miami  (Ohio)':'Miami-OH',
'Middle Tennessee State':'Middle Tennessee',
'Minnesota, Twin Cities':'Minnesota',
'Missouri, Columbia':'Missouri',
'U.S. Naval Academy':'Navy',
'Nebraska, Lincoln':'Nebraska',
'Nevada, Reno':'Nevada',
'Nevada, Las Vegas':'UNLV',
'North Carolina, Chapel Hill':'North Carolina',
'North Carolina State':'NC State',
'The Ohio State':'Ohio State',
'Pennsylvania State':'Penn State',
'Rutgers, The State  New Jersey, New Brunswick':'Rutgers',
'South Carolina, Columbia':'South Carolina',
'Southern California':'USC',
'Southern Methodist':'SMU',
'The  Southern Mississippi':'Southern Miss',
'Tennessee, Knoxville':'Tennessee',
'Texas at Austin':'Texas',
'Texas A&M , College Station':'Texas A&M',
'Texas Christian':'TCU',
'Texas at El Paso':'UTEP',
'Texas at San Antonio':'UTSA',
'The  Tulsa':'Tulsa',
'California, Los Angeles':'UCLA',
'Virginia Polytechnic Institute and State':'Virginia Tech',
'Wisconsin-Madison':'Wisconsin'}})

###############
### Merge 3 ###
###############
#merge coaches with gsr
coaches_final = coaches_final.merge(gsr_df, how='left', on='School')

###########################
### Cleanup after merge ###
###########################
#Drop columns from coaches
coaches_final.drop(['Cohort Year','Conference','Sport','State','GSR Report','FGR Report'],axis =1, inplace=True)

#Rename Conference_x
coaches_final = coaches_final.rename({'Conference_x':'Conference'}, axis=1)

###########################
### 2017 School Records ###
###########################
#Get 2017 records for Big 10
big10_2017 = requests.get("https://www.sports-reference.com/cfb/conferences/big-ten/2017.html")
big10_soup = bs4.BeautifulSoup(big10_2017.text, 'lxml')
big10_table = big10_soup.select('table')
big10_df = pd.read_html(str(big10_table))[0]
big10_df.columns = big10_df.columns.droplevel(0)
big10_df.rename(columns={big10_df.columns[0]: "School" }, inplace = True)

#Get 2017 records for ACC
acc_2017 = requests.get("https://www.sports-reference.com/cfb/conferences/acc/2017.html")
acc_soup = bs4.BeautifulSoup(acc_2017.text, 'lxml')
acc_table = acc_soup.select('table')
acc_df = pd.read_html(str(acc_table))[0]
acc_df.columns = acc_df.columns.droplevel(0)
acc_df.rename(columns={acc_df.columns[0]: "School" }, inplace = True)

#Get 2017 records for SEC
sec_2017 = requests.get("https://www.sports-reference.com/cfb/conferences/sec/2017.html")
sec_soup = bs4.BeautifulSoup(sec_2017.text, 'lxml')
sec_table = sec_soup.select('table')
sec_df = pd.read_html(str(sec_table))[0]
sec_df.columns = sec_df.columns.droplevel(0)
sec_df.rename(columns={sec_df.columns[0]: "School" }, inplace = True)

#Get 2017 records for Big12
big12_2017 = requests.get("https://www.sports-reference.com/cfb/conferences/big-12/2017.html")
big12_soup = bs4.BeautifulSoup(big12_2017.text, 'lxml')
big12_table = big12_soup.select('table')
big12_df = pd.read_html(str(big12_table))[0]
big12_df.columns = big12_df.columns.droplevel(0)
big12_df.rename(columns={big12_df.columns[0]: "School" }, inplace = True)

#Get 2017 records for PAC12
pac12_2017 = requests.get("https://www.sports-reference.com/cfb/conferences/pac-12/2017.html")
pac12_soup = bs4.BeautifulSoup(pac12_2017.text, 'lxml')
pac12_table = pac12_soup.select('table')
pac12_df = pd.read_html(str(pac12_table))[0]
pac12_df.columns = pac12_df.columns.droplevel(0)
pac12_df.rename(columns={pac12_df.columns[0]: "School" }, inplace = True)

#Get 2017 records for Independents
ind_2017 = requests.get("https://www.sports-reference.com/cfb/conferences/independent/2017.html")
ind_soup = bs4.BeautifulSoup(ind_2017.text, 'lxml')
ind_table = ind_soup.select('table')
ind_df = pd.read_html(str(ind_table))[0]
ind_df.columns = ind_df.columns.droplevel(0)
ind_df.rename(columns={ind_df.columns[0]: "School" }, inplace = True)

#Get 2017 records for AAC
aac_2017 = requests.get("https://www.sports-reference.com/cfb/conferences/american/2017.html")
aac_soup = bs4.BeautifulSoup(aac_2017.text, 'lxml')
aac_table = aac_soup.select('table')
aac_df = pd.read_html(str(aac_table))[0]
aac_df.columns = aac_df.columns.droplevel(0)
aac_df.rename(columns={aac_df.columns[0]: "School" }, inplace = True)

#Get 2017 records for Mountain West
mwc_2017 = requests.get("https://www.sports-reference.com/cfb/conferences/mwc/2017.html")
mwc_soup = bs4.BeautifulSoup(mwc_2017.text, 'lxml')
mwc_table = mwc_soup.select('table')
mwc_df = pd.read_html(str(mwc_table))[0]
mwc_df.columns = mwc_df.columns.droplevel(0)
mwc_df.rename(columns={mwc_df.columns[0]: "School" }, inplace = True)

#Get 2017 records for MAC
mac_2017 = requests.get("https://www.sports-reference.com/cfb/conferences/mac/2017.html")
mac_soup = bs4.BeautifulSoup(mac_2017.text, 'lxml')
mac_table = mac_soup.select('table')
mac_df = pd.read_html(str(mac_table))[0]
mac_df.columns = mac_df.columns.droplevel(0)
mac_df.rename(columns={mac_df.columns[0]: "School" }, inplace = True)

#Get 2017 records for CUSA
cusa_2017 = requests.get("https://www.sports-reference.com/cfb/conferences/cusa/2017.html")
cusa_soup = bs4.BeautifulSoup(cusa_2017.text, 'lxml')
cusa_table = cusa_soup.select('table')
cusa_df = pd.read_html(str(cusa_table))[0]
cusa_df.columns = cusa_df.columns.droplevel(0)
cusa_df.rename(columns={cusa_df.columns[0]: "School" }, inplace = True)

#Get 2017 records for Sun Belt
sun_2017 = requests.get("https://www.sports-reference.com/cfb/conferences/sun-belt/2017.html")
sun_soup = bs4.BeautifulSoup(sun_2017.text, 'lxml')
sun_table = sun_soup.select('table')
sun_df = pd.read_html(str(sun_table))[0]
sun_df.columns = sun_df.columns.droplevel(0)
sun_df.rename(columns={sun_df.columns[0]: "School" }, inplace = True)

#Create DataFrame for all conferences
results_2017 = big10_df.append([
    acc_df,
    sec_df,
    big12_df,
    pac12_df,
    ind_df,
    aac_df,
    mwc_df,
    mac_df,
    cusa_df,
    sun_df])

#Rename schools for results
results_2017 = results_2017.replace({'School': {
'Bowling Green State':'Bowling Green',
'UCF':'Central Florida',
'Charlotte':'UNC Charlotte',
'Louisiana':'Louisiana-Lafayette',
'Massachusetts':'UMass',
'Miami (FL)':'Miami',
'Miami (OH)':'Miami-OH',
'Middle Tennessee State':'Middle Tennessee',
'Ole Miss':'Mississippi',
'Nevada-Las Vegas':'UNLV',
'North Carolina State':'NC State',
'Pitt':'Pittsburgh',
'Southern Mississippi':'Southern Miss',
'Texas Christian':'TCU'}})

###############
### Merge 4 ###
###############
coaches_final = coaches_final.merge(results_2017, how='left', on='School')
###########################
### Cleanup after merge ###
###########################
#Retain the first 22 columns
coaches_final = coaches_final.iloc[:,[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21]]
#Rename columns
coaches_final = coaches_final.rename({'W_x':'SchoolWins17','L_x':'SchoolLosses17'}, axis=1)
coaches_final = coaches_final.rename({'W':'SchoolWins17','L':'SchoolLosses17'}, axis=1)
#Change SchoolWins and Losses to int
coaches_final['SchoolWins17'] = coaches_final['SchoolWins17'].astype(str).astype(int)
coaches_final['SchoolLosses17'] = coaches_final['SchoolLosses17'].astype(str).astype(int)

###############
### Liberty ###
###############
#Liberty did not play in Divisino 1 in 2017 but used their records from that season.
coaches_final[coaches_final['SchoolWins17'].isna()]
coaches_final.loc[50,'SchoolWins17']= 6
coaches_final.loc[50,'SchoolLosses17']= 5
#Liberty's stadium was not listed in the data set found online
coaches_final.loc[50,'StadiumCapacity']= 25000

#####################
### Dropping Data ###
#####################
#Dropping Syracuse from the data since that is who we are making a prediction on
coaches_final = coaches_final[coaches_final.School != 'Syracuse']
#Decided to remove first-year coaches since they do not have any wins.
#This is done, in part, because we are making a prediction for a coach that has 4 years of experience
#A different approach would have been considered if Syracuse was hiring a first-time coach.
coaches_final.isnull().sum(axis = 0)
coaches_final = coaches_final.drop(coaches_final[coaches_final.Coach.isin(["Herm Edwards",
                                                           "Josh Heupel",
                                                           "Joe Moglia",
                                                           "Sean Lewis",
                                                           "Billy Napier",
                                                           "Joe Moorhead",
                                                           "Jonathan Smith",
                                                           "Mike Bloomgren",
                                                           "Steve Campbell",
                                                           "Jeremy Pruitt"])].index)

#####################
### Data Cleaning ###
#####################
#change certain objects to ints
#SchoolPay, TotalPay, Bonus, BonusPaid, Buyout
coaches_final['SchoolPay'] = pd.to_numeric(coaches_final.SchoolPay, errors='coerce').convert_dtypes()
coaches_final['TotalPay'] = pd.to_numeric(coaches_final.TotalPay, errors='coerce').convert_dtypes()
coaches_final['Bonus'] = pd.to_numeric(coaches_final.Bonus, errors='coerce').convert_dtypes()
coaches_final['BonusPaid'] = pd.to_numeric(coaches_final.BonusPaid, errors='coerce').convert_dtypes()
coaches_final['Buyout'] = pd.to_numeric(coaches_final.Buyout, errors='coerce').convert_dtypes()

#Appears to be no values other than 0 for coaches pay, but want to confirm
#coaches_final.AssistantPay.sum()
#Drop AssistantPay
coaches_final.drop(['AssistantPay'],axis =1, inplace=True)

#Obtain mean for report
#coaches_final.SchoolPay.mean()
#coaches_final.CareerW.mean()
#coaches_final.CareerL.mean()
#coaches_final.CareerT.mean()
#coaches_final.CareerWinPct.mean()
#coaches_final.AvgSRS.mean()
#coaches_final.AvgSOS.mean()
#coaches_final.SeasonsRanked.mean()
#coaches_final.StadiumCapacity.mean()
#coaches_final.GSR.mean()
#coaches_final.FGR.mean()
#coaches_final.SchoolWins17.mean()
#coaches_final.SchoolLosses17.mean()

#Tried to build a histogram for School Pay and it could not due to NAs
#Identified that there are 4 coaches with NAs in this column:
#Matt Rhule at Baylor
#Kalani Sitake
#Mike Bloomgren
#Sonny Dykes
coaches_final[coaches_final['SchoolPay'].isna()]
#Went online and found contract values for Bloomgreen and Dykes. Decided to Drop Rhule and Sitake.
#https://sports.usatoday.com/ncaa/salaries/
#Mike Bloomgren 1332308. He was removed above so no longer going to include.
#Also needed to populate Dykes Total pay to help build a histogram
#coaches_final.loc[91,'SchoolPay']=1332308
coaches_final.loc[99,'SchoolPay']=1340314
coaches_final.loc[99,'TotalPay']=1340314
#Drop Rhule and Sitake
coaches_final = coaches_final[coaches_final.Coach != 'Matt Rhule']
coaches_final = coaches_final[coaches_final.Coach != 'Kalani Sitake']

#Replace missing FGR values with a value of 63, which is rounded up from the mean of 62.58
coaches_final.loc[0,'FGR']=63
coaches_final.loc[9,'FGR']=63
coaches_final.loc[21,'FGR']=63


###############
### Visuals ###
###############
#View distribution of School Pay
coaches_final['SchoolPay'].hist(color='#F04F22', edgecolor='#061F3E')
plt.title('Distribution of Coaches Salaries: 2017')
plt.xlabel("Salaries By Millions", size=12)
plt.ylabel("Count", size=12)
plt.grid(b=None)
plt.show()

#Create histogram of Total Pay
coaches_final['TotalPay'].hist(color='#061F3E', edgecolor='#F04F22')
plt.title('Distribution of Coaches Total Pay: 2017')
plt.xlabel("Salaries By Millions", size=12)
plt.ylabel("Count", size=12)
plt.grid(b=None)
plt.show()

#View Log Transformation
SchoolPay_log = coaches_final['SchoolPay'].transform(np.log)
SchoolPay_log.hist(color='#F04F22', edgecolor='#061F3E')
plt.suptitle("SchoolPay Log Transformed")
plt.grid(None)
plt.show()

#View Square Root Transformation
SchoolPay_sqrt = coaches_final['SchoolPay'].transform(np.sqrt)
SchoolPay_sqrt.hist(color='#F04F22', edgecolor='#061F3E')
plt.suptitle("SchoolPay Square Root Transformed")
plt.grid(None)
plt.show()

#Drop Total Pay, Bonus, Bonus Paid and Buyout based on visuals and observations
coaches_final = coaches_final.drop(['TotalPay'], axis=1)
coaches_final = coaches_final.drop(['Bonus'], axis=1)
coaches_final = coaches_final.drop(['BonusPaid'], axis=1)
coaches_final = coaches_final.drop(['Buyout'], axis=1)

#Create box plot based on school pay and conference
my_pal = {'Mt. West':'#D64309', 'MAC':'#FFC82E', 'SEC':'#9E1B32', 'C-USA':'#00B140', 'Sun Belt':'#222222', 'Pac-12':'#2D68C4', 'Ind.':'#AE9142', 'Big 12':'#BF5700', 'ACC':'#522D80', 'AAC':'#FFC904','Big Ten':'#041E42'}
box= sns.boxplot(x='Conference', y='SchoolPay', data=coaches_final, palette=my_pal)
box.tick_params(labelsize=10)
box.set_xticklabels(box.get_xticklabels(),rotation=45)
plt.tight_layout()
plt.title('Distribution of Salaries By Conference: 2017')
plt.xlabel("Conference", size=10)
plt.ylabel("Salary in Millions", size=10)
plt.show()

#Convert CareerW to int to create scatter plot
coaches_final['CareerW'] = pd.to_numeric(coaches_final['CareerW']).round(0).astype(int)

#Scatter Plots
#Career Wins and School Pay
plt.scatter(x='CareerW', y='SchoolPay', data=coaches_final, color='#F04F22', edgecolor='#061F3E')
plt.title('Relationship Between Wins\n and Coach Salary: 2017')
plt.xlabel("Career Wins", size=12)
plt.ylabel("Coach Salary in  Millions", size=12)
plt.tight_layout()
plt.show()

#Relationship between Seasons Ranked and School Pay
plt.scatter(x='SeasonsRanked', y='SchoolPay', data=coaches_final, color='#F04F22', edgecolor='#061F3E')
plt.title('Relationship Between Seasons Ranked\n and Coach Salary: 2017')
plt.xlabel("Number of Times Ranked at End of Season", size=12)
plt.ylabel("Coach Salary in  Millions", size=12)
plt.tight_layout()
plt.show()

#############
### Model ###
#############
#Build model
#Change Conference from Object to category
coaches_final['Conference'] = coaches_final['Conference'].astype('category')
#Check for NAs
coaches_final.isnull().sum(axis = 0)

#Create dummy variables
#Create a variable to hold the conferences
data_cat = coaches_final['Conference']
#Create a data frame that holds the dummy variables
coaches_conf = pd.get_dummies(data_cat,dummy_na=False, drop_first=True)
#Combine the Coaches Final and Coaches Conference
coaches_pred = pd.concat([coaches_final, coaches_conf], axis=1)
#Remove conferences since it is no longer needed
coaches_pred.drop(['Conference'],axis =1, inplace=True)
#Remove coachID
coaches_pred.drop(['coachID'],axis =1, inplace=True)

#############################################
#######      MODEL 1 Includes All    ########
#############################################
#Create x and y variable for prediction
x=coaches_pred.drop(['SchoolPay','Coach','School'], axis=1).values
y=coaches_pred['SchoolPay'].values
#Split into testing and training
x_train, x_test, y_train, y_test = train_test_split(x,y, test_size=0.3, random_state=0)

ml = LinearRegression()
ml.fit(x_train, y_train)

#Predict the Result
y_pred = ml.predict(x_test)

#Provide input for Syracuse Coach
#Prediction is 2,310,360. Actual is 2,401,206
ml.predict([[26,25,0,0.509803922,-0.28,1.04,0,49250,85,70,4,8,1,0,0,0,0,0,0,0,0,0]])

#Evaluate the model
#R2 = 0.8084369573972316
from sklearn.metrics import r2_score
r2_score(y_test,y_pred)

####################################################
#######  MODEL 2 Drops Ties, GSR, and FGR   ########
####################################################
#Create x and y variable for prediction
x=coaches_pred.drop(['SchoolPay','Coach','School','CareerT','GSR','FGR'], axis=1).values
y=coaches_pred['SchoolPay'].values
#Split into testing and training
x_train, x_test, y_train, y_test = train_test_split(x,y, test_size=0.3, random_state=0)

#Training
ml = LinearRegression()
ml.fit(x_train, y_train)

#Predict the Result
y_pred = ml.predict(x_test)

#Provide input for Syracuse Coach
#Prediction is 2,637,263. Actual is 2,401,206
ml.predict([[26,25,0.509803922,-0.28,1.04,0,49250,4,8,1,0,0,0,0,0,0,0,0,0]])

#Evaluate the model
#R2 = 0.8579385824726855
r2_score(y_test,y_pred)


###################################
#######  MODEL 3 Use OLS   ########
###################################
X3 = coaches_pred.drop(['SchoolPay','Coach','School'], axis=1)
Y3 = coaches_pred[['SchoolPay']]
model3 = sm.OLS(Y3.astype(float), X3.astype(float)).fit()
print(model3.summary())
#R-squared (uncentered): 0.950


###################################
#######  MODEL 4 Use OLS   ########
###################################
#Drop Ties, GSR, FGR
X4 = coaches_pred.drop(['SchoolPay','Coach','School','CareerT','GSR','FGR'], axis=1)
Y4 = coaches_pred[['SchoolPay']]
model4 = sm.OLS(Y4.astype(float), X4.astype(float)).fit()
print(model4.summary())
#R-squared (uncentered): 0.949

###################################
#######  MODEL 5 Use OLS   ########
###################################
#Drop CarrerW, CareerL, CareerT, GSR, FGR, AvgSRS, AvgSOS, SchoolWins17, SchoolLosses17
X5 = coaches_pred.drop(['SchoolPay','Coach','School', 'CareerW', 'CareerL', 'CareerT','GSR','FGR', 'AvgSRS', 'AvgSOS', 'SchoolWins17', 'SchoolLosses17'], axis=1)
Y5 = coaches_pred[['SchoolPay']]
model5 = sm.OLS(Y5.astype(float), X5.astype(float)).fit()
print(model5.summary())
#R-squared (uncentered): 0.947


###################################
#######  MODEL 6 Use OLS   ########
###################################
#Drop CarrerWinPct, CareerL, CareerT, GSR, FGR, AvgSRS, AvgSOS, SchoolWins17, SchoolLosses17
X6 = coaches_pred.drop(['SchoolPay','Coach','School', 'CareerWinPct', 'CareerL', 'CareerT','GSR','FGR', 'AvgSRS', 'AvgSOS', 'SchoolWins17', 'SchoolLosses17'], axis=1)
Y6 = coaches_pred[['SchoolPay']]
model6 = sm.OLS(Y6.astype(float), X6.astype(float)).fit()
print(model6.summary())
#R-squared (uncentered): 0.943

###################################
#######  MODEL 7 Use OLS   ########
###################################
#Drop CarrerWinPct, CareerL, CareerT, GSR, FGR, AvgSRS, AvgSOS, SchoolWins17, SchoolLosses17
X7 = coaches_pred.drop(['Ind.', 'Mt. West','Sun Belt','MAC','C-USA', 'SchoolPay','Coach','School', 'CareerW', 'CareerL', 'CareerT','GSR','FGR', 'AvgSRS', 'AvgSOS', 'SchoolWins17', 'SchoolLosses17'], axis=1)
Y7 = coaches_pred[['SchoolPay']]
model7 = sm.OLS(Y7.astype(float), X7.astype(float)).fit()
print(model7.summary())
#R-squared (uncentered): 0.943

###################################
#######  MODEL 8 Use OLS   ########
###################################
X8 = coaches_pred.drop(['CareerL', 'CareerT','SchoolPay','Coach','School', 'AvgSRS','CareerW','GSR','AvgSOS','SchoolWins17','FGR','SchoolLosses17'], axis=1)
Y8 = coaches_pred[['SchoolPay']]
model8 = sm.OLS(Y8.astype(float), X8.astype(float)).fit()
print(model8.summary())
##R-squared: 0.947