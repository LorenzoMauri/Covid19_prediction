#per fare una stima dei morti covid, dato che non abbiamo tutti i comuni ma solo un campione non casuale ma condizionato,  
#possiamo calcolare i morti covid con 'precisione' per i comuni di cui abbiamo i dati dell'Istat e per i restanti possiamo 
#considerare come limite inferiore che non ci siano stati morti covid, e come limite superiore che la mortalità per covid sia 
#stata pari a quella dei comuni campionati.
#Ho pensato un po' ma non mi è venuto in mente un metodo più preciso per stimare i morti perchè i dati che non abbiamo 
#potrebbero rientrare o nei comuni che non rientrano nell'ANPR, o nei comuni che non hanno avuto un incremento del 20% di 
#morti o che hanno avuto almeno 10 morti tra gennaio e marzo..
#ALCUNE NOTE:
#1) in realtà non calcoliamo i morti covid, ma i morti relazionati al covid (per esempio a causa delle restrizioni del governo 
#e della quarantena, il numero di morti per incidenti stradali e sul lavoro sarà diminuito.. ).  
#Ci sono troppi fattori che non consideriamo e che dovremmo avere conoscenza specifica nel settore per stimare

#a questo link c'è un articolo di alcuni tipi che hanno provato anche loro a fare una stima dei morti covid... 
#l'idea è molto simile a quella che ho usato io
#https://www.scienzainrete.it/articolo/verso-stima-di-morti-dirette-e-indirette-covid/enrico-bucci-luca-leuzzi-enzo-marinari


library(dplyr)   # pacchetto per "data wrangling"
library(tidyr)   # funzioni utili per tidy data: noi useremo gather()
library(ggplot2) # pacchetto per grafici secondo "grammar of graphics"
library(plotly)  # per grafici interattivi che usano la libreria plotly (https://plotly.com/)

wd <- paste(dirname(rstudioapi::getActiveDocumentContext()$path), "data", sep = .Platform$file.sep)
setwd(wd)

#POPOLAZIONE NEI COMUNI ITALIANI
#quello che mi interessa è il numero di abitanti e il codice Istat del comune, COD_PROVCOM
pop<-read.csv("Popolazione_comuni_italiani.csv")

pop %>% rename( COD_PROVCOM = ï..ITTER107, ABITANTI = Value )%>%
        filter(Sesso=="totale")%>%
        select(-c(Territorio:Seleziona.periodo ,Flag.Codes,Flags))->pop

#poichè mi considera il numero di abitanti come lista, trasformo in character e poi in vector
#(non potevo fare direttamente) non so se c'è modo migliore...
pop$COD_PROVCOM<-as.character(pop$COD_PROVCOM)
pop$COD_PROVCOM<-as.numeric(pop$COD_PROVCOM)

pop <- na.omit(pop)
#ci sono alcune righe che non sono comuni ma regioni, aree geografiche, quindi le tolgo. 
#Alla fne risultano 7926 comuni.




#DECESSI
td<-read.csv("comune_giorno.csv")

#sistemo il data frame in modo che sia più funzionale, come abbiamo fatto a lezione
td<- td %>% gather(key="SESSO_ANNO", value="DECESSI", MASCHI_15:TOTALE_20)
splt_sesso_anno <-strsplit(td$SESSO_ANNO, "_", fixed=TRUE)
td$SESSO<-sapply(splt_sesso_anno, function(x) x[1])
td$ANNO<-sapply(splt_sesso_anno, function(x) x[2])
td$DATA<-as.Date(paste0("0", td$GE, "2020"), format="%m%d%Y")

#faccio una join con pop così aggiungo la colonna con il numero di abitanti per comune
td<-left_join(td, pop, by="COD_PROVCOM")

#Escono dei NAN (una decina di paesi, pochi!!), li tolgo! forse c' è modo un po' più preciso?!?!
#questi NAN sono dovuti al fatto che i due diversi dataframe sono aggiornati a date diverse. 
#pop è aggornato al 01/01/2019, td al 04/04/2020. 
#In questo lasso di tempo alcuni comuni si sono fusi tra di loro, ne sono nati altri...
td <- na.omit(td)


#pulisco da variabili che non servono perchè se no R è troppo lento a fare tutto!!
td %>% select(-c(NOME_PROVINCIA,REG,SESSO_ANNO, PROV, CL_ETA,GE))%>%
      filter(SESSO=="TOTALE", DECESSI<9999, format(as.Date(DATA), "%m")!="04" )%>%
      group_by(ANNO, NOME_REGIONE, NOME_COMUNE, COD_PROVCOM, DATA_INIZIO_DIFF, ABITANTI)%>%
      summarise(DECESSI=sum(DECESSI))->tdp
#ho contato le date da gennaio a marzo, si potrebbero considerare intervalli diversi





#Adesso andiamo a vedere la percentuale di popolazione, relativa alle diverse regioni, 
#di cui l'Istat ci ha fornito i dati e la percentuale di popolazione che rientra neli comuni considerati dall'ANPR. 
#il file l'ho trovato a questo link
#https://www.anpr.interno.it/portale/tabelle-di-riferimento
ANPR<-read.csv("Tabella_45 Comuni subentrati.csv",sep=";")
ANPR$DATASUBENTRO<-as.character(ANPR$DATASUBENTRO)
ANPR$DATASUBENTRO<-as.Date.character(ANPR$DATASUBENTRO,"%d/%m/%Y")
ANPR$diffDate<-as.numeric(as.Date("2020-04-17")-ANPR$DATASUBENTRO)

#tengo solo le variabili che mi ineressano e considero solo i comuni che fanno parte dei comuni considerati dall'ANPR 
#in data 16/04/2020
ANPR %>% select(CODISTAT, DATASUBENTRO, diffDate)%>%
        filter(diffDate>0)%>%
        select(-c(diffDate,DATASUBENTRO))%>%
        rename(COD_PROVCOM=CODISTAT)->ANPR

#dataframe con abitanti per comune, COD_PROVCOM e relativa regione di appartenenza
td %>% distinct(NOME_REGIONE,COD_PROVCOM, ABITANTI)%>%
      group_by(COD_PROVCOM, ABITANTI)->AbitantiPerComune

left_join(ANPR, AbitantiPerComune, by="COD_PROVCOM")->ANPR
#non ho la regione di tutti i comuni!! le versioni non sono aggiornate alla stesa data. 
#Anche qui un po' impreciso. Come potrei fare?
ANPR <- na.omit(ANPR)


ANPR %>% group_by(NOME_REGIONE)%>%
        summarise(abitantiANPR=sum(ABITANTI))->ANPR

#abitanti per regione che rientrano nell'ANPR
td %>% distinct(NOME_REGIONE, COD_PROVCOM, ABITANTI)%>%
      group_by(NOME_REGIONE)%>%
      summarise(abitanti=sum(ABITANTI))->AbitantiPerRegione

inner_join(AbitantiPerRegione, ANPR, by="NOME_REGIONE")->ANPR

#abitanti per regione dei comuni considerati dall'Istat
td %>% filter(DATA_INIZIO_DIFF!="Dati 2020 n.d." )%>%
      distinct(NOME_COMUNE, NOME_REGIONE, ABITANTI)%>%
      group_by(NOME_REGIONE)%>%
      summarise(abitantiCampionati=sum(ABITANTI))->AbitantiPerRegioneCampionati

inner_join( ANPR, AbitantiPerRegioneCampionati, by="NOME_REGIONE")->TOT

#dataframe riassuntivo che compara le varie percentuali
TOT %>% mutate(percANPR=abitantiANPR/abitanti*100,percCAMPIONATI=abitantiCampionati/abitanti*100)->TOT

#Come si vede per la Lombardia il campionamento messo a disposizione dall'Istat costituisce un'ampia 
#copertura del totale. quindi per me se analizziamo la Lombardia possiamo fare stime più precise 
#rispetto a considerare tutta l'Italia o altre regioni



#ANALIZZO I MORTI
#io farei Lombardia perchè mi sembra più omogeneo rispetto a tutta Italia
tdp %>% filter(NOME_REGIONE=="Lombardia")->tdpl
TOT %>%  filter(NOME_REGIONE=="Lombardia")->Lombardia

#morti per covid nei comuni campionati dall'Istat
tdpl %>% filter(DATA_INIZIO_DIFF!="Dati 2020 n.d.")%>%
        group_by(ANNO)%>%
        summarise(DECESSIperanno=sum(DECESSI))->tdplc

#morti per covid nei comuni non campionati dall'Istat
#per trovare i morti dovuti al covid considero la media dei morti negli anni 15-19 e li sottraggo ai morti di quest'anno 
#(sempre nello stesso periodo gennaio_marzo)
tdplc$DECESSIperanno[6]-mean(tdplc$DECESSIperanno[1:5])->mortiLombardiaCampionata
#in realtà quest'anno è stato un'anno più mite, ci sono state poche influenze, e prima di covid meno morti 
#rispetto agli anni precedenti. Anche il 2016 ha avuto un comportamento simile. Quindi invece che sottrarre la
#media dei morti 15-19 potremmo sottrarre  i morti del 2016
#tdplc$DECESSIperanno[6]-(tdplc$DECESSIperanno[2])

#percentuale di morti covid sulla popolazione campionata dall'Istat
mortalitàLombardiaCampionata<-as.vector(mortiLombardiaCampionata/Lombardia$abitantiCampionati)


#morti per covid nei comuni non campionati 
#(posso considerare come limite inferiore nessun morto per covid e come limite superiore che ci sia stata una 
#mortalità per covid pari a quella dei comuni campionati dall'Istat)
mortiLombardiaCampionata/Lombardia$abitantiCampionati*(Lombardia$abitantiANPR-Lombardia$abitantiCampionati)->mortiLombardiaNonCampionatasup

#mortalitàLombardiaNonCampionata<-as.vector(mortiLombardiaNonCampionata/(Lombardia$abitantiANPR-Lombardia$abitantiCampionati)*100)

#morti per covid nei comuni non ANPR. 
#considero che i comuni ANPR abbiano una mortalità per covid pari a quella media dei comuni dell'Istat
#anche in questo caso posso fare limite superiore e limite inferiore come sopra
(Lombardia$abitanti-Lombardia$abitantiANPR)*((mortiLombardiaCampionata)/Lombardia$abitantiANPR)->mortiLombardiaNonANPRinf
(Lombardia$abitanti-Lombardia$abitantiANPR)*((mortiLombardiaCampionata+mortiLombardiaNonCampionatasup)/Lombardia$abitantiANPR)->mortiLombardiaNonANPRsup
#equivale a: (Lombardia$abitanti-Lombardia$abitantiANPR)*(mortiLombardiaCampionata/Lombardia$abitantiCampionati)


mortiCOVIDinf<-mortiLombardiaCampionata+mortiLombardiaNonANPRinf
mortiCOVIDsup<-mortiLombardiaCampionata+mortiLombardiaNonCampionatasup+mortiLombardiaNonANPRsup


#intervallo di confidenza morti covid in Lombardia fino a fine marzo
print(c(mortiCOVIDinf, mortiCOVIDsup))
#(12376.35  15042.63)
#intervallo abbastanza ampio. C'è un'altro modo per fare in modo più preciso?
#per me ci sono troppe altre variabili da considerare!!!!!