#### Yann Aubineau --- Exemple nettoyage de données ####


set.seed(1234) # Toujours setup l'aléatoire pour produire des tableaux/documents reproductibles 

#### Packages ####

library(questionr) # Tris croisés 
library(tidyverse) # Ensemble de packages dont la grammaire est utilisé ici
library(forcats) # fct_relevel pour changer l'ordre factors



### Ouverture des données ####
CDT2016 <- read.csv2("Data/CDT2016.csv")

#### (1) Isoler le champ 

###  (1.a) Les travailleurs en hôpitaux publics

CDT2016 <- CDT2016 %>% mutate( # Création de variables
  statut = case_when( # syntaxe : condition ~ string/num, condition ~ string/num, ..., T (tout le reste) ~ string/num/NA 
                         STATUT == 3 ~ "Hôpital public",
                         STATUT == 4 ~ "Etablissement privé"),
  profession = case_when(PE == "311C" ~ "Chirurgiens dentistes",
                         PE == "311D" ~ "Psychologues",
                         PE == "344A" ~ "Médecins",
                         PE == "344C" ~ "Internes",
                         PE == "431B" ~ "Infirmiers psychiatriques",
                         PE == "431C" ~ "Puéricultrices",
                         PE == "431D" ~ "Infirmiers spécialisés",
                         PE == "431E" ~ "Sages-femmes",
                         PE == "431F" ~ "Infirmiers généraux",
                         PE == "432B" ~ "Kinésithérapeutes",
                         PE == "432D" ~ "Rééducation autre",
                         PE == "433B" ~ "Opticiens et audio",
                         PE == "526A" ~ "Aide-soignants",
                         PE == "526B" ~ "Assistants médicaux",
                         PE == "526C" ~ "Auxiliaires puériculture",
                         PE == "526D" ~ "Aides médico-psychologiques"))

hp2016 <- filter(CDT2016, STATUT %in% c(3,4)) # Isole les travailleurs en hôpitaux public et privé, le privé étant parfois rattaché au public

### (1.b) Isoler les travailleurs d'intérêt = considèrent qu'ils soignent les gens comme principale activité > 50%

personnel_soignant <- c("344A", # Profession 344a : Médecins hospitaliers sans activité libérale
                        "311C" , # Chirurgiens dentistes
                        "311D" , # Psychologues
                        # [ELIMINE] : MEDECIN SALARIES NON HOSPITALIERS [66.7%]
                        #"344B", # Profession 344b : Médecins salariés non hospitaliers
                        
                        "344C", # Profession 344c : Internes en médecine, odontologie et pharmacie
                        
                        # [ELIMINE] : PHARMACIENS [68.4%]
                        #"344D", # Profession 344d : Pharmaciens salariés [représente 19 personnes en 2013]
                        
                        # [ELIMINE] : INFIRMIERS CADRES [50.9%]
                        # "431A", # Profession 431a : Cadres infirmiers et assimilés
                        
                        "431B", # Profession 431b : Infirmiers psychiatriques
                        "431C", # Profession 431c : Puéricultrices
                        "431D", # Profession 431d : Infirmiers spécialisés (autres qu'infirmiers psychiatriques et puéricultrices)
                        "431E", # Profession 431e : Sages-femmes (libérales ou salariées)
                        "431F", # Profession 431f : Infirmiers en soins généraux, salariés
                        
                        # [CHOIX] : NE PAS PRENDRE EN COMPTE LES 431A et 431C avec etiquette LIBERAUX
                        
                        "432B", # Profession 432b : Masseurs-kinésithérapeutes rééducateurs, salariés
                        "432D", # Profession 432d : Autres spécialistes de la rééducation, salariés
                        
                        # [ELIMINE] : TECHNICIEN MEDICAUX [56%]
                        #"433A", # Profession 433a : Techniciens médicaux
                        "433B", # Profession 433b : Opticiens lunetiers et audioprothésistes (indépendants et salariés)
                        
                        # [ELIMINE] : APPAREILLAGE [0%]
                        #"433C", # Profession 433c : Autres spécialistes de l'appareillage médical (indépendants et salariés)
                        
                        # [ELIMINE] : PREPARATEURS EN PHARMACIE [56.8%]
                        #"433D", # Profession 433d : Préparateurs en pharmacie
                        
                        "526A", # Profession 526a : Aides-soignants (de la fonction publique ou du secteur privé)"
                        
                        # [DOUTE] : ASSISTANT MEDICAUX [30.8%]
                        "526B", # Profession 526b : Assistants dentaires, médicaux et vétérinaires, aides de techniciens médicaux {représente 13 personnes en 2013}
                        
                        "526C",  # Profession 526c : Auxiliaires de puériculture"
                        "526D" # Profession 526d : Aides médico-psychologiques
                        
                        # [CHOIX] : DE NE PAS PRENDRE LES AMBULANCIERS
                        
                        # [ELIMINE] : TRAVAILLEUSES FAMILLIALES [31.4%]
                        #"563B" # Profession 563b : Aides à domicile, aides ménagères, travailleuses familiales
                        
)

### (1.c) Première version du champ, qui comporte encore l'ensemble des professions soignantes 

data <- hp2016 %>% filter(PE %in% personnel_soignant) %>% # Champ
  filter(NUIT %in% c(1,2,3)) # Variable d'intérêt, doivent nécessairement avoir répondu à cette question


#### (2) Ajout de variables importantes et plus lisibles

data <- data %>% mutate(
  sexe = case_when(SEXE == 1 ~ "Homme",
                   SEXE == 2 ~ "Femme"),
  age_10 = case_when(AGEQ %in% c(15,20,25) ~ "15-29 ans",
                     AGEQ %in% c(30,35) ~ "30-39 ans",
                     AGEQ %in% c(40,45) ~ "40-49 ans",
                     AGEQ %in% c(50,55) ~ "50-59 ans",
                     AGEQ %in% c(60,65) ~ "60-69 ans",
                     AGEQ %in% c(70,80) ~ "70-84 ans"),
  couple = case_when(COUPLE == 1 ~ "Oui, dans logement",
                     COUPLE == 2 ~ "Oui, pas dans logement",
                     COUPLE == 3 ~ "Non, pas en couple"),
  typmen5b = case_when(typmen5 == 1 ~ "Personne seule",
                       typmen5 == 2 ~ "Famille mono",
                       typmen5 == 3 ~ "Couple sans enfant",
                       typmen5 == 4 ~ "Couple avec enfant",
                       typmen5 == 5 ~ "Autre menage"),
  profession = case_when(PE == "311C" ~ "Chirurgiens dentistes",
                         PE == "311D" ~ "Psychologues",
                         PE == "344A" ~ "Médecins",
                         PE == "344C" ~ "Internes",
                         PE == "431B" ~ "Infirmiers psychiatriques",
                         PE == "431C" ~ "Puéricultrices",
                         PE == "431D" ~ "Infirmiers spécialisés",
                         PE == "431E" ~ "Sages-femmes",
                         PE == "431F" ~ "Infirmiers généraux",
                         PE == "432B" ~ "Kinésithérapeutes",
                         PE == "432D" ~ "Rééducation autre",
                         PE == "433B" ~ "Opticiens et audio",
                         PE == "526A" ~ "Aide-soignants",
                         PE == "526B" ~ "Assistants médicaux",
                         PE == "526C" ~ "Auxiliaires puériculture",
                         PE == "526D" ~ "Aides médico-psychologiques"),
  tpp = case_when(TPP == 1 ~ "Temps complet",
                  TPP == 2 ~ "Temps partiel"),
  JOURTR_num = as.numeric(JOURTR),
  jourtr = case_when(JOURTR_num < 3 ~ "Moins de 3 jours travaillés",
                     JOURTR_num >= 3 & JOURTR_num < 4  ~ "3 - 3,5 jours travaillés",
                     JOURTR_num >= 4 & JOURTR_num < 5 ~ "4 - 4,5 jours travaillés",
                     JOURTR_num >= 5 & JOURTR_num < 6 ~ "5 jours travaillés",
                     JOURTR_num > 5 ~ "Plus de 5 jours travaillés"),
  nuit = case_when(NUIT == 1 ~ "Nuit Habituellement",
                   NUIT == 2 ~ "Nuit Occasionnellement",
                   NUIT == 3 ~ "Nuit Jamais"),
  nuit_tab = case_when(NUIT == 1 ~ "Habituellement",
                       NUIT == 2 ~ "Occasionnellement",
                       NUIT == 3 ~ "Jamais"),
  statut = case_when(STATUT == 3 ~ "Hôpital public",
                     STATUT == 4 ~ "Etablissement privé"),
  hh = case_when(HH < 35 ~ "Moins de 35h travail",
                 HH == 35 ~ "35h travail",
                 HH > 35 ~ "Plus de 35h travail"),
  repos = case_when(REPOS == 1 ~ "Au moins 48h repos",
                    REPOS == 2 ~ "Pas 48h repos"),
  samedi = case_when(SAMEDI == 1 ~ "Samedi Habituellement",
                     SAMEDI == 2 ~ "Samedi Occasionnellement",
                     SAMEDI == 3 ~ "Samedi Jamais"),
  dimanche = case_when(DIMANCHE == 1 ~ "Dimanche Habituellement",
                       DIMANCHE == 2 ~ "Dimanche Occasionnellement",
                       DIMANCHE == 3 ~ "Dimanche Jamais"),
  # forfait = case_when(FORFAIT == 1 ~ "Forfait jour oui",
  #                     FORFAIT == 2 ~ "Forfait jour non"),
  previs = case_when(PREVIS == 1 ~ "Connais ses horaires du mois",
                     PREVIS == 2 ~ "Connais ses horaires de semaine prochaine",
                     PREVIS == 3 ~ "Connais ses horaires que de demain",
                     PREVIS == 4 ~ "Aucune connaissance des horaires"),
  horvar = case_when(HORVAR == 1 ~ "Horaire toujours même",
                     HORVAR == 2 ~ "Alternant 2x8",
                     HORVAR == 3 ~ "Alternant 3x8",
                     HORVAR == 4 ~ "Variable jour à lautre"),
  controle = case_when(CONTROLE == 1 ~ "Aucun contrôle",
                       CONTROLE %in% c(2,3) ~ "Contrôle par signature/carte",
                       CONTROLE == 4 ~ "Contrôle par encadrement",
                       CONTROLE == 5 ~ "Contrôle par autres collègues"),
  hsup = case_when(HSUP == 1 ~ "Dépassement tlj",
                   HSUP == 2 ~ "Dépassement souvent",
                   HSUP == 3 ~ "Dépassement parfois",
                   HSUP == 4 ~ "Dépassement jamais"),
  joindre = case_when(JOINDRE == 1 ~ "Être joint oui",
                      JOINDRE == 2 ~ "Être joint non"),
  # congetr_acm = case_when(congetr == 1 ~ "NA congé",
  #                         congetr == 2 ~ "Aucune congé",
  #                         congetr == 3 ~ "Moins de 5 sem congés",
  #                         congetr == 4 ~ "5 sem congés",
  #                         congetr == 5 ~ "6 ou 7 sem congés",
  #                         congetr == 6 ~ "8 ou 9 sem congés",
  #                         congetr == 7 ~ "Plus de 9 sem congés"),
  # priscong = case_when(PRISCONG == 1 ~ "Prise toutes les congés",
  #                      PRISCONG == 2 ~ "Non mais report congés",
  #                      PRISCONG == 3 ~ "Non et congés perdues"),
  # temps_travail = abs(FINH - DEBUTH),
  # temps_fcat = factor(case_when(
  #   temps_travail <= 7 ~ "7h de travail ou moins",
  #   temps_travail == 8 ~ "8h de travail ",
  #   temps_travail %in% c(9,10,11) ~ "9-10-11h de travail",
  #   temps_travail == 12 ~ "12h de travail",
  #   temps_travail >= 13 ~ "13h de travail et plus")),
  typemploi = case_when(TYPEMPLOI %in% c(1,2,3,4,8) ~ "Autre type contrat",
                        TYPEMPLOI == 6 ~ "Contrat court type CDD",
                        TYPEMPLOI == 7 ~ "Contrat long type CDI"),
  nbnuit = case_when(NBNUIT == 0 | is.na(NBNUIT) ~ "Aucune nuit",
                     NBNUIT == 1 ~ "1 à 3 nuits",
                     NBNUIT == 2 ~ "4 à 11 nuits",
                     NBNUIT == 3 ~ "12 à 23 nuits",
                     NBNUIT == 4 ~ "24 à 49 nuits",
                     NBNUIT == 5 ~ "50 à 99 nuits",
                     NBNUIT == 6 ~ "100 à 199 nuits",
                     NBNUIT == 7 ~ "200 ou plus nuits"),
  horangt = case_when(HORANGT == 1 ~ "Modification horaire possible si imprev",
                      HORANGT == 2 ~ "Modification horaire impossible si imprev"),
  payecom = case_when(PAYECOM == 1 ~ "Très bien payé",
                      PAYECOM == 2 ~ "Bien payé",
                      PAYECOM == 3 ~ "Normalement payé",
                      PAYECOM == 4 ~ "Plutôt mal payé",
                      PAYECOM == 5 ~ "Très mal payé"),
  nbnuit_cat = case_when(NBNUIT %in% c(1,2,3,4) ~ "Moins d'une nuit par semaine en moyenne",
                         T ~ nbnuit),
  periode = case_when(PERIODE == 1 ~ "Journée travail en plusieurs périodes",
                      PERIODE == 2 ~ "Journée travail bloc"),
  ptmatin = case_when(PTMATIN == 1 ~ "Matin Habituellement",
                      PTMATIN == 2 ~ "Matin Occasionnellement",
                      PTMATIN == 3 ~ "Matin Jamais"),
  soir = case_when(SOIR == 1 ~ "Soir Habituellement",
                   SOIR == 2 ~ "Soir Occasionnellement",
                   SOIR == 3 ~ "Soir Jamais")
)


### (2.b) Regroupement de variables précedemment créées 

data <- data %>% mutate(
  profession_statut = case_when(
        PE == "344A" ~ "Médecins",
        PE == "344C" ~ "Internes",
        PE %in% c("431B","431C","431D","431F") ~ "Infirmiers",
        PE == "431A" ~ "Infirmiers cadres",
        PE == "525D" ~ "Agents de service hospitaliers",
        PE == "526A" ~ "Aide-soignants",
        PE == "431E" ~ "Sages-femmes",
        PE %in% c("344D","433D") ~ "Pharmaciens et préparateurs",
        PE %in% c("433A","526B","526C","526D", "433C","433B") ~ "Aides, techniciens et auxiliaires",
        PE %in% c("432D","432B") ~ "Kinésithérapeutes et assimilés",
        PE %in% c("311D","311C") ~ "Psychologues et dentistes",
        T ~ "Autres professions"),
  age_43 = case_when(
    AGE > 43 ~ "Plus de 43 ans",
    T ~ "43 ans ou moins")
)
