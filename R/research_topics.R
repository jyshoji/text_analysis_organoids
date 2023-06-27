### This R script file is for identifying miscellaneous research topics in organoid and 
### organ-on-chip publications, including research interests, studied disease, and used techniques.
###
### The file uses "./R_results/all_corpus" as an input.
###
### The file includes following steps: 
### 1. Identifying research topics to capture.
### 2. Preparing lists of research topics to capture
### 3. Capturing research topics.
### 4. Making a summary data frame of research topics and other research themes.
###
### The outcome of the codes are saved as:
### "./R_results/research_topics_F"
### which were used in TF_tile_graph.R to draw tile graphs.



### Loading a package
library(tidyverse)

### Setting the path to the root folder.
root_path <- "~/Research_data/Hybrida/final_analysis/"
### Change the above according to your root folder location.

### If you use Windows, change all occurrences of "/" in file paths to "\\".

### Loading the input file.
load(paste0(root_path, "R_results/all_corpus"))




##########
###
### 1. Identifying research topics to capture.
### 
### In short, frequently occurring phrases were manually checked, and important research topics were extracted from there.
### 
### See "./R/capturing_topics.R" for the code.
###
##########






##########
###
### 2. Preparing lists of research topics to capture
###
########## 

### Loading a csv file of terms that are to be considered for organ development when appearing before "development".
pre_dev_inc <- read.csv(paste0(root_path, "csv/pre_dev_F.csv")) %>% 
  filter(include == "y")

### Loading a csv file of terms that are to be considered as tumor.
tumor_inc <- read.csv(paste0(root_path, "csv/tumor_detected_F.csv")) %>% 
  filter(include == "y")

### Selecting tumor-related terms ending with "oma" or "omas" from the above terms.
oma_captured <- tumor_inc %>% 
  filter(grepl("omas?\\b", tumor)) %>% 
  mutate(tumor = gsub("omas\\b", "oma", tumor)) %>% 
  arrange(.$tumor) 

### Making a vector containing terms that are to be considered as multi-component organoids 
### when appearing between multi/multiple and "organoid".
multi_w = c("lineage", "tissue", "layered", "layer", "region", "system", "organ", 
            "lobed", "cell type", "compartment")



### Making a nested list of terms for research topics.
research_topics_w <- list(
  ### Research interests
  organ_development = c("organogenes[ei]s", "embryogenes[ei]s", "neurogenes[ei]s", "morphogenes[ei]s", "nephrogenesis", 
                        "corticogenesis", "vasculogenesis", "neurodevelopments?", 
                        paste0(pre_dev_inc$pre_dev, "[- ]developments?", collapse = "|")),
  tumor = tumor_inc$tumor, 
  disease = c("diseases?", "syndromes?", "disorders?", "infections?", "injury", "injuries", "dystrophy", "dystrophies"),
  drug_development = c("drug developments?", "drug screenings?", "drug discovery", "drug discoveries", 
                       "developments? of [a-z]*[- ]?[a-z]* ?drugs?", "discovery of [a-z]*[- ]?[a-z]* ?drugs?", 
                       "discoveries of [a-z]*[- ]?[a-z]* ?drugs?", "screenings? of [a-z]*[- ]?[a-z]* ?drugs?", 
                       "drug testing", "testing of [a-z]*[- ]?[a-z]* ?drugs?"),
  drug_delivery = c("drug delivery", "drug deliveries", "delivery of [a-z]*[- ]?[a-z]* ?drugs?", "deliveries of [a-z]*[- ]?[a-z]* ?drugs?"), 
  pharmacokinetics = c("pharmacokinetics?", "pharmacodynamics?", "adme", "drug metabolisms?", "drug absorptions?"), 
  pharmacogenomics = c("pharmacogenomics?", "pharmacogenetics?"), 
  toxicology = c("[a-z]*toxicology", "[a-z]*toxicological"),
  electrophysiology = c("electro[- ]?physiology", "electro[- ]?physiological"), 
  mechanobiology = c("mechano[- ]?biology", "mechano[- ]?transductions?", "mechano[- ]?sensations?", "mechano[- ]?sensing"), 
  high_throughput_screening = c("high[- ]throughput [a-z]* ?screenings?", "high[- ]throughput [a-z]* ?screens?", 
                                "high[- ]throughput [a-z]* ?platforms?"), 
  ethics = c("ethics?", "ethical", "neuroethics?", "neuroethical", "bioethics?", "bioethical", "metabioethics?", "metabioethical"), 
  
  ## Clinical interests
  biobanking = c("biobanks?", "biobanking"), 
  clinical_study = c("clinical [a-z]* ?trials?", "clinical [a-z]* ?study", "clinical [a-z]* ?studies"), 
  preclinical_model = c("pre[- ]?clinical [a-z]* ?models?", "pre[- ]?clinical [a-z]* ?study", 
                        "pre[- ]?clinical [a-z]* ?studies", "disease models?", "disease modell?ing", 
                        "tumou?r models?", "tumou?r modell?ing", "cancer models?", "cancer modell?ing"), 
  precision_medicine = c("precision [a-z]* ?medicines?", "personali[sz]ed [a-z]* ?medicines?", 
                         "precision [a-z]* ?therapy", "precision [a-z]* ?therapies", 
                         "personali[sz]ed [a-z]* ?therapy", "personali[sz]ed [a-z]* ?therapies", 
                         "precision [a-z]* ?treatments?", "personali[sz]ed [a-z]* ?treatments?", 
                         "precision oncology", "personali[sz]ed oncology"), 
  homeostasis = c("homeostas[ei]s"), 
  regeneration = c("regenerations?", "regenerative medicines?", "regenerative therapy", 
                   "regenerative therapies", "tissue renewals?", "tissue repairs?", 
                   "cell renewals?"), 
  transplantation = c("transplantations?"), 
  cell_therapy = c("cell therapy", "cell therapies", "cellular therapy", "cellular therapies"), 
  drug_therapy = c("drug therapy", "drug therapies", "drug treatments?"), 
  gene_therapy = c("gene therapy", "gene therapies"), 
  photodynamic_therapy = c("photodynamic therapy", "photodynamic therapies"), 
  photothermal_therapy = c("photothermal therapy", "photothermal therapies"), 
  replacement_therapy = c("replacement therapy", "replacement therapies"), 
  targeted_therapy = c("targeted therapy", "targeted therapies"),
  
  ## Patient-derived material
  patient_derived = c("patient[- ]derived"), 
  
  ### Diseases
  
  ### Viral disease
  coronavirus = c("coronavirus", "coronaviruses", "coronaviral", "sars[- ]cov[- ]2", "covid[- ]19", 
                  "severe acute respiratory syndromes?", "porcine epidemic diarrhea virus", 
                  "porcine epidemic diarrhea viruses", "porcine deltacoronavirus", "porcine deltacoronaviruses"), 
  astrovirus = c("astrovirus", "astroviruses", "astroviral"), 
  enterovirus = c("enterovirus", "enteroviruses", "enteroviral"), 
  flavivirus = c("flavivirus", "flaviviruses", "flaviviral"),  
  norovirus = c("norovirus", "noroviruses", "noroviral"), 
  lentivirus = c("lentivirus", "lentiviruses", "lentiviral"),  
  rotavirus = c("rotavirus", "rotaviruses", "rotaviral"), 
  Zika_virus = c("zika", "zikv"), 
  dengue = c("dengue"), 
  influenza = c("influenza"), 
  HIV = c("hivs?"), 
  HPV = c("human papillomavirus", "hpvs?"), 
  
  ## neural diseases
  ADHD = c("adhd", "attention deficit[ /]hyperactivity disorders?"), 
  epilepsy = c("epilepsy", "epilepsies", "epileptic"), 
  guillain_barre_syndrome = c("guillain[- ]barre? syndromes?"), 
  hypertonia = c("hypertonia"), 
  leigh_syndrome = c("leigh syndromes?", "leigh diseases?", "subacute necrotizing encephalomyelopathy"), 
  traumatic_brain_injury = c("traumatic brain injury", "traumatic brain injuries", "intracranial injury", "intracranial injuries"), 
  
  ## neurodegenerative disorder
  neurodegenerative_disorder = c("neurodegenerative diseases?", "neurodegenerative disorders?", "dementia"), 
  alzheimers_disease = c("alzheimer'?s diseases?"), 
  huntingtons_disease = c("huntington'?s diseases?"), 
  parkinsons_disease = c("parkinson'?s diseases?"), 
  motor_neuron_disease = c("motor neurone? diseases?", "amyotrophic lateral sclerosis", "amyotrophic lateral sclerotic", 
                           "lou gehrig'?s? diseases?"), 
  
  ## mental_disorder
  mental_disorder = c("neuropsychiatric disorders?", "psychiatric disorders?", "mental disorders?", "neuropsychiatric diseases?", 
                      "mental health disorders?", "mental illness", "mental diseases?", "psychological disorders?"), 
  attentione_deficit = c("attention deficits?"), 
  autism_spectrum = c("autism spectrum"), 
  bipolar_disorders = c("bipolar disorders?", "bipolar diseases?"), 
  schizophrenia = c("schizophrenia"), 
  
  ## eye disease
  amaurosis = c("amaurosis"), 
  glaucoma = c("glaucoma"), 
  retinal_degenerative_disease = c("retinal degenerative diseases?", "retinal degenerations?"), 
  macular_degeneration = c("macular degenerations?"), 
  retinal_dystrophy = c("retinal dystrophy", "retinal dystrophies"), 
  retinitis_pigmentosa = c("retinitis pigmentosa"), 
  stargardt_disease = c("stargardt diseases?"), 
  
  ## respiratory disease 
  acute_respiratory_distress_syndrome = c("acute respiratory distress syndromes?"), 
  chronic_obstructive_pulmonary_disease = c("chronic obstructive pulmonary diseases?", "chronic obstructive lung diseases?", 
                                            "chronic obstructive airway diseases?"), 
  pulmonary_hypertension = c("pulmonary hypertensions?", "pulmonary[- ]arterial hypertensions?"), 
  pulmonary_vascular_disease = c("pulmonary vascular diseases?"), 
  respiratory_distress = c("respiratory distress"), 
  emphysema = c("emphysema"), 
  asthma = c("asthma"), 
  
  ## cardiovascular disease
  aneurysm = c("aneurysms?", "microaneurysms?"), 
  arrhythmia = c("arrhythmias?", "dysrhythmias?"), 
  atherosclerosis = c("atherosclerosis", "atherosclerotic"), 
  congenital_heart_disease = c("congenital heart diseases?", "congenital heart disorders?", "congenital heart anomaly", "single ventricle"), 
  single_ventricle = c("single ventricle"), 
  endocarditis = c("endocarditis"), 
  hemolytic_uremic_syndrome = c("ha?emolytic[- ]uremic syndromes?"), 
  hypoplastic_left_heart_syndrome = c("hypoplastic left heart", "cyanotic heart diseases?"), 
  Long_QT_syndrome = c("long qt syndromes?"), 
  sickle_cell_disease = c("sickle cell diseases?", "sickle cell disorders?", "sickle cell anaemias?"), 
  thrombosis = c("thrombos[ei]s"), 
  extravasation = c("extravasations?"), 
  heart_failure = c("heart failure", "cardiac failure"), 
  
  ## gastrointestinal disease
  gastroesophageal_reflux_disease = c("gastro-?o?esophageal reflux diseases?"), 
  gastroenteritis = c("gastroenteriti[cs]"), 
  gastritis = c("gastriti[cs]"), 
  enterocolitis = c("enterocoliti[cs]"), 
  enteritis = c("enteriti[cs]"), 
  colitis = c("coliti[cs]"), 
  inflammatory_bowel_disease = c("inflammatory bowel diseases?"), 
  ulcerative_colitis = c("ulcerative coliti[cs]"), 
  crohns_disease = c("crohn'?s diseases?"), 
  coeliac_disease = c("co?eliac diseases?"), 
  microvillous_inclusion_disease = c("microvillo?us inclusion diseases?"), 
  mucositis = c("mucosit[ei]s"), 
  irritable_bowel_syndrome = c("irritable bowel syndromes?"), 
  short_bowel_syndrome = c("short bowel syndromes?"), 
  diarrhea = c("diarrho?eal?"), 
  barretts_esophagus = c("barrett'?s esophagus"), 
  cholestasis = c("cholestasis"), 
  
  ## hpb disease
  diabetes = c("diabetes", "diabetic"), 
  cirrhosis = c("cirrhosis", "end[- ]stage liver diseases?", "chronic liver failures?"), 
  hepatotoxicity = c("drug[- ]induced liver injury", "drug[- ]induced liver injuries", "hepatotoxicity", "drug[- ]induced liver diseases?"), 
  fatty_liver_disease = c("fatty liver diseases?", "steatohepatitis", "hepatosteatosis", "hepatic steatosis"), 
  hepatitis = c("hepatitis"), 
  acute_liver_failure = c("acute liver failures?"), 
  cholangitis = c("cholangitis"), 
  
  ## kidney disease
  chronic_kidney_disease = c("chronic kidney diseases?", "chronic renal diseases?"), 
  end_stage_renal_disease = c("end[- ]stage kidney diseases?", "end[- ]stage renal diseases?", "kidney failures?", "renal failures?"), 
  nephrotoxicity = c("nephrotoxicity", "drug[- ]induced kidney injury", "drug[- ]induced kidney injuries", 
                     "drug[- ]induced renal injury", "drug[- ]induced renal injuries"), 
  polycystic_kidney_disease = c("polycystic kidney diseases?"), 
  acute_kidney_injury = c("acute kidney injury", "acute kidney injuries", "acute renal failure"), 
  
  ## Musculoskeletal diseases
  arthritis = c("[a-z]*arthrit[ei]s"), 
  osteoarthritis = c("osteoarthrit[ei]s"),   
  muscular_dystrophy = c("muscular dystrophy", "muscular dystrophies"), 
  
  ## fibrosis
  fibrosis = c("fibrosis", "fibrotic"), 
  cystic_fibrosis = c("cystic fibrosis"), 
  idiopathic_pulmonary_fibrosis = c("idiopathic pulmonary fibrosis"), 
  
  ## genetic disorder
  alagille_syndrome = c("alagille syndromes?", "alagille[- ]watson syndromes?", "hepatic ductular hypoplasia"), 
  alport_syndrome = c("alport syndromes?"), 
  charcot_marie_tooth = c("charcot-marie-tooth", "peroneal muscular atrophy", "Dejerine-sottas syndromes?"), 
  ciliopathy = c("ciliopathy"), 
  creatine_transporter_deficiency = c("creatine transporter deficiency", "creatine transporter deficiencies", "creatine transporter defects?"), 
  dravet_syndrome = c("dravet syndromes?", "severe myoclonic epilepsy of infancy"), 
  Friedreichs_ataxia = c("friedreich's ataxia"), 
  fragile_X_syndrome = c("fragile x syndromes?"), 
  hermansky_pudlak_syndrome = c("hermansky[- ]pudlak syndromes?"), 
  hirschsprung_disease = c("hirschsprung'?s? diseases?"), 
  lynch_syndrome = c("lynch syndromes?", "hereditary nonpolyposis colorectal cancers?"), 
  lysosomal_storage_disease = c("lysosomal storage diseases?"), 
  microcephaly = c("microcephaly"), 
  niemann_pick_disease = c("niemann[- ]pick diseases?"), 
  Nijmegen_breakage_syndrome = c("nijmegen breakage syndromes?"), 
  primary_ciliary_dyskinesia = c("primary ciliary dyskinesia", "immotile ciliary syndromes?", "kartagener syndromes?"), 
  progeria = c("progeria"), 
  down_syndrome = c("down'?s? syndromes?"),   
  rett_syndrome = c("rett syndromes?"), 
  usher_syndrome = c("usher syndromes?", "hallgren syndromes?", "dysacusis syndromes?"), 
  wilson_disease = c("wilson'?s? diseases?"), 
  
  ## other disease
  autoimmune_disease = c("autoimmune diseases?", "autoimmune disorders?"), 
  atresia = c("atresia"), 
  callus = c("callus", "calluses"), 
  dystrophy = c("dystrophy", "dystrophies"), 
  endometriosis = c("endometriosis"), 
  graft_versus_host_disease = c("graft[- ]versus[- ]host diseases?"), 
  helminthiasis = c("helminthias[ei]s", "helminthosis", "helminth infections?", "worm infections?"), 
  hyperplasia = c("hyperplasia", "hypergenesis"), 
  hypoxia = c("hypoxia"), 
  ischemia = c("ischa?emi[ac]"),   
  reperfusion_injury = c("reperfusion injury", "reperfusion injuries", "reoxygenation injury", "reoxygenation injuries"), 
  injury = c("injury", "injuries"), 
  radiation_injury = c("radiation injury", "radiation injuries", "radiation[- ]induced [a-z]* ?injury", "radiation[- ]induced [a-z]* ?injuries", 
                       "radiation syndromes?", "radiation[- ]induced [a-z]* ?syndromes?"), 
  refractory_disease = c("refractory diseases?"), 
  metaplasia = c("metaplasia"), 
  obesity = c("obesity"), 
  polyposis = c("polyposis"), 
  pre_eclampsia = c("pre[- ]?eclampsia"), 
  prion = c("prions?"),   
  psoriasis = c("psoriasis"), 
  sclerosis = c("[a-z]*sclerosis", "[a-z]*sclerotic"), 
  thyroid_associated_orbitopathy = c("thyroid[- ]associated orbitopathy"), 
  ulcer = c("ulcers?"), 
  sepsis = c("sepsis"), 
  
  ### Microbiology
  host_microbe_interactions = c("host[- ]pathogen interactions?", "host[- ]microbe interactions?", "host[- ]microbiome interactions?", 
                                "host[- ]parasite interactions?", "gut[- ]microb[a-z]*", "intestinal[- ]microb[a-z]*", "microbiota", 
                                "microbiomes?", "dysbiosis"), 
  probiotics = c("probiotics?"),   
  Chlamydia = c("chlamydias?"),  
  Citrobacter = c("citrobacter"), 
  Escherichia_coli = c("escherichia coli", "e\\. coli"), 
  lactobacillus = c("lactobacillus", "lactobacilli"), 
  Helicobacter_pylori = c("helicobacter pylori", "h\\. pylori"), 
  Clostridioides_difficile = c("clostridioides difficile", "c\\. difficile"), 
  Salmonella = c("salmonella"), 
  Staphylococcus = c("staphylococcus"), 
  tuberculosis = c("tuberculosis", "phthisis"), 
  Cryptosporidium = c("cryptosporidium"), 
  malaria = c("malaria"), 
  nematode = c("nematodes?", "helminth"), 
  Toxoplasma = c("toxoplasma"), 
  
  ## tumor words
  cancer = c("cancers?", "cancerous"), 
  neoplasm = c("neoplastic", "neoplasms?"), 
  leukemia = c("leuka?emias?"), 
  
  ## tumor physiology
  metastasis = c("metastas[ei]s", "metastatic"), 
  circulating_tumor_cell = c("circulating tumor cells?"),   
  minimal_residual_disease = c("minimal residual diseases?"), 
  tumor_heterogeneity = c("tumou?r heterogeneity", "cancer heterogeneity"), 
  tumor_microenvironment = c("tumou?r microenvironments?", "cancer microenvironments?"),   
  tumor_vessel = c("tumou?r vessels?"), 
  
  ## cellular processes
  apoptosis = c("apoptosis", "apoptotic"), 
  ferroptosis = c("ferroptosis", "ferroptotic"), 
  necroptosis = c("necroptosis", "necroptotic"), 
  autophagy = c("autophagy", "autophagic"), 
  ER_stress = c("endoplasmic reticulum stress", "er stress", "unfolded protein responses?"), 
  reactive_oxigen_species = c("reactive oxygen species", "ros", "oxidative stress"), 
  redox = c("redox"), 
  
  ## physioloigy
  barrier_function = c("barrier functions?"), 
  epithelial_barrier = c("epithelial barriers?"), 
  epithelial_mesenchymal_transition = c("epithelial[- ]mesenchymal transitions?"),   
  stem_cell_niche = c("stem[- ]cell niches?"),   
  chemotaxis = c("chemotaxis"), 
  crypt_villus_axis = c("crypt[- ]villus ax[ei]s"), 
  gut_brain_axis = c("gut[- ]brain[- ]ax[ei]s"), 
  gut_liver_axis = c("gut[- ]liver[- ]ax[ei]s"), 
  innate_immune_response = c("innate immune responses?", "innate immune systems?"), 
  quiescence = c("quiescence"), 
  senescence = c("senescence", "age?ing"), 
  
  ## organoid-generating techniques
  ThreeD_printing = c("3d)?[- ]printings?", "three[- ]dimensional[- ]printings?", "3d)?[- ]printed", "three[- ]dimensional[- ]printed"), 
  ThreeD_bioprinting = c("3d)?[- ]bioprintings?", "three[- ]dimensional[- ]bioprintings?", "3d)?[- ]bioprinted", 
                         "three[- ]dimensional[- ]bioprinted", "3d)? cell[- ]printings?", "three[- ]dimensional cell[- ]printings?", 
                         "3d)? cell[- ]printed", "three[- ]dimensional cell[- ]printed", "bioinks?"), 
  self_organization = c("self[- ]organi[sz]ed", "self[- ]organization"), 
  grafted_organoid = c("grafted[a-z0-9 ]+organoids?", "xenografted[a-z0-9 ]+organoids?", "transplanted[a-z0-9 ]+organoids?", 
                       "organoid[a-z0-9 ]+grafted", "organoid[a-z0-9 ]+transplanted"),   
  vascularization = c("vasculari[sz]ations?", "vasculari[sz]ed"), 
  immune_cells = c("immune cells?", "macrophages?"), 
  immune_cells_key = c("immune cells?"), 
  lymphatic = c("lymphoid", "lymph[- ]vessels?", "lymphatic[- ]vessels?", "lymphatic[- ]vasculatures?"), 
  primary_lymphoid_organs = c("thymus", "thymuses", "thymic?", "bone marrows?"), 
  secondary_lymphoid_organs = c("spleens?", "germinal centers?", "tonsils?", "tonsillar", "lymph[- ]nodes?", "lymph[- ]glands?", "lymph[- ]glandular"), 
  macrophages = c("macrophages?"), 
  resident_immune_cells = c("resident immune cells?", "kupffer cells?", "micro glia"), 
  assembloids = c("assembloids?"), 
  multi_structure_organoid = c(paste0("multi-?", multi_w, "[- ]?[a-z]*[- ]organoids?"), 
                               paste0("multiple ", multi_w, "[- ]?[a-z]*[- ]organoids?")), 
  gene_editing = c("genome[- ]editing", "crispr", "gene[- ]editing"), 
  tissue_engineering = c("tissue[- ]engineering", "tissue[- ]engineered"), 
  cardiac_tissue_engineering = c("cardiac tissue[- ]engineering"), 
  engineered_heart_tissue = c("engineered heart tissues?"), 
  microelectrode_array = c("micro[- ]?electrode arrays?"), 
  organoid = c("organoids?", "enteroids?", "gastruloids?", "colonoids?", "blastoids?", "tumou?roids?", 
               "assembloids?", "embryoids?", "cerebroids?", "cardioids?"), 
  organ_on_chip = c("onchip", "oocs?", "micro[- ]?physiological systems?", 
                    "micro[- ]?physiology systems?", "micro[- ]?physiologic systems?"), 
  
  ## Analytical techniques
  genomics = c("genomics?", "genome sequencing"), 
  epigenetics = c("epigenetics?", "epigenomics?", "epigenomes?", "dna[- ]methylations?"), 
  exome_sequencing = c("exome sequencing"), 
  transcriptomics = c("transcriptomics?", "transcriptomes?"), 
  sc_RNA_seq = c("single[- ]cell m?rna sequencing", "single[- ]cell m?rna seq", 
                 "single[- ]cell transcriptomics?", "single[- ]cell transcriptomes?", "scrna seq"),   
  proteomics = c("proteomics?", "proteomes?", "phosphoproteomics?", "phosphoproteomes?"), 
  metabolomics = c("metabolomics?", "metabolomes?"), 
  next_generation_sequencing = c("next[- ]generation sequencing", "ngs"), 
  immunohistochemistry = c("immuno[- ]?histochemistry", "immuno[- ]?histochemical", "immuno[- ]?fluorescence", "immuno[- ]?fluorescent", 
                           "immuno[- ]?stainings?", "immuno[- ]cytochemistry", "immuno[- ]cytochemical"), 
  cell_lineage = c("lineage tracing", "cell lineages?"), 
  whole_mount_techniques = c("whole[- ]mount"), 
  TEER = c("transendothelial electrical resistance", "transepithelial electrical resistance", "teer"), 
  acoustofluidics = c("acoustofluidics?"), 
  optogenetics = c("optogenetics?"), 
  high_content_analysis = c("high[- ]content screenings?", "high[- ]content analys[ei]s", "high[- ]content imagings?", "cellomics?"), 
  machine_learning = c("machine[- ]learning", "deep[- ]learning"), 
  
  ## Imaging techniques
  expansion_microscopy = c("expansion microscopy"), 
  intravital_microscopy = c("intravital microscopy"), 
  optical_coherence_tomography = c("optical coherence tomography"), 
  quantitative_phase_imaging = c("quantitative[- ]phase[- ]imaging", "quantitative[- ]phase[- ]contrast[- ]microscopy", "quantitative[- ]phase[- ]microscopy"), 
  Raman_spectroscopy = c("raman")
) %>% 
  lapply(., function(x) paste0("\\b", x, "\\b", collapse = "|"))


### Making another nested list for terms that need case-sensitive searches.
CS_topics <- list(
  mass_spectrometry = c("[Mm]ass[- ][Ss]pectrometry", "MS", "MALDI-MSI"), 
  MS_imaging = c("[Mm]ass[- ][Ss]pectrometric[- ][Ii]magings?", "[Mm]ass[- ][Ss]pectrometry[- ][Ii]magings?", 
                 "MS[- ][Ii]magings?", "MALDI-MSI"), 
  atomic_force_microscopy = c("[Aa]tomic [Ff]orce [Mm]icroscopy", "AFM"), 
  fluorescence_lifetime_imaging_microscopy = c("[Ff]luorescence[- ][Ll]ifetime [Ii]maging [Mm]icroscopy", "FLIM"), 
  light_sheet_microscopy = c("[Ll]ight[- ][Ss]heet [Mm]icroscopy", "[Ll]ight[- ][Ss]heet [Ff]luorescence [Mm]icroscopy", "LSFM"), 
  ### "MRI" is not captured at the moment as its use is mostly not directly on organoids/organ-on-a-chip.
  #MRI = c("[Mm]agnetic [Rr]esonance [Ii]maging", "MRI"), 
  transmission_electron_microscopy = c("[Tt]ransmission [Ee]lectron [Mm]icroscopy", "TEM"), 
  scanning_electron_microscopy = c("[Ss]canning [Ee]lectron [Mm]icroscopy", "SEM"), 
  FACS = c("[Ff]luorescence-[Aa]ctivated [Cc]ell [Ss]orting", "[Ff]luorescent-[Aa]ctivated [Cc]ell [Ss]orting, FACS"), 
  patient_derived_xenograft = c("[Pp]atient-[Dd]erived [Xx]enografts?", "PDXs?"), 
  patient_derived_organoid = c("[Pp]atient-[Dd]erived [Oo]rganoids?", "PDOs?"), 
  patient_derived_tumor_organoid = c("[Pp]atient-[Dd]erived [Tt]umou?r [Oo]rganoids?", "PDTOs?")
) %>% 
  lapply(., function(x) paste0("\\b", x, "\\b", collapse = "|"))







##########
###
### 3. Capturing research topics.
###
##########

### Capturing research topics.
pre_TF_purpose <- as.data.frame(sapply(research_topics_w, function(x) grepl(x, tolower(all_corpus$text_all_mod)))) %>% 
  ### "immune cells" is also captured in "key" sentences only, to see if it can mostly capture immune cells as components of organoids.
  mutate(immune_cells_key = grepl("immune cells?", tolower(all_corpus$text_all_key))) %>% 
  rename_with(., ~ gsub("^", "TF_", .))

### Capturing research topics that needs case-sensitive searches.
CS_TF <- as.data.frame(sapply(CS_topics, function(x) grepl(x, all_corpus$text_all_mod))) %>% 
  rename_with(., ~ gsub("^", "TF_", .))

### Capturing tumor terms ending with "oma".
tumor_TF <- as.data.frame(sapply(unique(oma_captured$tumor), 
                                 function(x) grepl(paste0("\\b", x, "s?\\b", collapse = "|"), tolower(all_corpus$text_all_mod)))) %>% 
  rename_with(., ~ gsub("^", "TF_", .))





##########
###
### 4. Making a summary data frame of research topics and other research themes.
###
##########

### Loading required files.
load(paste0(root_path, "R_results/organ_types_F"))
load(paste0(root_path, "R_results/countries_F"))
load(paste0(root_path, "R_results/organisms_F"))
load(paste0(root_path, "R_results/cell_types_F"))

### Combining data frames of the corpus, research topics, cell sources, organisms
TF_purpose <- all_corpus %>% 
  ### Adding research interests and then clinical interests  
  cbind(., pre_TF_purpose) %>% 
  ### Adding research organisms.
  cbind(., organisms_F %>% select(starts_with("TF_"))) %>%  
  ### Adding cell sources.
  cbind(., cell_types_F %>% select(starts_with("TF_"))) %>% 
  ### Adding case-sensi
  cbind(., CS_TF) %>% 
  cbind(., tumor_TF)
  
colnames(TF_purpose)

### Adjusting the logical columns.
### Basically, an upper category is changed to TRUE if any of the lower categories are TRUE.
TF_purpose2 <- TF_purpose %>% 
  mutate(TF_disease = ifelse(rowSums(.[, 55:189]) > 0, TRUE, TF_disease)) %>% 
  mutate(TF_host_microbe_interactions = as.logical(rowSums(.[, 190:204]))) %>% 
  mutate(TF_neurodegenerative_disorder = as.logical(rowSums(.[, 73:77]))) %>% 
  mutate(TF_mental_disorder = as.logical(rowSums(.[, 78:82]))) %>% 
  mutate(TF_retinal_dystrophy = ifelse(TF_retinitis_pigmentosa == TRUE, TRUE, TF_retinal_dystrophy)) %>% 
  mutate(TF_retinal_degenerative_disease = ifelse(TF_retinal_dystrophy == TRUE, TRUE, TF_retinal_degenerative_disease)) %>% 
  mutate(TF_inflammatory_bowel_disease = 
           ifelse(TF_ulcerative_colitis == TRUE | TF_crohns_disease == TRUE, TRUE, TF_inflammatory_bowel_disease)) %>% 
  mutate(TF_colitis = ifelse(TF_inflammatory_bowel_disease == TRUE, TRUE, TF_colitis)) %>% 
  mutate(TF_enterocolitis = ifelse(TF_enteritis == TRUE | TF_colitis == TRUE, TRUE, TF_enterocolitis), 
         TF_gastroenteritis = ifelse(TF_gastritis == TRUE | TF_enteritis == TRUE, TRUE, TF_gastroenteritis)) 

### Adding organ_type, major_organ, tumor_type, main_country, organism, cell_type columns.
research_topics_F <- TF_purpose2 %>% 
  mutate(organ_type = organ_types_F$organ_type, 
         major_organ = organ_types_F$major_organ, 
         tumor_type = organ_types_F$tumor_w, 
         prenatal_type = organ_types_F$prenatal, 
         blastoid_gastruloid = organ_types_F$blastoid_gastruloid, 
         main_country = countries_F$main_country, 
         organism = organisms_F$organism, 
         cell_type = cell_types_F$cell_type, 
         corpus_F = organ_types_F$corpus_F) 
  

### Saving the result
save(research_topics_F, file = paste0(root_path, "R_results/research_topics_F"))

### Loading an R data file that shows which publications are on organoids/OoCs, rather than on tumor organoids/ToCs.
load(paste0(root_path, "R_results/organ_types_P"))

### Removing text fields.
### Also removing tumor organoids and ToC corpora.
research_topics_P <- research_topics_F %>% 
  filter(ID %in% organ_types_P$ID) %>%    
  select(!c(7:25))

save(research_topics_P, file = paste0(root_path, "R_results/research_topics_P"))


##########
###
### Writing a summary csv file.
### 
########## 
load(paste0(root_path, "R_results/research_topics_F"))

category_names_F <- read.csv(paste0(root_path, "csv/category_names_F.csv")) 

selected_topics <- category_names_F %>% 
  filter(A_filter == "A")

selected_publications <- research_topics_F %>% 
  filter(rowSums(across(starts_with("TF_"))) > 0, 
         !major_organ == "unidentified", 
         type == "Research article", 
         corpus_F %in% c("organoid", "OoC")) %>% 
  select(author, year, title, doi, corpus_F, major_organ, any_of(selected_topics$category_colname)) %>% 
  mutate(across(starts_with("TF_"), ~ ifelse(. == FALSE, "", "X"))) %>% 
  rename(any_of(setNames(selected_topics$category_colname, selected_topics$category_renamed))) %>% 
  arrange(corpus_F, major_organ, year, author)

write.csv(selected_publications, file = paste0(root_path, "results/csv/research_topics.csv"), row.names = FALSE)