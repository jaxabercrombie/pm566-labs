## Question 1: How many sars-cov-2 papers?

    # Downloading the website
    website <- xml2::read_html("https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2")

    # Finding the counts
    counts <- xml2::xml_find_first(website, "/html/body/main/div[9]/div[2]/div[2]/div[1]/span")

    # Turning it into text
    counts <- as.character(counts)

    # Extracting the data using regex
    stringr::str_extract(counts, "[0-9,]+")

    ## [1] "115,799"

    stringr::str_extract(counts, "([[:digit:]]*),?([[:digit:]]+)")

    ## [1] "115,799"

### Question 2: Academic publications on COVID19 and Hawaii

    query_ids <- GET(
      url   = "https://eutils.ncbi.nlm.nih.gov/",
      path = "entrez/eutils/esearch.fcgi",
      query = list(db     = "pubmed",
                   term   = "covid19 hawaii",
                   retmax = 1000)
    )

    # Extracting the content of the response of GET
    ids <- httr::content(query_ids)
    ids

    ## {xml_document}
    ## <eSearchResult>
    ## [1] <Count>151</Count>
    ## [2] <RetMax>151</RetMax>
    ## [3] <RetStart>0</RetStart>
    ## [4] <IdList>\n  <Id>34621978</Id>\n  <Id>34562997</Id>\n  <Id>34559481</Id>\n ...
    ## [5] <TranslationSet>\n  <Translation>\n    <From>covid19</From>\n    <To>"cov ...
    ## [6] <TranslationStack>\n  <TermSet>\n    <Term>"covid-19"[MeSH Terms]</Term>\ ...
    ## [7] <QueryTranslation>("covid-19"[MeSH Terms] OR "covid-19"[All Fields] OR "c ...

### Question 3: Get details about the articles

    ids_list <- xml2::as_list(ids)
    ids_char <- as.character(ids)

    # Turn the result into a character vector
    ids <- as.character(ids)

    # Find all the ids 
    ids <- stringr::str_extract_all(ids, "<Id>[0-9]+</Id>")[[1]]

    # Remove all the leading and trailing <Id> </Id>. Make use of "|"
    ids <- stringr::str_remove_all(ids, "<Id>|</Id>")

    #
    publications <- GET(
      url   = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
      query = list(
        db = "pubmed",
        id = I(paste(ids, collapse = ",")),
        retmax = 1000,
        rettype = "abstract"
        )
    )

    # Turning the output into character vector
    publications <- httr::content(publications)
    publications_txt <- as.character(publications)

### Question 4: Distribution of universities, schools, and departments

    institution <- str_extract_all(
      tolower(publications_txt),
      "university\\s+of\\s+[[:alpha:]-]+|institute\\s+of\\s+[[:alpha:]-]+"
      )
    institution <- unlist(institution)
    table(institution)

    ## institution
    ##        institute of allergy         institute of allied 
    ##                           1                           3 
    ##   institute of biochemistry     institute of biomedical 
    ##                           1                           1 
    ##    institute of biomedicine   institute of chiropractic 
    ##                           1                           1 
    ##       institute of clinical      institute of education 
    ##                           6                           1 
    ##          institute of emory  institute of environmental 
    ##                           2                           3 
    ##   institute of epidemiology           institute of food 
    ##                           1                           1 
    ##       institute of genetics        institute of harvard 
    ##                           1                           2 
    ##         institute of health          institute of liver 
    ##                           5                           2 
    ##         institute of marine       institute of medicine 
    ##                           1                           3 
    ##    institute of montpellier            institute of new 
    ##                           1                           5 
    ##   institute of oceanography   institute of pharmacology 
    ##                           1                           2 
    ##     institute of psychiatry         institute of public 
    ##                           1                           2 
    ## institute of rehabilitation      institute of rheumatic 
    ##                           3                           2 
    ##      institute of singapore       institute of southern 
    ##                           1                           2 
    ##     institute of technology            institute of the 
    ##                           4                           1 
    ##       institute of tropical       university of alberta 
    ##                          15                           2 
    ##       university of applied       university of arizona 
    ##                           1                           5 
    ##      university of arkansas         university of basel 
    ##                           1                           8 
    ##         university of benin      university of botswana 
    ##                           1                           1 
    ##      university of bradford       university of bristol 
    ##                           1                           4 
    ##       university of british       university of calgary 
    ##                           4                           1 
    ##    university of california       university of chicago 
    ##                          65                          11 
    ##    university of cincinnati      university of colorado 
    ##                           9                           5 
    ##   university of connecticut    university of copenhagen 
    ##                           1                           1 
    ##       university of córdoba     university of education 
    ##                           1                           1 
    ##        university of exeter       university of florida 
    ##                           1                           5 
    ##       university of granada         university of haifa 
    ##                           2                           1 
    ##         university of hawai        university of hawaii 
    ##                          92                         180 
    ##  university of hawaii-manoa        university of health 
    ##                           2                           8 
    ##          university of hong      university of honolulu 
    ##                           1                           3 
    ##      university of illinois          university of iowa 
    ##                           1                           4 
    ##     university of jerusalem          university of juiz 
    ##                           1                           4 
    ##        university of kansas      university of kentucky 
    ##                           2                           1 
    ##      university of lausanne         university of leeds 
    ##                           1                           2 
    ##    university of louisville        university of malaya 
    ##                           1                           2 
    ##      university of maryland      university of medicine 
    ##                           9                           3 
    ##     university of melbourne         university of miami 
    ##                           1                           2 
    ##      university of michigan     university of minnesota 
    ##                           8                           4 
    ##        university of murcia      university of nebraska 
    ##                           1                           5 
    ##        university of nevada           university of new 
    ##                           1                           8 
    ##         university of north       university of ontario 
    ##                           2                           1 
    ##          university of oslo        university of ottawa 
    ##                           6                           1 
    ##        university of oxford         university of paris 
    ##                           9                           1 
    ##  university of pennsylvania    university of pittsburgh 
    ##                          47                          13 
    ##         university of porto        university of puerto 
    ##                           2                           2 
    ##           university of rio     university of rochester 
    ##                           1                           4 
    ##           university of sao       university of science 
    ##                           2                          13 
    ##     university of singapore         university of south 
    ##                           1                           4 
    ##      university of southern        university of sydney 
    ##                          22                           1 
    ##    university of technology         university of texas 
    ##                           3                           7 
    ##           university of the       university of toronto 
    ##                          17                           5 
    ##        university of toulon      university of tübingen 
    ##                           1                           3 
    ##          university of utah    university of washington 
    ##                           4                           6 
    ##     university of wisconsin 
    ##                           3

    schools_and_deps <- str_extract_all(
      tolower(publications_txt),
      "school\\s+of\\s+[[:alpha:]-]+|department\\s+of\\s+[[:alpha:]-]+"
      )
    table(schools_and_deps)

    ## schools_and_deps
    ##              department of ageing             department of anatomy 
    ##                                 1                                 2 
    ##          department of anesthesia       department of anesthesilogy 
    ##                                 2                                 1 
    ##      department of anesthesiology             department of applied 
    ##                                 6                                 3 
    ##        department of biochemistry             department of biology 
    ##                                 1                                11 
    ##         department of biosciences       department of biostatistics 
    ##                                 1                                15 
    ##              department of botany          department of cardiology 
    ##                                 1                                 1 
    ##      department of cardiovascular                department of cell 
    ##                                 1                                 4 
    ##           department of chemistry               department of civil 
    ##                                 2                                12 
    ##            department of clinical            department of commerce 
    ##                                10                                 1 
    ##       department of communication       department of communicology 
    ##                                 2                                 2 
    ##           department of community       department of computational 
    ##                                 3                                 1 
    ##            department of critical             department of defense 
    ##                                 4                                 1 
    ##         department of dermatology           department of economics 
    ##                                22                                 3 
    ##           department of education           department of emergency 
    ##                                 7                                 5 
    ##       department of environmental        department of epidemiology 
    ##                                 6                                18 
    ##        department of experimental              department of family 
    ##                                 1                                 6 
    ##             department of general             department of genetic 
    ##                                 3                                 1 
    ##           department of geography              department of health 
    ##                                 5                                50 
    ##          department of hematology          department of immunology 
    ##                                 3                                 1 
    ##          department of infectious         department of information 
    ##                                22                                 2 
    ##           department of intensive            department of internal 
    ##                                 3                                55 
    ##       department of international         department of kinesiology 
    ##                                 1                                 2 
    ##          department of laboratory         department of mathematics 
    ##                                 3                                 7 
    ##          department of mechanical             department of medical 
    ##                                 5                                 7 
    ##            department of medicine        department of microbiology 
    ##                               110                                 3 
    ##              department of native          department of nephrology 
    ##                                 2                                 5 
    ##        department of neurological           department of neurology 
    ##                                12                                 2 
    ##        department of neurosurgery             department of nursing 
    ##                                 1                                 1 
    ##           department of nutrition                  department of ob 
    ##                                 7                                 5 
    ##          department of obstetrics        department of occupational 
    ##                                18                                 2 
    ##          department of orthopedic department of otolaryngology-head 
    ##                                 5                                 4 
    ##          department of paediatric           department of pathology 
    ##                                 1                                11 
    ##           department of pediatric          department of pediatrics 
    ##                                 2                                26 
    ##      department of pharmaceutical        department of pharmacology 
    ##                                 1                                 2 
    ##            department of pharmacy            department of physical 
    ##                                 1                                 5 
    ##          department of physiology       department of physiotherapy 
    ##                                10                                 1 
    ##          department of population          department of preventive 
    ##                                 6                                13 
    ##          department of psychiatry          department of psychology 
    ##                                27                                 7 
    ##              department of public           department of pulmonary 
    ##                                10                                 1 
    ##        department of quantitative      department of rehabilitation 
    ##                                 8                                 6 
    ##            department of research        department of rheumatology 
    ##                                 1                                 7 
    ##             department of smoking              department of social 
    ##                                 8                                 1 
    ##           department of sociology              department of sports 
    ##                                 4                                 1 
    ##          department of statistics             department of surgery 
    ##                                 2                                13 
    ##             department of traffic       department of translational 
    ##                                 1                                 1 
    ##            department of tropical                department of twin 
    ##                                31                                 4 
    ##             department of urology            department of veterans 
    ##                                 1                                 8 
    ##          department of veterinary              school of biomedical 
    ##                                 2                                 3 
    ##                   school of brown               school of education 
    ##                                 2                                 2 
    ##              school of electronic            school of epidemiology 
    ##                                 1                                 6 
    ##                  school of health              school of immunology 
    ##                                 1                                 1 
    ##                    school of life                school of medicine 
    ##                                 1                               343 
    ##                 school of natural                 school of nursing 
    ##                                 1                                23 
    ##                   school of ocean                school of pharmacy 
    ##                                 1                                 1 
    ##                school of physical           school of physiotherapy 
    ##                                 6                                 1 
    ##              school of population                  school of public 
    ##                                 2                                64 
    ##                  school of social          school of transportation 
    ##                                11                                 1

### Question 5: Form a Database

    pub_char_list <- xml2::xml_children(publications)
    pub_char_list <- sapply(pub_char_list, as.character)

    abstracts <- str_extract(pub_char_list, "<Abstract>[[:print:][:space:]]+</Abstract>")
    abstracts <- str_remove_all(abstracts, "</?[[:alnum:]- =\"]+>")
    abstracts <- str_replace_all(abstracts, "[[:space:]]+", " ")

    titles <- str_extract(pub_char_list, "[YOUR REGULAR EXPRESSION]")
    titles <- str_remove_all(titles, "[CLEAN ALL THE HTML TAGS]")

    database <- data.frame(
      PubMedID = ids,
      Title = titles,
      Abstract = abstracts
    )
    knitr::kable(database[1:20, ], caption = "Some papers about COVID-19 and Hawaii")

<table>
<caption>Some papers about COVID-19 and Hawaii</caption>
<colgroup>
<col style="width: 0%" />
<col style="width: 0%" />
<col style="width: 99%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">PubMedID</th>
<th style="text-align: left;">Title</th>
<th style="text-align: left;">Abstract</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">34621978</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">Recent clinical trials are considering inclusion of more than just apolipoprotein E (APOE) ε4 genotype as a way of reducing variability in analysis of outcomes. Case-control data were used to compare the capacity of age, sex, and 58 Alzheimer’s disease (AD)-associated single nucleotide polymorphisms (SNPs) to predict AD status using several statistical models. Model performance was assessed with Brier scores and tenfold cross-validation. Genotype and sex × age estimates from the best performing model were combined with age and intercept estimates from the general population to develop a personalized genetic risk score, termed age, and sex-adjusted GenoRisk. The elastic net model that included age, age x sex interaction, allelic APOE terms, and 29 additional SNPs performed the best. This model explained an additional 19% of the heritable risk compared to APOE genotype alone and achieved an area under the curve of 0.747. GenoRisk could improve the risk assessment of individuals identified for prevention studies. © 2021 The Authors. Alzheimer’s &amp; Dementia: Diagnosis, Assessment &amp; Disease Monitoring published by Wiley Periodicals, LLC on behalf of Alzheimer’s Association.</td>
</tr>
<tr class="even">
<td style="text-align: left;">34562997</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">Given that the success of vaccines against coronavirus disease 2019 (COVID-19) relies on herd immunity, identifying patients at risk for vaccine hesitancy is imperative-particularly for those at high risk for severe COVID-19 (i.e., minorities and patients with neurological disorders). Among patients from a large neuroscience institute in Hawaii, vaccine hesitancy was investigated in relation to over 30 sociodemographic variables and medical comorbidities, via a telephone quality improvement survey conducted between 23 January 2021 and 13 February 2021. Vaccine willingness (n = 363) was 81.3%. Univariate analysis identified that the odds of vaccine acceptance reduced for patients who do not regard COVID-19 as a severe illness, are of younger age, have a lower Charlson Comorbidity Index, use illicit drugs, or carry Medicaid insurance. Multivariable logistic regression identified the best predictors of vaccine hesitancy to be: social media use to obtain COVID-19 information, concerns regarding vaccine safety, self-perception of a preexisting medical condition contraindicated with vaccination, not having received the annual influenza vaccine, having some high school education only, being a current smoker, and not having a prior cerebrovascular accident. Unique amongst males, a conservative political view strongly predicted vaccine hesitancy. Specifically for Asians, a higher body mass index, while for Native Hawaiians and other Pacific Islanders (NHPI), a positive depression screen, both reduced the odds of vaccine acceptance. Upon identifying the variables associated with vaccine hesitancy amongst patients with neurological disorders, our clinic is now able to efficiently provide ancillary COVID-19 education to sub-populations at risk for vaccine hesitancy. While our results may be limited to the sub-population of patients with neurological disorders, the findings nonetheless provide valuable insight to understanding vaccine hesitancy.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">34559481</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">34545941</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">The Hispanic/Latino population is the second largest racial/ethnic group in the continental United States and Hawaii, accounting for 18% (60.6 million) of the total population. An additional 3 million Hispanic Americans live in Puerto Rico. Every 3 years, the American Cancer Society reports on cancer occurrence, risk factors, and screening for Hispanic individuals in the United States using the most recent population-based data. An estimated 176,600 new cancer cases and 46,500 cancer deaths will occur among Hispanic individuals in the continental United States and Hawaii in 2021. Compared to non-Hispanic Whites (NHWs), Hispanic men and women had 25%-30% lower incidence (2014-2018) and mortality (2015-2019) rates for all cancers combined and lower rates for the most common cancers, although this gap is diminishing. For example, the colorectal cancer (CRC) incidence rate ratio for Hispanic compared with NHW individuals narrowed from 0.75 (95% CI, 0.73-0.78) in 1995 to 0.91 (95% CI, 0.89-0.93) in 2018, reflecting delayed declines in CRC rates among Hispanic individuals in part because of slower uptake of screening. In contrast, Hispanic individuals have higher rates of infection-related cancers, including approximately two-fold higher incidence of liver and stomach cancer. Cervical cancer incidence is 32% higher among Hispanic women in the continental US and Hawaii and 78% higher among women in Puerto Rico compared to NHW women, yet is largely preventable through screening. Less access to care may be similarly reflected in the low prevalence of localized-stage breast cancer among Hispanic women, 59% versus 67% among NHW women. Evidence-based strategies for decreasing the cancer burden among the Hispanic population include the use of culturally appropriate lay health advisors and patient navigators and targeted, community-based intervention programs to facilitate access to screening and promote healthy behaviors. In addition, the impact of the COVID-19 pandemic on cancer trends and disparities in the Hispanic population should be closely monitored. © 2021 The Authors. CA: A Cancer Journal for Clinicians published by Wiley Periodicals LLC on behalf of American Cancer Society.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">34536350</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">34532685</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">34529634</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">Native Hawaiian and Pacific Islander populations have been disproportionately affected by COVID-19 (1-3). Native Hawaiian, Pacific Islander, and Asian populations vary in language; cultural practices; and social, economic, and environmental experiences,† which can affect health outcomes (4).§ However, data from these populations are often aggregated in analyses. Although data aggregation is often used as an approach to increase sample size and statistical power when analyzing data from smaller population groups, it can limit the understanding of disparities among diverse Native Hawaiian, Pacific Islander, and Asian subpopulations¶ (4-7). To assess disparities in COVID-19 outcomes among Native Hawaiian, Pacific Islander, and Asian populations, a disaggregated, descriptive analysis, informed by recommendations from these communities,** was performed using race data from 21,005 COVID-19 cases and 449 COVID-19-associated deaths reported to the Hawaii State Department of Health (HDOH) during March 1, 2020-February 28, 2021.†† In Hawaii, COVID-19 incidence and mortality rates per 100,000 population were 1,477 and 32, respectively during this period. In analyses with race categories that were not mutually exclusive, including persons of one race alone or in combination with one or more races, Pacific Islander persons, who account for 5% of Hawaii’s population, represented 22% of COVID-19 cases and deaths (COVID-19 incidence of 7,070 and mortality rate of 150). Native Hawaiian persons experienced an incidence of 1,181 and a mortality rate of 15. Among subcategories of Asian populations, the highest incidences were experienced by Filipino persons (1,247) and Vietnamese persons (1,200). Disaggregating Native Hawaiian, Pacific Islander, and Asian race data can aid in identifying racial disparities among specific subpopulations and highlights the importance of partnering with communities to develop culturally responsive outreach teams§§ and tailored public health interventions and vaccination campaigns to more effectively address health disparities.</td>
</tr>
<tr class="even">
<td style="text-align: left;">34499878</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">Following the publication of 2014 consensus statement regarding mass critical care during public health emergencies, much has been learned about surge responses and the care of overwhelming numbers of patients during the COVID-19 pandemic.1 Gaps in prior pandemic planning were identified and require modification in the midst of ongoing surge throughout the world. The Task Force for Mass Critical Care (TFMCC) adopted a modified version of established rapid guideline methodologies from the World Health Organization2 and the Guidelines International Network-McMaster Guideline Development Checklist.3 With a consensus development process incorporating expert opinion to define important questions and extract evidence, TFMCC developed relevant pandemic surge suggestions in a structured manner, incorporating peer-reviewed literature, “gray” evidence from lay media sources, and anecdotal experiential evidence. Ten suggestions were identified regarding staffing, load-balancing, communication, and technology. Staffing models are suggested with resilience strategies to support critical care staff. Intensive care unit (ICU) surge strategies and strain indicators are suggested to enhance ICU prioritization tactics to maintain contingency level care and avoid crisis triage, with early transfer strategies to further load-balance care. We suggest intensivists and hospitalists be engaged with the incident command structure to ensure two-way communication, situational awareness, and the use of technology to support critical care delivery and families of patients in intensive care units (ICUs). A subcommittee from the Task Force for Mass Critical Care offers interim evidence-informed operational strategies to assist hospitals and communities to plan for and respond to surge capacity demands from COVID-19. Copyright © 2021. Published by Elsevier Inc.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">34491990</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">Accurate estimates of infection prevalence and seroprevalence are essential for evaluating and informing public health responses and vaccination coverage needed to address the ongoing spread of COVID-19 in each United States (U.S.) state. However, reliable, timely data based on representative population sampling are unavailable, and reported case and test positivity rates are highly biased. A simple data-driven Bayesian semi-empirical modeling framework was developed and used to evaluate state-level prevalence and seroprevalence of COVID-19 using daily reported cases and test positivity ratios. The model was calibrated to and validated using published state-wide seroprevalence data, and further compared against two independent data-driven mathematical models. The prevalence of undiagnosed COVID-19 infections is found to be well-approximated by a geometrically weighted average of the positivity rate and the reported case rate. Our model accurately fits state-level seroprevalence data from across the U.S. Prevalence estimates of our semi-empirical model compare favorably to those from two data-driven epidemiological models. As of December 31, 2020, we estimate nation-wide a prevalence of 1.4% [Credible Interval (CrI): 1.0%-1.9%] and a seroprevalence of 13.2% [CrI: 12.3%-14.2%], with state-level prevalence ranging from 0.2% [CrI: 0.1%-0.3%] in Hawaii to 2.8% [CrI: 1.8%-4.1%] in Tennessee, and seroprevalence from 1.5% [CrI: 1.2%-2.0%] in Vermont to 23% [CrI: 20%-28%] in New York. Cumulatively, reported cases correspond to only one third of actual infections. The use of this simple and easy-to-communicate approach to estimating COVID-19 prevalence and seroprevalence will improve the ability to make public health decisions that effectively respond to the ongoing COVID-19 pandemic.</td>
</tr>
<tr class="even">
<td style="text-align: left;">34481278</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">During the COVID-19 pandemic, the prevalence of psychological distress rose from 11% in 2019 to more than 40% in 2020. This study aims to examine the disparities among US adult men and women. We used 21 waves of cross-sectional data from the Household Pulse Survey that were collected between April and December 2020 for the study. The Household Pulse Survey was developed by the U.S. Census Bureau to document the social and economic impact of COVID-19. The study population included four groups of adults: emerging adults (18-24 years); young adults (25-44 years); middle-aged adults (45-64 years); and older adults (65-88 years). Psychological distress was measured by their Generalized Anxiety Disorder score and the Patient Health Questionnaire. The prevalence of psychological stress was calculated using logistic models adjusted for socio-demographic variables including race/ethnicity, education, household income, and household structure. All descriptive and regression analysis considered survey weights. Younger age groups experienced higher prevalence of psychological distress than older age groups. Among emerging adults, the prevalence of anxiety (42.6%) and depression (39.5%) was more than twice as high as older adults who experienced prevalence of anxiety at 20% and depression at 16.6%. Gender differences were also more apparent in emerging adults. Women between 18 and 24 years reported higher differential rates of anxiety and depression than those with men (anxiety: 43.9% vs. 28.3%; depression: 33.3% vs. 24.9%). Understanding the complex dynamics between COVID-19 and psychological distress has emerged as a public health priority. Mitigating the negative mental health consequences associated with the COVID-19 pandemic, for younger generations and females in particular, will require local efforts to rebuild capacity for social integration and social connection. Copyright © 2021 The Royal Society for Public Health. Published by Elsevier Ltd. All rights reserved.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">34473201</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">People who have been infected with or vaccinated against SARS-CoV-2 have reduced risk of subsequent infection, but the proportion of people in the US with SARS-CoV-2 antibodies from infection or vaccination is uncertain. To estimate trends in SARS-CoV-2 seroprevalence related to infection and vaccination in the US population. <AbstractText Label="Design, Setting, and Participants" NlmCategory="UNASSIGNED">In a repeated cross-sectional study conducted each month during July 2020 through May 2021, 17 blood collection organizations with blood donations from all 50 US states; Washington, DC; and Puerto Rico were organized into 66 study-specific regions, representing a catchment of 74% of the US population. For each study region, specimens from a median of approximately 2000 blood donors were selected and tested each month; a total of 1 594 363 specimens were initially selected and tested. The final date of blood donation collection was May 31, 2021. Calendar time. Proportion of persons with detectable SARS-CoV-2 spike and nucleocapsid antibodies. Seroprevalence was weighted for demographic differences between the blood donor sample and general population. Infection-induced seroprevalence was defined as the prevalence of the population with both spike and nucleocapsid antibodies. Combined infection- and vaccination-induced seroprevalence was defined as the prevalence of the population with spike antibodies. The seroprevalence estimates were compared with cumulative COVID-19 case report incidence rates. Among 1 443 519 specimens included, 733 052 (50.8%) were from women, 174 842 (12.1%) were from persons aged 16 to 29 years, 292 258 (20.2%) were from persons aged 65 years and older, 36 654 (2.5%) were from non-Hispanic Black persons, and 88 773 (6.1%) were from Hispanic persons. The overall infection-induced SARS-CoV-2 seroprevalence estimate increased from 3.5% (95% CI, 3.2%-3.8%) in July 2020 to 20.2% (95% CI, 19.9%-20.6%) in May 2021; the combined infection- and vaccination-induced seroprevalence estimate in May 2021 was 83.3% (95% CI, 82.9%-83.7%). By May 2021, 2.1 SARS-CoV-2 infections (95% CI, 2.0-2.1) per reported COVID-19 case were estimated to have occurred. Based on a sample of blood donations in the US from July 2020 through May 2021, vaccine- and infection-induced SARS-CoV-2 seroprevalence increased over time and varied by age, race and ethnicity, and geographic region. Despite weighting to adjust for demographic differences, these findings from a national sample of blood donors may not be representative of the entire US population.</td>
</tr>
<tr class="even">
<td style="text-align: left;">34448649</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">Our six goals are: 1) describe the relationship between the National Strategy for the COVID-19 Response and Pandemic Preparedness and the 55 US poison centers (PCs); 2) detail FDA emergency Use Authorization (EUA) COVID-19 vaccine-related regulatory procedures and associated acronyms; 3) list availability of specific vaccine clinical information to support PC staff COVID-19 vaccination and adverse event (AE) data collection; 4) describe required health care practitioner COVID-19 vaccine AE reporting to the Vaccine AE Reporting System (VAERS) and PC reporting options; 5) document public and health care professionals’ use of PCs for COVID-19 vaccine information; and 6) propose strategy to maximize PCs contribution to the pandemic solution. We reviewed 13-Feb-2020 through 15-Apr-2021 National Poison Data System (NPDS) COVID-19 records for changes over time. We examined NPDS cases and VAERS COVID-19 vaccine reports 1-Nov-2020 through 2-Apr-2021 for vaccine manufacturer, patient characteristics, state, and clinical effects. PCs reported 1,052,174 COVID-19 contacts; maximum (peak) contacts/day (12,163) on 16-Mar-2020. As of 5-Apr-2021 the US reported &gt;167 million administrations of COVID-19 vaccines (Pfizer-BioNTech, Moderna or Janssen). US PCs reported 162,052 COVID-19 vaccine contacts. Most (61.1%) were medical information calls, 34.9% were drug information, and 2.58% were exposures. Over the same period VAERS reported 49,078 COVID-19 vaccine cases reporting 226,205 symptoms - headache most frequent, ranging from 20% to 40% across the 3 vaccines. Although differences exist between the intent and content of the 2 data sets, NPDS volume is compelling. The PC nationwide 800 number facilitates data collection and suggests comingling the 2 data streams has merit. PC professionals received tens of thousands of calls and can: 1) support fact-based vaccine information; 2) contribute vaccine AE follow-up information: 3) advocate for best-case coding and reporting, especially for vaccine adverse experiences.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">34417121</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">Mathematical modelling has played a pivotal role in understanding the epidemiology of and guiding public health responses to the ongoing coronavirus disease of 2019 (COVID-19) pandemic. Here, we review the role of epidemiological models in understanding evolving epidemic characteristics, including the effects of vaccination and Variants of Concern (VoC). We highlight ways in which models continue to provide important insights, including (1) calculating the herd immunity threshold and evaluating its limitations; (2) verifying that nascent vaccines can prevent severe disease, infection, and transmission but may be less efficacious against VoC; (3) determining optimal vaccine allocation strategies under efficacy and supply constraints; and (4) determining that VoC are more transmissible and lethal than previously circulating strains, and that immune escape may jeopardize vaccine-induced herd immunity. Finally, we explore how models can help us anticipate and prepare for future stages of COVID-19 epidemiology (and that of other diseases) through forecasts and scenario projections, given current uncertainties and data limitations. Copyright © 2021. Published by Elsevier Ltd.</td>
</tr>
<tr class="even">
<td style="text-align: left;">34406840</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">COVID-19 vaccination campaigns continue in the United States, with the expectation that vaccines will slow transmission of the virus, save lives, and enable a return to normal life in due course. However, the extent to which faster vaccine administration has affected COVID-19-related deaths is unknown. We assessed the association between US state-level vaccination rates and COVID-19 deaths during the first five months of vaccine availability. We estimated that by May 9, 2021, the US vaccination campaign was associated with a reduction of 139,393 COVID-19 deaths. The association varied in different states. In New York, for example, vaccinations led to an estimated 11.7 fewer COVID-19 deaths per 10,000, whereas Hawaii observed the smallest reduction, with an estimated 1.1 fewer deaths per 10,000. Overall, our analysis suggests that the early COVID-19 vaccination campaign was associated with reductions in COVID-19 deaths. As of May 9, 2021, reductions in COVID-19 deaths associated with vaccines had translated to value of statistical life benefit ranging between $625 billion and $1.4 trillion.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">34391908</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">To evaluate the impact of the World Antimicrobial Awareness Week (WAAW) on public awareness of antimicrobial resistance using Google Trends analysis. The impact of WAAW on public awareness of ‘antimicrobial resistance’ (AMR), ‘antibacterial’, and ‘antibiotics’ in Japan, the UK, the United States, and worldwide from 2015 to 2020 was analyzed, using the relative search volume (RSV) of Google Trends as a surrogate. A joinpoint regression analysis was performed to identify a statistically significant time point of a change in trend. No joinpoints around WAAW were identified in Japan, the United Kingdom, or the United States from 2015 to 2020 with RSVs of ‘AMR’, whereas increasing RSVs were noted worldwide in 2017 and 2020. Further, there were decreasing RSVs of ‘antibiotics’ in the first half of 2020, which could be due to the COVID-19 pandemic. The study results suggest that WAAW did little to improve public awareness of AMR in the selected countries despite its contribution worldwide. This study implies that we need to develop a more effective method to improve public awareness to fight against AMR. Copyright © 2021 The Author(s). Published by Elsevier Ltd.. All rights reserved.</td>
</tr>
<tr class="even">
<td style="text-align: left;">34367726</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">The impact of COVID-19 disease on health and economy has been global, and the magnitude of devastation is unparalleled in modern history. Any potential course of action to manage this complex disease requires the systematic and efficient analysis of data that can delineate the underlying pathogenesis. We have developed a mathematical model of disease progression to predict the clinical outcome, utilizing a set of causal factors known to contribute to COVID-19 pathology such as age, comorbidities, and certain viral and immunological parameters. Viral load and selected indicators of a dysfunctional immune response, such as cytokines IL-6 and IFNα which contribute to the cytokine storm and fever, parameters of inflammation D-Dimer and Ferritin, aberrations in lymphocyte number, lymphopenia, and neutralizing antibodies were included for the analysis. The model provides a framework to unravel the multi-factorial complexities of the immune response manifested in SARS-CoV-2 infected individuals. Further, this model can be valuable to predict clinical outcome at an individual level, and to develop strategies for allocating appropriate resources to manage severe cases at a population level.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">34355196</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">Native Hawaiian and Pacific Islander (NHPI) populations suffer from disproportionately higher rates of chronic conditions, such as type 2 diabetes, that arises from metabolic dysfunction and are often associated with obesity and inflammation. In addition, the global coronavirus disease 2019 pandemic has further compounded the effect of health inequities observed in Indigenous populations, including NHPI communities. Reversible lifestyle habits, such as diet, may either be protective of or contribute to the increasing prevalence of health inequities in these populations via the immunoepigenetic-microbiome axis. This axis offers insight into the connection between diet, epigenetics, the microbiome composition, immune function, and response to viral infection. Epigenetic mechanisms that regulate inflammatory states associated with metabolic diseases, including diabetes, are impacted by diet. Furthermore, diet may modulate the gut microbiome by influencing microbial diversity and richness; dysbiosis of the microbiome is associated with chronic disease. A high fiber diet facilitates a favorable microbiome composition and in turn increases production of intermediate metabolites named short-chain fatty acids (SCFAs) that act on metabolic and immune pathways. In contrast, low fiber diets typically associated with a westernized lifestyle decreases the abundance of microbial derived SCFAs. This decreased abundance is characteristic of metabolic syndromes and activation of chronic inflammatory states, having larger implications in disease pathogenesis of both communicable and non-communicable diseases. Native Hawaiians and Pacific Islanders that once thrived on healthy traditional diets may be more sensitive than non-indigenous peoples to the metabolic perturbation of westernized diets that impinge on the immunoepigenetic-gut microbiome axis. Recent studies conducted in the Maunakea lab at the University of Hawai’i at Mānoa John A. Burns School of Medicine have helped elucidate the connections between diet, microbiome composition, metabolic syndrome, and epigenetic regulation of immune function to better understand disease pathogenesis. Potentially, this research could point to ways to prevent pre-disease conditions through novel biomarker discovery using community-based approaches. ©Copyright 2021 by University Health Partners of Hawai‘i (UHP Hawai‘i).</td>
</tr>
<tr class="even">
<td style="text-align: left;">34352507</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">The United States experienced three surges of COVID-19 community infection since the World Health Organization declared the pandemic on March 11, 2020. The prevalence of psychological distress among U.S. adults increased from 11 % in 2019 to 35.9 % in April 2020 when New York City become the epicenter of the COVID-19 outbreak. Analyzing 21 waves of the Household Pulse Survey data collected between April 2020 and December 2020, this study aimed to examine the distress level in the 15 most populated metropolitan areas in the U.S. Our study found that, as the pandemic swept from East to South and soared in the West, 39.9%-52.3 % U.S. adults living in these 15 metropolitan areas reported symptoms of psychological distress. The highest distress levels were found within the Western areas including Riverside-San Bernardino-Ontario (52.3 % in July 2020, 95 % CI: 44.9%-59.6 %) and Los Angeles-Long Beach-Anaheim (49.9 % in December 2020, 95 % CI: 44.5%-55.4 %). The lowest distress level was observed in Washington-Arlington-Alexandria ranging from 29.1 % in May 2020 to 39.9 % in November 2020. COVID-19 and its complex ecology of social and economic stressors have engaged high levels of sustained psychological distress. Our findings will support the efforts of local, state and national leadership to plan interventions by addressing not only the medical, but also the economic and social conditions associated with the pandemic. Copyright © 2021 Elsevier Ltd. All rights reserved.</td>
</tr>
<tr class="odd">
<td style="text-align: left;">34334985</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">Although the COVID-19 pandemic has disrupted elective shoulder arthroplasty throughput, traumatic shoulder arthroplasty procedures are less apt to be postponed. We sought to evaluate shoulder arthroplasty utilization for fracture during the COVID-19 pandemic and California’s associated shelter-in-place order compared to historical controls. We conducted a cohort study with historical controls, identifying patients who underwent shoulder arthroplasty for proximal humerus fracture in California using our integrated electronic health record. The time period of interest was following the implementation of the statewide shelter-in-place order: March 19, 2020-May 31, 2020. This was compared to three historical periods: January 1, 2020-March 18, 2020, March 18, 2019-May 31, 2019, and January 1, 2019-March 18, 2019. Procedure volume, patient characteristics, in-hospital length of stay, and 30-day events (emergency department visit, readmission, infection, pneumonia, and death) were reported. Changes over time were analyzed using linear regression adjusted for usual seasonal and yearly changes and age, sex, comorbidities, and postadmission factors. Surgical volume dropped from an average of 4.4, 5.2, and 2.6 surgeries per week in the historical time periods, respectively, to 2.4 surgeries per week after shelter-in-place. While no more than 30% of all shoulder arthroplasty procedures performed during any given week were for fracture during the historical time periods, arthroplasties performed for fracture was the overwhelming primary indication immediately after the shelter-in-place order. More patients were discharged the day of surgery (+33.2%, P = .019) after the shelter-in-place order, but we did not observe a change in any of the corresponding 30-day events. The volume of shoulder arthroplasty for fracture dropped during the time of COVID-19. The reduction in volume could be due to less shoulder trauma due to shelter-in-place or a change in the indications for arthroplasty given the perceived higher risks associated with intubation and surgical care. We noted more patients undergoing shoulder arthroplasty for fracture were safely discharged on the day of surgery, suggesting this may be a safe practice that can be adopted moving forward. Level III; Retrospective Case-control Comparative Study. © 2021 American Shoulder and Elbow Surgeons. Published by Elsevier Inc. All rights reserved.</td>
</tr>
<tr class="even">
<td style="text-align: left;">34314211</td>
<td style="text-align: left;">P</td>
<td style="text-align: left;">As of March 2021, Native Hawaiians and Pacific Islanders (NHPIs) in the United States have lost more than 800 lives to COVID-19-the highest per capita death rate in 18 of 20 US states reporting NHPI deaths. However, NHPI risks are overlooked in policy discussions. We discuss the NHPI COVID-19 Data Policy Lab and dashboard, featuring the disproportionate COVID-19 mortality burden for NHPIs. The Lab democratized NHPI data, developed community infrastructure and resources, and informed testing site and outreach policies related to health equity.</td>
</tr>
</tbody>
</table>

Some papers about COVID-19 and Hawaii

    titles <- str_extract(pub_char_list, "<ArticleTitle>[[:print:][:space:]]+</ArticleTitle>")
    titles <- str_remove_all(titles, "</?[[:alnum:]- =\"]+>")
