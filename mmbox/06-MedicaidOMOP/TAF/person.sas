
data taf_person_table_2016(keep=person_id
	gender_concept_id
	year_of_birth
	month_of_birth
	day_of_birth
	birth_datetime
	death_datetime
	race_concept_id
	ethnicity_concept_id
	location_id
	provider_id
	care_site_id
	person_source_value
	gender_source_value
	gender_source_concept_id
	race_source_value
	race_source_concept_id
	ethnicty_source_concept_id
);set tafr16.demog_elig_base;
	person_id=bene_id;
	year_of_birth=year(birth_dt);
	month_of_birth=month(birth_dt);
	day_of_birth=day(birth_dt);
	birth_datetime=birth_dt  ;
	death_datetime=death_dt ;
	location_id='';
	provider_id='';
	care_site_id='';
	person_source_value=bene_id;
	gender_source_value=sex_cd;
	gender_source_concept_id='';
	race_source_value=RACE_ETHNCTY_CD;
	race_source_concept_id='';
	ethnicty_source_concept_id=ethncty_cd;
format
	birth_datetime MMDDYYD10.
	death_datetime MMDDYYD10.;
	if sex_cd='F' then gender_concept_id='8353';/*female*/
	if sex_cd='M' then gender_concept_id='8507';/*male*/
	if sex_cd='' then gender_concept_id='0';
	if RACE_ETHNCTY_CD='2' then race_concept_id='8516';/*black*/
	if RACE_ETHNCTY_CD='1' then race_concept_id='8527';/*white*/
	if RACE_ETHNCTY_CD='3' then race_concept_id='8515';/*asian*/
	if RACE_ETHNCTY_CD='4' then race_concept_id='8657'/*american indian alaska native*/;
	if RACE_ETHNCTY_CD='5' then race_concept_id='8557';/*native hawaiian or OPI*/
	if RACE_ETHNCTY_CD='6' then race_concept_id='0';/*multi*/
	if RACE_ETHNCTY_CD='7' then race_concept_id='38003563';/*hispanic*/
	if RACE_ETHNCTY_CD='' then race_concept_id='0';/*blank unknown*/
	if ethncty_cd='1' then ethnicity_concept_id='38003563';/*Hispanic*/
	if ethncty_cd='2' then ethnicity_concept_id='38003563';/*Hispanic*/
	if ethncty_cd='3' then ethnicity_concept_id='38003563';/*Hispanic*/
	if ethncty_cd='4' then ethnicity_concept_id='38003563';/*Hispanic*/
	if ethncty_cd='5' then ethnicity_concept_id='38003563';/*Hispanic*/
	if ethncty_cd='0' then ethnicity_concept_id='38003564';/*not hispanic*/
	if ethncty_cd='' then ethnicity_concept_id='0';/*blank*/
	run;