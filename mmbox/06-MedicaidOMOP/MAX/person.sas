data max_person_table_2013(keep=person_id
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
);set max2013.maxdata_ps_2013;
	person_id=bene_id;
	year_of_birth=year(el_dob);
	month_of_birth=month(el_dob);
	day_of_birth=day(el_dob);
	birth_datetime=el_dob  ;
	death_datetime=el_dod ;
	location_id='';
	provider_id='';
	care_site_id='';
	person_source_value=bene_id;
	gender_source_value=el_sex_cd;
	gender_source_concept_id='';
	race_source_value=el_race_ethncy_cd;
	race_source_concept_id='';
	ethnicty_source_concept_id=ethnicity_code;
format
	birth_datetime MMDDYYD10.
	death_datetime MMDDYYD10.;
	if el_sex_cd='F' then gender_concept_id='8353';/*female*/
	if el_sex_cd='M' then gender_concept_id='8507';/*male*/
	if el_sex_cd='U' then gender_concept_id='0';
	if el_race_ethncy_cd=2 then race_concept_id='8516';/*black*/
	if el_race_ethncy_cd=1 then race_concept_id='8527';/*white*/
	if el_race_ethncy_cd=3 then race_concept_id='8515';/*asian*/
	if el_race_ethncy_cd=4 then race_concept_id='8657'/*AIAN*/;
	if el_race_ethncy_cd=5 then race_concept_id='38003563';/*hispanic*/
	if el_race_ethncy_cd=6 then race_concept_id='8557';/*NH or OPI*/
	if el_race_ethncy_cd=7 then race_concept_id='38003563';/*Hispanic multi*/
	if el_race_ethncy_cd=8 then race_concept_id='0';/*multi*/
	if el_race_ethncy_cd=9 then race_concept_id='0';/*blank unknown*/
	if ethnicity_code=1 then ethnicity_concept_id='38003563';/*Hispanic*/
	if ethnicity_code=0 then ethnicity_concept_id='38003564';/*not hHspanic*/
	if ethnicity_code=9 then ethnicity_concept_id='0';/*blank*/
	run;
