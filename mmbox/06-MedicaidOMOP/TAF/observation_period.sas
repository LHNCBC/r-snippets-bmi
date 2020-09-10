data workingfile;
set
tafr14.demog_elig_dates
tafr15.demog_elig_dates
tafr16.demog_elig_dates
;run;

data rollup1;
set workingfile(rename(bene_id=idvar
enrlmt_start_dt=adm_date
enrlmt_end_dt=dis_date));
if adm_date eq. and dis_date eq. then delete;
if adm_date eq. and dis_date ne . Then adm_date=dis_date;
if adm_date ne. and dis_date eq . Then dis_date=adm_date;
run;

proc sort date=rollup1;
by idvar adm_date dis_date;
run;

data rollup2(keep=idvar in_date out_date state_cd);
set rollup1
;byidvar;
if first.idvar then do;
in_date=adm_date;
out_date=dis_date;
retain in_date;
out_date;
end;
else do;
if adm_date le out_date then do;
if out_date lt dis_date then do;
out_date=dis_date;
retain out_date;
end;
end;
if adm_date gt out_date then dol
if intck('DAYS',out_date adm_date) gt 1 then do;
output;
in_date=adm_date;
out_date=dis_date;
retain in_date out_date;
end;end;end;
if last.idvar then output;
format adm_date dis_date in_date out_date date10.
;run;
data rollup3;
set rollup2;
month=intck('MONTHS',indate,outdate);
max=out_date-indate;run;

proc sql; create table taf_largest_enrollment_period as
selct count(distinct idvar) as pt_cnt,
max(months),
state_cd 
from rollup3
group by months,state_cd;

data rollup4;
set rollup3;
byidvar in_date;
if first.in_date and first.id_var then episode_order='first';run;

proc sql; create table taf_earlyest_enrollment_period as
select count(distinct idvar) as pt_cnt,
months,state_cd from rollup4 where episode_order='first' group by months,state_cd;
