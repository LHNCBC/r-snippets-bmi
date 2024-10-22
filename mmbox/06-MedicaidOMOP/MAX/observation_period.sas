

%macro patchy(year);
data workingfile_&year(keep=bene_id 
MAX_ELG_CD_MO_&year.01-MAX_ELG_CD_MO_&year.12
EL_RSTRCT_BNFT_FLG_&year.01-EL_RSTRCT_BNFT_FLG_&year.12
state_cd);
set max&year..maxdata_ps_&year(where=(state_cd='AL'));
MAX_ELG_CD_MO_&year.01=MAX_ELG_CD_MO_1;
MAX_ELG_CD_MO_&year.02=MAX_ELG_CD_MO_2;
MAX_ELG_CD_MO_&year.03=MAX_ELG_CD_MO_3;
MAX_ELG_CD_MO_&year.04=MAX_ELG_CD_MO_4;
MAX_ELG_CD_MO_&year.05=MAX_ELG_CD_MO_5;
MAX_ELG_CD_MO_&year.06=MAX_ELG_CD_MO_6;
MAX_ELG_CD_MO_&year.07=MAX_ELG_CD_MO_7;
MAX_ELG_CD_MO_&year.08=MAX_ELG_CD_MO_8;
MAX_ELG_CD_MO_&year.09=MAX_ELG_CD_MO_9;
MAX_ELG_CD_MO_&year.10=MAX_ELG_CD_MO_10;
MAX_ELG_CD_MO_&year.11=MAX_ELG_CD_MO_11;
MAX_ELG_CD_MO_&year.12=MAX_ELG_CD_MO_12;
EL_RSTRCT_BNFT_FLG_&year.01=EL_RSTRCT_BNFT_FLG_1;
EL_RSTRCT_BNFT_FLG_&year.02=EL_RSTRCT_BNFT_FLG_2;
EL_RSTRCT_BNFT_FLG_&year.03=EL_RSTRCT_BNFT_FLG_3;
EL_RSTRCT_BNFT_FLG_&year.04=EL_RSTRCT_BNFT_FLG_4;
EL_RSTRCT_BNFT_FLG_&year.05=EL_RSTRCT_BNFT_FLG_5;
EL_RSTRCT_BNFT_FLG_&year.06=EL_RSTRCT_BNFT_FLG_6;
EL_RSTRCT_BNFT_FLG_&year.07=EL_RSTRCT_BNFT_FLG_7;
EL_RSTRCT_BNFT_FLG_&year.08=EL_RSTRCT_BNFT_FLG_8;
EL_RSTRCT_BNFT_FLG_&year.09=EL_RSTRCT_BNFT_FLG_9;
EL_RSTRCT_BNFT_FLG_&year.10=EL_RSTRCT_BNFT_FLG_10;
EL_RSTRCT_BNFT_FLG_&year.11=EL_RSTRCT_BNFT_FLG_11;
EL_RSTRCT_BNFT_FLG_&year.12=EL_RSTRCT_BNFT_FLG_12;
run;%mend;
%macro multi_patchy(first_year,last_year);
%do year=&first_year %to &last_year;
%patchy(year=&year);run;%end;%mend;
%multi_patchy(
first_year=1999,
last_year=2013);run;

%macro lazy_sorter;

%do year=1999 %to 2013;
proc sort data=work.workingfile_&year;
by bene_id state_cd;run;
%end;
%mend;
%lazy_sorter();run;

data workingfile(where=(bene_id>1));
merge
work.workingfile_1999-work.workingfile_2013;
by bene_id state_cd;run;


data workingfile(keep=state_cd bene_id max_elg_cd_mo_1-max_elg_cd_mo_180);
set workingfile;
rename 
MAX_ELG_CD_MO_199901=MAX_ELG_CD_MO_1
MAX_ELG_CD_MO_199902=MAX_ELG_CD_MO_2
MAX_ELG_CD_MO_199903=MAX_ELG_CD_MO_3
MAX_ELG_CD_MO_199904=MAX_ELG_CD_MO_4
MAX_ELG_CD_MO_199905=MAX_ELG_CD_MO_5
MAX_ELG_CD_MO_199906=MAX_ELG_CD_MO_6
MAX_ELG_CD_MO_199907=MAX_ELG_CD_MO_7
MAX_ELG_CD_MO_199908=MAX_ELG_CD_MO_8
MAX_ELG_CD_MO_199909=MAX_ELG_CD_MO_9
MAX_ELG_CD_MO_199910=MAX_ELG_CD_MO_10
MAX_ELG_CD_MO_199911=MAX_ELG_CD_MO_11
MAX_ELG_CD_MO_199912=MAX_ELG_CD_MO_12
MAX_ELG_CD_MO_200001=MAX_ELG_CD_MO_13
MAX_ELG_CD_MO_200002=MAX_ELG_CD_MO_14
MAX_ELG_CD_MO_200003=MAX_ELG_CD_MO_15
MAX_ELG_CD_MO_200004=MAX_ELG_CD_MO_16
MAX_ELG_CD_MO_200005=MAX_ELG_CD_MO_17
MAX_ELG_CD_MO_200006=MAX_ELG_CD_MO_18
MAX_ELG_CD_MO_200007=MAX_ELG_CD_MO_19
MAX_ELG_CD_MO_200008=MAX_ELG_CD_MO_20
MAX_ELG_CD_MO_200009=MAX_ELG_CD_MO_21
MAX_ELG_CD_MO_200010=MAX_ELG_CD_MO_22
MAX_ELG_CD_MO_200011=MAX_ELG_CD_MO_23
MAX_ELG_CD_MO_200012=MAX_ELG_CD_MO_24
MAX_ELG_CD_MO_200101=MAX_ELG_CD_MO_25
MAX_ELG_CD_MO_200102=MAX_ELG_CD_MO_26
MAX_ELG_CD_MO_200103=MAX_ELG_CD_MO_27
MAX_ELG_CD_MO_200104=MAX_ELG_CD_MO_28
MAX_ELG_CD_MO_200105=MAX_ELG_CD_MO_29
MAX_ELG_CD_MO_200106=MAX_ELG_CD_MO_30
MAX_ELG_CD_MO_200107=MAX_ELG_CD_MO_31
MAX_ELG_CD_MO_200108=MAX_ELG_CD_MO_32
MAX_ELG_CD_MO_200109=MAX_ELG_CD_MO_33
MAX_ELG_CD_MO_200110=MAX_ELG_CD_MO_34
MAX_ELG_CD_MO_200111=MAX_ELG_CD_MO_35
MAX_ELG_CD_MO_200112=MAX_ELG_CD_MO_36
MAX_ELG_CD_MO_200201=MAX_ELG_CD_MO_37
MAX_ELG_CD_MO_200202=MAX_ELG_CD_MO_38
MAX_ELG_CD_MO_200203=MAX_ELG_CD_MO_39
MAX_ELG_CD_MO_200204=MAX_ELG_CD_MO_40
MAX_ELG_CD_MO_200205=MAX_ELG_CD_MO_41
MAX_ELG_CD_MO_200206=MAX_ELG_CD_MO_42
MAX_ELG_CD_MO_200207=MAX_ELG_CD_MO_43
MAX_ELG_CD_MO_200208=MAX_ELG_CD_MO_44
MAX_ELG_CD_MO_200209=MAX_ELG_CD_MO_45
MAX_ELG_CD_MO_200210=MAX_ELG_CD_MO_46
MAX_ELG_CD_MO_200211=MAX_ELG_CD_MO_47
MAX_ELG_CD_MO_200212=MAX_ELG_CD_MO_48
MAX_ELG_CD_MO_200301=MAX_ELG_CD_MO_49
MAX_ELG_CD_MO_200302=MAX_ELG_CD_MO_50
MAX_ELG_CD_MO_200303=MAX_ELG_CD_MO_51
MAX_ELG_CD_MO_200304=MAX_ELG_CD_MO_52
MAX_ELG_CD_MO_200305=MAX_ELG_CD_MO_53
MAX_ELG_CD_MO_200306=MAX_ELG_CD_MO_54
MAX_ELG_CD_MO_200307=MAX_ELG_CD_MO_55
MAX_ELG_CD_MO_200308=MAX_ELG_CD_MO_56
MAX_ELG_CD_MO_200309=MAX_ELG_CD_MO_57
MAX_ELG_CD_MO_200310=MAX_ELG_CD_MO_58
MAX_ELG_CD_MO_200311=MAX_ELG_CD_MO_59
MAX_ELG_CD_MO_200312=MAX_ELG_CD_MO_60
MAX_ELG_CD_MO_200401=MAX_ELG_CD_MO_61
MAX_ELG_CD_MO_200402=MAX_ELG_CD_MO_62
MAX_ELG_CD_MO_200403=MAX_ELG_CD_MO_63
MAX_ELG_CD_MO_200404=MAX_ELG_CD_MO_64
MAX_ELG_CD_MO_200405=MAX_ELG_CD_MO_65
MAX_ELG_CD_MO_200406=MAX_ELG_CD_MO_66
MAX_ELG_CD_MO_200407=MAX_ELG_CD_MO_67
MAX_ELG_CD_MO_200408=MAX_ELG_CD_MO_68
MAX_ELG_CD_MO_200409=MAX_ELG_CD_MO_69
MAX_ELG_CD_MO_200410=MAX_ELG_CD_MO_70
MAX_ELG_CD_MO_200411=MAX_ELG_CD_MO_71
MAX_ELG_CD_MO_200412=MAX_ELG_CD_MO_72
MAX_ELG_CD_MO_200501=MAX_ELG_CD_MO_73
MAX_ELG_CD_MO_200502=MAX_ELG_CD_MO_74
MAX_ELG_CD_MO_200503=MAX_ELG_CD_MO_75
MAX_ELG_CD_MO_200504=MAX_ELG_CD_MO_76
MAX_ELG_CD_MO_200505=MAX_ELG_CD_MO_77
MAX_ELG_CD_MO_200506=MAX_ELG_CD_MO_78
MAX_ELG_CD_MO_200507=MAX_ELG_CD_MO_79
MAX_ELG_CD_MO_200508=MAX_ELG_CD_MO_80
MAX_ELG_CD_MO_200509=MAX_ELG_CD_MO_81
MAX_ELG_CD_MO_200510=MAX_ELG_CD_MO_82
MAX_ELG_CD_MO_200511=MAX_ELG_CD_MO_83
MAX_ELG_CD_MO_200512=MAX_ELG_CD_MO_84
MAX_ELG_CD_MO_200601=MAX_ELG_CD_MO_85
MAX_ELG_CD_MO_200602=MAX_ELG_CD_MO_86
MAX_ELG_CD_MO_200603=MAX_ELG_CD_MO_87
MAX_ELG_CD_MO_200604=MAX_ELG_CD_MO_88
MAX_ELG_CD_MO_200605=MAX_ELG_CD_MO_89
MAX_ELG_CD_MO_200606=MAX_ELG_CD_MO_90
MAX_ELG_CD_MO_200607=MAX_ELG_CD_MO_91
MAX_ELG_CD_MO_200608=MAX_ELG_CD_MO_92
MAX_ELG_CD_MO_200609=MAX_ELG_CD_MO_93
MAX_ELG_CD_MO_200610=MAX_ELG_CD_MO_94
MAX_ELG_CD_MO_200611=MAX_ELG_CD_MO_95
MAX_ELG_CD_MO_200612=MAX_ELG_CD_MO_96
MAX_ELG_CD_MO_200701=MAX_ELG_CD_MO_97
MAX_ELG_CD_MO_200702=MAX_ELG_CD_MO_98
MAX_ELG_CD_MO_200703=MAX_ELG_CD_MO_99
MAX_ELG_CD_MO_200704=MAX_ELG_CD_MO_100
MAX_ELG_CD_MO_200705=MAX_ELG_CD_MO_101
MAX_ELG_CD_MO_200706=MAX_ELG_CD_MO_102
MAX_ELG_CD_MO_200707=MAX_ELG_CD_MO_103
MAX_ELG_CD_MO_200708=MAX_ELG_CD_MO_104
MAX_ELG_CD_MO_200709=MAX_ELG_CD_MO_105
MAX_ELG_CD_MO_200710=MAX_ELG_CD_MO_106
MAX_ELG_CD_MO_200711=MAX_ELG_CD_MO_107
MAX_ELG_CD_MO_200712=MAX_ELG_CD_MO_108
MAX_ELG_CD_MO_200801=MAX_ELG_CD_MO_109
MAX_ELG_CD_MO_200802=MAX_ELG_CD_MO_110
MAX_ELG_CD_MO_200803=MAX_ELG_CD_MO_111
MAX_ELG_CD_MO_200804=MAX_ELG_CD_MO_112
MAX_ELG_CD_MO_200805=MAX_ELG_CD_MO_113
MAX_ELG_CD_MO_200806=MAX_ELG_CD_MO_114
MAX_ELG_CD_MO_200807=MAX_ELG_CD_MO_115
MAX_ELG_CD_MO_200808=MAX_ELG_CD_MO_116
MAX_ELG_CD_MO_200809=MAX_ELG_CD_MO_117
MAX_ELG_CD_MO_200810=MAX_ELG_CD_MO_118
MAX_ELG_CD_MO_200811=MAX_ELG_CD_MO_119
MAX_ELG_CD_MO_200812=MAX_ELG_CD_MO_120
MAX_ELG_CD_MO_200901=MAX_ELG_CD_MO_121
MAX_ELG_CD_MO_200902=MAX_ELG_CD_MO_122
MAX_ELG_CD_MO_200903=MAX_ELG_CD_MO_123
MAX_ELG_CD_MO_200904=MAX_ELG_CD_MO_124
MAX_ELG_CD_MO_200905=MAX_ELG_CD_MO_125
MAX_ELG_CD_MO_200906=MAX_ELG_CD_MO_126
MAX_ELG_CD_MO_200907=MAX_ELG_CD_MO_127
MAX_ELG_CD_MO_200908=MAX_ELG_CD_MO_128
MAX_ELG_CD_MO_200909=MAX_ELG_CD_MO_129
MAX_ELG_CD_MO_200910=MAX_ELG_CD_MO_130
MAX_ELG_CD_MO_200911=MAX_ELG_CD_MO_131
MAX_ELG_CD_MO_200912=MAX_ELG_CD_MO_132
MAX_ELG_CD_MO_201001=MAX_ELG_CD_MO_133
MAX_ELG_CD_MO_201002=MAX_ELG_CD_MO_134
MAX_ELG_CD_MO_201003=MAX_ELG_CD_MO_135
MAX_ELG_CD_MO_201004=MAX_ELG_CD_MO_136
MAX_ELG_CD_MO_201005=MAX_ELG_CD_MO_137
MAX_ELG_CD_MO_201006=MAX_ELG_CD_MO_138
MAX_ELG_CD_MO_201007=MAX_ELG_CD_MO_139
MAX_ELG_CD_MO_201008=MAX_ELG_CD_MO_140
MAX_ELG_CD_MO_201009=MAX_ELG_CD_MO_141
MAX_ELG_CD_MO_201010=MAX_ELG_CD_MO_142
MAX_ELG_CD_MO_201011=MAX_ELG_CD_MO_143
MAX_ELG_CD_MO_201012=MAX_ELG_CD_MO_144
MAX_ELG_CD_MO_201101=MAX_ELG_CD_MO_145
MAX_ELG_CD_MO_201102=MAX_ELG_CD_MO_146
MAX_ELG_CD_MO_201103=MAX_ELG_CD_MO_147
MAX_ELG_CD_MO_201104=MAX_ELG_CD_MO_148
MAX_ELG_CD_MO_201105=MAX_ELG_CD_MO_149
MAX_ELG_CD_MO_201106=MAX_ELG_CD_MO_150
MAX_ELG_CD_MO_201107=MAX_ELG_CD_MO_151
MAX_ELG_CD_MO_201108=MAX_ELG_CD_MO_152
MAX_ELG_CD_MO_201109=MAX_ELG_CD_MO_153
MAX_ELG_CD_MO_201110=MAX_ELG_CD_MO_154
MAX_ELG_CD_MO_201111=MAX_ELG_CD_MO_155
MAX_ELG_CD_MO_201112=MAX_ELG_CD_MO_156
MAX_ELG_CD_MO_201201=MAX_ELG_CD_MO_157
MAX_ELG_CD_MO_201202=MAX_ELG_CD_MO_158
MAX_ELG_CD_MO_201203=MAX_ELG_CD_MO_159
MAX_ELG_CD_MO_201204=MAX_ELG_CD_MO_160
MAX_ELG_CD_MO_201205=MAX_ELG_CD_MO_161
MAX_ELG_CD_MO_201206=MAX_ELG_CD_MO_162
MAX_ELG_CD_MO_201207=MAX_ELG_CD_MO_163
MAX_ELG_CD_MO_201208=MAX_ELG_CD_MO_164
MAX_ELG_CD_MO_201209=MAX_ELG_CD_MO_165
MAX_ELG_CD_MO_201210=MAX_ELG_CD_MO_166
MAX_ELG_CD_MO_201211=MAX_ELG_CD_MO_167
MAX_ELG_CD_MO_201212=MAX_ELG_CD_MO_168
MAX_ELG_CD_MO_201301=MAX_ELG_CD_MO_169
MAX_ELG_CD_MO_201302=MAX_ELG_CD_MO_170
MAX_ELG_CD_MO_201303=MAX_ELG_CD_MO_171
MAX_ELG_CD_MO_201304=MAX_ELG_CD_MO_172
MAX_ELG_CD_MO_201305=MAX_ELG_CD_MO_173
MAX_ELG_CD_MO_201306=MAX_ELG_CD_MO_174
MAX_ELG_CD_MO_201307=MAX_ELG_CD_MO_175
MAX_ELG_CD_MO_201308=MAX_ELG_CD_MO_176
MAX_ELG_CD_MO_201309=MAX_ELG_CD_MO_177
MAX_ELG_CD_MO_201310=MAX_ELG_CD_MO_178
MAX_ELG_CD_MO_201311=MAX_ELG_CD_MO_179
MAX_ELG_CD_MO_201312=MAX_ELG_CD_MO_180;
run;


proc transpose 
data=workingfile 
out=workingfile_t;
by bene_id state_cd;
var MAX_ELG_CD_MO_1-MAX_ELG_CD_MO_180;
run;
data workingfile_t(where=(col1^=''));
set
workingfile_t;
run;
data workingfile_t1(keep=bene_id state_cd col1 YYYYYMM);
set workingfile_t;
if _name_= 'MAX_ELG_CD_MO_1' then  YYYYYMM=199901;
if _name_= 'MAX_ELG_CD_MO_2' then  YYYYYMM=199902;
if _name_= 'MAX_ELG_CD_MO_3' then  YYYYYMM=199903;
if _name_= 'MAX_ELG_CD_MO_4' then  YYYYYMM=199904;
if _name_= 'MAX_ELG_CD_MO_5' then  YYYYYMM=199905;
if _name_= 'MAX_ELG_CD_MO_6' then  YYYYYMM=199906;
if _name_= 'MAX_ELG_CD_MO_7' then  YYYYYMM=199907;
if _name_= 'MAX_ELG_CD_MO_8' then  YYYYYMM=199908;
if _name_= 'MAX_ELG_CD_MO_9' then  YYYYYMM=199909;
if _name_= 'MAX_ELG_CD_MO_10' then  YYYYYMM=199910;
if _name_= 'MAX_ELG_CD_MO_11' then  YYYYYMM=199911;
if _name_= 'MAX_ELG_CD_MO_12' then  YYYYYMM=199912;
if _name_= 'MAX_ELG_CD_MO_13' then  YYYYYMM=200001;
if _name_= 'MAX_ELG_CD_MO_14' then  YYYYYMM=200002;
if _name_= 'MAX_ELG_CD_MO_15' then  YYYYYMM=200003;
if _name_= 'MAX_ELG_CD_MO_16' then  YYYYYMM=200004;
if _name_= 'MAX_ELG_CD_MO_17' then  YYYYYMM=200005;
if _name_= 'MAX_ELG_CD_MO_18' then  YYYYYMM=200006;
if _name_= 'MAX_ELG_CD_MO_19' then  YYYYYMM=200007;
if _name_= 'MAX_ELG_CD_MO_20' then  YYYYYMM=200008;
if _name_= 'MAX_ELG_CD_MO_21' then  YYYYYMM=200009;
if _name_= 'MAX_ELG_CD_MO_22' then  YYYYYMM=200010;
if _name_= 'MAX_ELG_CD_MO_23' then  YYYYYMM=200011;
if _name_= 'MAX_ELG_CD_MO_24' then  YYYYYMM=200012;
if _name_= 'MAX_ELG_CD_MO_25' then  YYYYYMM=200101;
if _name_= 'MAX_ELG_CD_MO_26' then  YYYYYMM=200102;
if _name_= 'MAX_ELG_CD_MO_27' then  YYYYYMM=200103;
if _name_= 'MAX_ELG_CD_MO_28' then  YYYYYMM=200104;
if _name_= 'MAX_ELG_CD_MO_29' then  YYYYYMM=200105;
if _name_= 'MAX_ELG_CD_MO_30' then  YYYYYMM=200106;
if _name_= 'MAX_ELG_CD_MO_31' then  YYYYYMM=200107;
if _name_= 'MAX_ELG_CD_MO_32' then  YYYYYMM=200108;
if _name_= 'MAX_ELG_CD_MO_33' then  YYYYYMM=200109;
if _name_= 'MAX_ELG_CD_MO_34' then  YYYYYMM=200110;
if _name_= 'MAX_ELG_CD_MO_35' then  YYYYYMM=200111;
if _name_= 'MAX_ELG_CD_MO_36' then  YYYYYMM=200112;
if _name_= 'MAX_ELG_CD_MO_37' then  YYYYYMM=200201;
if _name_= 'MAX_ELG_CD_MO_38' then  YYYYYMM=200202;
if _name_= 'MAX_ELG_CD_MO_39' then  YYYYYMM=200203;
if _name_= 'MAX_ELG_CD_MO_40' then  YYYYYMM=200204;
if _name_= 'MAX_ELG_CD_MO_41' then  YYYYYMM=200205;
if _name_= 'MAX_ELG_CD_MO_42' then  YYYYYMM=200206;
if _name_= 'MAX_ELG_CD_MO_43' then  YYYYYMM=200207;
if _name_= 'MAX_ELG_CD_MO_44' then  YYYYYMM=200208;
if _name_= 'MAX_ELG_CD_MO_45' then  YYYYYMM=200209;
if _name_= 'MAX_ELG_CD_MO_46' then  YYYYYMM=200210;
if _name_= 'MAX_ELG_CD_MO_47' then  YYYYYMM=200211;
if _name_= 'MAX_ELG_CD_MO_48' then  YYYYYMM=200212;
if _name_= 'MAX_ELG_CD_MO_49' then  YYYYYMM=200301;
if _name_= 'MAX_ELG_CD_MO_50' then  YYYYYMM=200302;
if _name_= 'MAX_ELG_CD_MO_51' then  YYYYYMM=200303;
if _name_= 'MAX_ELG_CD_MO_52' then  YYYYYMM=200304;
if _name_= 'MAX_ELG_CD_MO_53' then  YYYYYMM=200305;
if _name_= 'MAX_ELG_CD_MO_54' then  YYYYYMM=200306;
if _name_= 'MAX_ELG_CD_MO_55' then  YYYYYMM=200307;
if _name_= 'MAX_ELG_CD_MO_56' then  YYYYYMM=200308;
if _name_= 'MAX_ELG_CD_MO_57' then  YYYYYMM=200309;
if _name_= 'MAX_ELG_CD_MO_58' then  YYYYYMM=200310;
if _name_= 'MAX_ELG_CD_MO_59' then  YYYYYMM=200311;
if _name_= 'MAX_ELG_CD_MO_60' then  YYYYYMM=200312;
if _name_= 'MAX_ELG_CD_MO_61' then  YYYYYMM=200401;
if _name_= 'MAX_ELG_CD_MO_62' then  YYYYYMM=200402;
if _name_= 'MAX_ELG_CD_MO_63' then  YYYYYMM=200403;
if _name_= 'MAX_ELG_CD_MO_64' then  YYYYYMM=200404;
if _name_= 'MAX_ELG_CD_MO_65' then  YYYYYMM=200405;
if _name_= 'MAX_ELG_CD_MO_66' then  YYYYYMM=200406;
if _name_= 'MAX_ELG_CD_MO_67' then  YYYYYMM=200407;
if _name_= 'MAX_ELG_CD_MO_68' then  YYYYYMM=200408;
if _name_= 'MAX_ELG_CD_MO_69' then  YYYYYMM=200409;
if _name_= 'MAX_ELG_CD_MO_70' then  YYYYYMM=200410;
if _name_= 'MAX_ELG_CD_MO_71' then  YYYYYMM=200411;
if _name_= 'MAX_ELG_CD_MO_72' then  YYYYYMM=200412;
if _name_= 'MAX_ELG_CD_MO_73' then  YYYYYMM=200501;
if _name_= 'MAX_ELG_CD_MO_74' then  YYYYYMM=200502;
if _name_= 'MAX_ELG_CD_MO_75' then  YYYYYMM=200503;
if _name_= 'MAX_ELG_CD_MO_76' then  YYYYYMM=200504;
if _name_= 'MAX_ELG_CD_MO_77' then  YYYYYMM=200505;
if _name_= 'MAX_ELG_CD_MO_78' then  YYYYYMM=200506;
if _name_= 'MAX_ELG_CD_MO_79' then  YYYYYMM=200507;
if _name_= 'MAX_ELG_CD_MO_80' then  YYYYYMM=200508;
if _name_= 'MAX_ELG_CD_MO_81' then  YYYYYMM=200509;
if _name_= 'MAX_ELG_CD_MO_82' then  YYYYYMM=200510;
if _name_= 'MAX_ELG_CD_MO_83' then  YYYYYMM=200511;
if _name_= 'MAX_ELG_CD_MO_84' then  YYYYYMM=200512;
if _name_= 'MAX_ELG_CD_MO_85' then  YYYYYMM=200601;
if _name_= 'MAX_ELG_CD_MO_86' then  YYYYYMM=200602;
if _name_= 'MAX_ELG_CD_MO_87' then  YYYYYMM=200603;
if _name_= 'MAX_ELG_CD_MO_88' then  YYYYYMM=200604;
if _name_= 'MAX_ELG_CD_MO_89' then  YYYYYMM=200605;
if _name_= 'MAX_ELG_CD_MO_90' then  YYYYYMM=200606;
if _name_= 'MAX_ELG_CD_MO_91' then  YYYYYMM=200607;
if _name_= 'MAX_ELG_CD_MO_92' then  YYYYYMM=200608;
if _name_= 'MAX_ELG_CD_MO_93' then  YYYYYMM=200609;
if _name_= 'MAX_ELG_CD_MO_94' then  YYYYYMM=200610;
if _name_= 'MAX_ELG_CD_MO_95' then  YYYYYMM=200611;
if _name_= 'MAX_ELG_CD_MO_96' then  YYYYYMM=200612;
if _name_= 'MAX_ELG_CD_MO_97' then  YYYYYMM=200701;
if _name_= 'MAX_ELG_CD_MO_98' then  YYYYYMM=200702;
if _name_= 'MAX_ELG_CD_MO_99' then  YYYYYMM=200703;
if _name_= 'MAX_ELG_CD_MO_100' then  YYYYYMM=200704;
if _name_= 'MAX_ELG_CD_MO_101' then  YYYYYMM=200705;
if _name_= 'MAX_ELG_CD_MO_102' then  YYYYYMM=200706;
if _name_= 'MAX_ELG_CD_MO_103' then  YYYYYMM=200707;
if _name_= 'MAX_ELG_CD_MO_104' then  YYYYYMM=200708;
if _name_= 'MAX_ELG_CD_MO_105' then  YYYYYMM=200709;
if _name_= 'MAX_ELG_CD_MO_106' then  YYYYYMM=200710;
if _name_= 'MAX_ELG_CD_MO_107' then  YYYYYMM=200711;
if _name_= 'MAX_ELG_CD_MO_108' then  YYYYYMM=200712;
if _name_= 'MAX_ELG_CD_MO_109' then  YYYYYMM=200801;
if _name_= 'MAX_ELG_CD_MO_110' then  YYYYYMM=200802;
if _name_= 'MAX_ELG_CD_MO_111' then  YYYYYMM=200803;
if _name_= 'MAX_ELG_CD_MO_112' then  YYYYYMM=200804;
if _name_= 'MAX_ELG_CD_MO_113' then  YYYYYMM=200805;
if _name_= 'MAX_ELG_CD_MO_114' then  YYYYYMM=200806;
if _name_= 'MAX_ELG_CD_MO_115' then  YYYYYMM=200807;
if _name_= 'MAX_ELG_CD_MO_116' then  YYYYYMM=200808;
if _name_= 'MAX_ELG_CD_MO_117' then  YYYYYMM=200809;
if _name_= 'MAX_ELG_CD_MO_118' then  YYYYYMM=200810;
if _name_= 'MAX_ELG_CD_MO_119' then  YYYYYMM=200811;
if _name_= 'MAX_ELG_CD_MO_120' then  YYYYYMM=200812;
if _name_= 'MAX_ELG_CD_MO_121' then  YYYYYMM=200901;
if _name_= 'MAX_ELG_CD_MO_122' then  YYYYYMM=200902;
if _name_= 'MAX_ELG_CD_MO_123' then  YYYYYMM=200903;
if _name_= 'MAX_ELG_CD_MO_124' then  YYYYYMM=200904;
if _name_= 'MAX_ELG_CD_MO_125' then  YYYYYMM=200905;
if _name_= 'MAX_ELG_CD_MO_126' then  YYYYYMM=200906;
if _name_= 'MAX_ELG_CD_MO_127' then  YYYYYMM=200907;
if _name_= 'MAX_ELG_CD_MO_128' then  YYYYYMM=200908;
if _name_= 'MAX_ELG_CD_MO_129' then  YYYYYMM=200909;
if _name_= 'MAX_ELG_CD_MO_130' then  YYYYYMM=200910;
if _name_= 'MAX_ELG_CD_MO_131' then  YYYYYMM=200911;
if _name_= 'MAX_ELG_CD_MO_132' then  YYYYYMM=200912;
if _name_= 'MAX_ELG_CD_MO_133' then  YYYYYMM=201001;
if _name_= 'MAX_ELG_CD_MO_134' then  YYYYYMM=201002;
if _name_= 'MAX_ELG_CD_MO_135' then  YYYYYMM=201003;
if _name_= 'MAX_ELG_CD_MO_136' then  YYYYYMM=201004;
if _name_= 'MAX_ELG_CD_MO_137' then  YYYYYMM=201005;
if _name_= 'MAX_ELG_CD_MO_138' then  YYYYYMM=201006;
if _name_= 'MAX_ELG_CD_MO_139' then  YYYYYMM=201007;
if _name_= 'MAX_ELG_CD_MO_140' then  YYYYYMM=201008;
if _name_= 'MAX_ELG_CD_MO_141' then  YYYYYMM=201009;
if _name_= 'MAX_ELG_CD_MO_142' then  YYYYYMM=201010;
if _name_= 'MAX_ELG_CD_MO_143' then  YYYYYMM=201011;
if _name_= 'MAX_ELG_CD_MO_144' then  YYYYYMM=201012;
if _name_= 'MAX_ELG_CD_MO_145' then  YYYYYMM=201101;
if _name_= 'MAX_ELG_CD_MO_146' then  YYYYYMM=201102;
if _name_= 'MAX_ELG_CD_MO_147' then  YYYYYMM=201103;
if _name_= 'MAX_ELG_CD_MO_148' then  YYYYYMM=201104;
if _name_= 'MAX_ELG_CD_MO_149' then  YYYYYMM=201105;
if _name_= 'MAX_ELG_CD_MO_150' then  YYYYYMM=201106;
if _name_= 'MAX_ELG_CD_MO_151' then  YYYYYMM=201107;
if _name_= 'MAX_ELG_CD_MO_152' then  YYYYYMM=201108;
if _name_= 'MAX_ELG_CD_MO_153' then  YYYYYMM=201109;
if _name_= 'MAX_ELG_CD_MO_154' then  YYYYYMM=201110;
if _name_= 'MAX_ELG_CD_MO_155' then  YYYYYMM=201111;
if _name_= 'MAX_ELG_CD_MO_156' then  YYYYYMM=201112;
if _name_= 'MAX_ELG_CD_MO_157' then  YYYYYMM=201201;
if _name_= 'MAX_ELG_CD_MO_158' then  YYYYYMM=201202;
if _name_= 'MAX_ELG_CD_MO_159' then  YYYYYMM=201203;
if _name_= 'MAX_ELG_CD_MO_160' then  YYYYYMM=201204;
if _name_= 'MAX_ELG_CD_MO_161' then  YYYYYMM=201205;
if _name_= 'MAX_ELG_CD_MO_162' then  YYYYYMM=201206;
if _name_= 'MAX_ELG_CD_MO_163' then  YYYYYMM=201207;
if _name_= 'MAX_ELG_CD_MO_164' then  YYYYYMM=201208;
if _name_= 'MAX_ELG_CD_MO_165' then  YYYYYMM=201209;
if _name_= 'MAX_ELG_CD_MO_166' then  YYYYYMM=201210;
if _name_= 'MAX_ELG_CD_MO_167' then  YYYYYMM=201211;
if _name_= 'MAX_ELG_CD_MO_168' then  YYYYYMM=201212;
if _name_= 'MAX_ELG_CD_MO_169' then  YYYYYMM=201301;
if _name_= 'MAX_ELG_CD_MO_170' then  YYYYYMM=201302;
if _name_= 'MAX_ELG_CD_MO_171' then  YYYYYMM=201303;
if _name_= 'MAX_ELG_CD_MO_172' then  YYYYYMM=201304;
if _name_= 'MAX_ELG_CD_MO_173' then  YYYYYMM=201305;
if _name_= 'MAX_ELG_CD_MO_174' then  YYYYYMM=201306;
if _name_= 'MAX_ELG_CD_MO_175' then  YYYYYMM=201307;
if _name_= 'MAX_ELG_CD_MO_176' then  YYYYYMM=201308;
if _name_= 'MAX_ELG_CD_MO_177' then  YYYYYMM=201309;
if _name_= 'MAX_ELG_CD_MO_178' then  YYYYYMM=201310;
if _name_= 'MAX_ELG_CD_MO_179' then  YYYYYMM=201311;
if _name_= 'MAX_ELG_CD_MO_180' then  YYYYYMM=201312;
run;


%let start = 199812;

/* List all participants: dataset "list" 
this is an adapted snipit from my sas code collection (by other people)*/

proc sql;
	create table list as
	select distinct bene_id, state_cd 
	from workingfile_t1;
	
%let start = 199812;
data list_month;
	set list;
	format Enroll_month YYMMn6.;
	do i=1 to 181;
		Enroll_month = intnx('month',
		input("&start",YYMMn6.),
		i-1,"b");
		output;
	end;
	drop i;
run;

/*flag prior enrollment month-*/

data have_flag;
	set workingfile_t1;
	by bene_id state_cd yyyyymm;
	if first.bene_id then flag_Enroll_month = 1;
	else flag_Enroll_month = .;
	enroll_month=input(put(yyyyymm),6.);
	format enroll_month yymmn6.;
run;

/*first observed enrollment month*/
data enroll;
	set have_flag;
	where flag_Enroll_month=1;
	drop flag_Enroll_month;
	rename Enroll_month = initial_month;
run;

/*assess enrollment status within month-*/

data have_pre_all;
	merge list_month (in=x) have_flag (in=y);
	by bene_id state_cd Enroll_month;
	if y=1 then flag_enroll = 1;
	else flag_enroll = 0;
	run;

/*merge enrollment with calandar-*/
data have_all;
	merge have_pre_all enroll;
	by bene_id state_cd;
run;


/* Enroll_Continous_months */

data have_continous;
	set have_all (where=(Enroll_month>initial_month));
	by bene_id State_cd flag_enroll notsorted;
	if first.flag_enroll then count = 0;
	count + 1;
run;

proc sql;
	create table case_enrollment_list as
	select bene_id, state_cd, max(count) as Enroll_Continous_months
	from have_continous
	where flag_enroll=1
	group by bene_id, state_cd;
quit;

data key_file (keep=bene_id
enrollment_episode_start 
enrollment_episode_end);
set have_continous;
by Pbene_id;
if bene_id>1 then do; 
if flag_enroll=1 and count=1 then enrollment_episode_start= Enroll_month;
if flag_enroll=0 and count=1 then enrollment_episode_end=   Enroll_month;
if last.bene_id and enrollment_episode_start= Enroll_month then enrollment_episode_end = enrollment_episode_start;
retain 
enrollment_episode_start 
enrollment_episode_end;
format 
enrollment_episode_start YYMMn6.
enrollment_episode_end YYMMn6.;
end;
run;

proc sort data=key_file nodup;
by 
bene_id 
enrollment_episode_start 
enrollment_episode_end;
run;

data observation_period(where=(flag<1));
set key_file(keep=person_id );
if enrollment_episode_end < enrollment_episode_start then flag=1;
person_id=bene_id;
observation_period_start_date=enrollment_episode_start;
observation_period_end_date=enrollment_episode_end;
run;
/*observation_period*/



