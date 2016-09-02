# Create config file using all forward (R1) and reverse (R2) reads:
echo '[general]
project_name = KI_Platy
researcher_email = dclaar@uvic.ca
input_directory = /Volumes/Seagate_Blue/JuliaBaum_DanielleClaar-32138116_all/
output_directory = /Volumes/Seagate_Blue/JuliaBaum_DanielleClaar-32138116_all/merged_reads

[files]
pair_1 = ' > config_KI_Platy_all

cat R1.txt >> config_KI_Platy_all

echo '
pair_2 = ' >> config_KI_Platy_all

cat R2.txt >> config_KI_Platy_all

# Make script to merge Illumina pairs
#ls ../JuliaBaum_DanielleClaar-32138116/ > sampleids_temp.txt
#cut -c-11 sampleids_temp.txt > sampleids.txt

echo "some file content" > /path/to/outputfile


for i in sample#
echo 'merge-illumina-pairs -o' > merge_illumina_pairs.sh
sample# >> merge_illumina_pairs.sh
echo '--enforce-Q30-check config_KI_Platy_all --marker-gene-stringent --retain-only-overlap  --min-overlap-size 100 --compute-qual-dicts' >> merge_illumina_pairs.sh



merge-illumina-pairs -o KI15aFSYM139,KI15aFSYM052,KI14FSYM161,KI14FSYM164,KI14FSYM192,KI14FSYM205,KI14FSYM213,KI14FSYM234,KI14FSYM242,KI14FSYM245,KI14FSYM273,KI14FSYM278,KI14FSYM285,KI14FSYM311,KI14FSYM339,KI14FSYM371,KI14FSYM410,KI14FSYM412,KI14FSYM435,KI14FSYM439,KI14FSYM442,KI14FSYM449,KI14FSYM473,KI14FSYM479,KI14FSYM492,KI14FSYM497,KI15bFSYM002,KI15bFSYM015,KI15bFSYM057,KI15bFSYM080,KI15bFSYM138,KI15bFSYM147,KI15bFSYM204,KI15bFSYM207,KI15bFSYM216,KI15bFSYM219,KI15bFSYM284,KI15bFSYM306,KI15bFSYM316,KI15cFSYM014,KI15cFSYM018,KI15cFSYM083,KI15cFSYM129,KI15cFSYM171,KI15cFSYM178,KI15cFSYM180,KI15cFSYM200,KI15cFSYM254,KI15cFSYM283,KI15cFSYM372,KI15cFSYM427,KI15cFSYM428,KI15cFSYM506,KI15cSSYM001,KI15cSSYM003,KI15cSSYM006,KI15cWSYM001,KI15cWSYM011,KI15cWSYM012,KI15cWSYM002,KI15cWSYM025,KI15cWSYM026,KI15cWSYM027,KI15cWSYM028,KI15cWSYM029,KI15cWSYM003,KI15cWSYM030,KI15cWSYM031,KI15cWSYM032,KI15cWSYM033,KI15cWSYM034,KI15cWSYM035,KI15cWSYM036,KI15cWSYM037,KI15cWSYM038,KI15cWSYM039,KI15cWSYM004,KI15cWSYM040,KI16aFSYM021,KI16aFSYM035,KI16aFSYM039,KI16aFSYM043,KI16aFSYM044,KI16aFSYM047,KI16aFSYM057,KI16aFSYM062,KI16aFSYM083,KI16aFSYM226,KI16aFSYM233,KI16aFSYM261,KI15cFSYM101,KI15cFSYM104,KI15cFSYM112,KI15cFSYM114,KI15cFSYM116,KI15cFSYM125,KI15cFSYM126,KI15cFSYM142,KI15cFSYM144,KI15cFSYM145,KI15cFSYM159,KI15cFSYM161,KI15cFSYM190,KI15cFSYM209,KI15cFSYM216,KI15cFSYM225,KI15cFSYM025,KI15cFSYM252,KI15cFSYM253,KI15cFSYM261,KI15cFSYM265,KI15cFSYM273,KI15cFSYM275,KI15cFSYM276,KI15cFSYM277,KI15cFSYM278,KI15cFSYM003,KI15cFSYM317,KI15cFSYM328,KI15cFSYM336,KI15cFSYM338,KI15cFSYM345,KI15cFSYM348,KI15cFSYM350,KI15cFSYM365,KI15cFSYM373,KI15cFSYM385,KI15cFSYM399,KI15cFSYM404,KI15cFSYM411,KI15cFSYM415,KI15cFSYM432,KI15cFSYM434,KI15cFSYM435,KI15cFSYM436,KI15cFSYM439,KI15cFSYM448,KI15cFSYM455,KI15cFSYM460,KI15cFSYM461,KI15cFSYM468,KI15cFSYM472,KI15cFSYM474,KI15cFSYM488,KI15cFSYM493,KI15cFSYM494,KI15cFSYM496,KI15cFSYM499,KI15cFSYM502,KI15cFSYM503,KI15cFSYM504,KI15cFSYM507,KI15cFSYM509,KI15cFSYM519,KI15cFSYM063,KI15cFSYM064,KI15cFSYM065,KI15cFSYM072,KI15cFSYM077,KI15cFSYM082,KI15cFSYM090,KI15cFSYM094,KI16aFSYM001,KI16aFSYM010,KI16aFSYM101,KI16aFSYM104,KI16aFSYM107,KI16aFSYM108,KI16aFSYM011,KI16aFSYM113,KI16aFSYM116,KI16aFSYM118,KI16aFSYM119,KI16aFSYM120,KI16aFSYM121,KI16aFSYM123,KI16aFSYM013,KI16aFSYM151,KI16aFSYM153,KI16aFSYM155,KI16aFSYM158,KI16aFSYM159,KI16aFSYM160,KI16aFSYM164,KI16aFSYM017,KI16aFSYM018,KI16aFSYM190,KI16aFSYM196,KI16aFSYM020,KI16aFSYM212,KI16aFSYM214,KI16aFSYM215,KI16aFSYM217,KI16aFSYM220,KI16aFSYM221,KI16aFSYM223,KI16aFSYM225,KI16aFSYM229,KI16aFSYM231,KI16aFSYM235,KI16aFSYM237,KI16aFSYM238,KI16aFSYM239,KI16aFSYM024,KI15bFSYM255,KI16aFSYM256,KI16aFSYM259,KI16aFSYM260,KI16aFSYM262,KI15bFSYM263,KI16aFSYM264,KI16aFSYM265,KI16aFSYM266,KI16aFSYM268,KI16aFSYM027,KI16aFSYM272,KI16aFSYM273,KI16aFSYM274,KI15bFSYM279,KI15bFSYM282,KI16aFSYM029,KI16aFSYM033,KI15bFSYM346,KI15bFSYM352,KI15bFSYM353,KI16aFSYM038,KI16aFSYM004,KI16aFSYM040,KI16aFSYM048,KI16aFSYM049,KI16aFSYM054,KI16aFSYM056,KI16aFSYM058,KI16aFSYM059,KI16aFSYM006,KI16aFSYM066,KI16aFSYM069,KI16aFSYM070,KI16aFSYM072,KI16aFSYM073,KI16aFSYM074,KI16aFSYM076,KI16aFSYM077,KI16aFSYM080,KI16aFSYM082,KI16aFSYM086,KI16aFSYM087,KI16aFSYM088,KI16aFSYM089,KI16aFSYM009,KI16aFSYM091,KI16aFSYM094,KI16aFSYM095,KI16aFSYM096,KI15bFSYM102,KI15bFSYM131,KI15bFSYM132,KI15bFSYM140,KI15bFSYM164,KI15bFSYM167,KI15bFSYM179,KI15bFSYM184,KI15bFSYM187,KI15bFSYM188,KI15bFSYM019,KI15bFSYM191,KI15bFSYM194,KI15bFSYM197,KI15bFSYM020,KI15bFSYM200,KI15bFSYM205,KI15bFSYM213,KI15bFSYM214,KI15bFSYM218,KI15bFSYM023,KI15bFSYM231,KI15bFSYM239,KI15bFSYM243,KI15bFSYM245,KI15bFSYM247,KI15bFSYM249,KI15bFSYM259,KI15bFSYM026,KI15bFSYM283,KI15bFSYM288,KI15bFSYM293,KI15bFSYM296,KI15bFSYM303,KI15bFSYM309,KI15bFSYM313,KI15bFSYM320,KI15bFSYM038,KI15bFSYM050,KI15bFSYM065,KI15bFSYM068,KI15bFSYM069,KI15bFSYM084,KI15bFSYM088,KI15bFSYM092,KI15bFSYM277,KI15bFSYM340,KI14FSYM100,KI14FSYM127,KI14FSYM146,KI14FSYM148,KI14FSYM157,KI14FSYM019,KI14FSYM223,KI14FSYM231,KI14FSYM248,KI14FSYM249,KI14FSYM253,KI14FSYM258,KI14FSYM266,KI14FSYM271,KI14FSYM289,KI14FSYM029,KI14FSYM298,KI14FSYM299,KI14FSYM306,KI14FSYM328,KI14FSYM334,KI14FSYM338,KI14FSYM341,KI14FSYM351,KI14FSYM352,KI14FSYM353,KI14FSYM373,KI14FSYM379,KI14FSYM386,KI14FSYM388,KI14FSYM389,KI14FSYM004,KI14FSYM400,KI14FSYM402,KI14FSYM447,KI14FSYM454,KI14FSYM455,KI14FSYM461,KI14FSYM466,KI14FSYM470,KI14FSYM482,KI14FSYM486,KI14FSYM487,KI14FSYM500,KI14FSYM504,KI14FSYM062,KI14FSYM089,KI14FSYM099,KI15aFSYM103,KI15aFSYM105,KI15aFSYM113,KI15aFSYM116,KI15aFSYM123,KI15aFSYM014,KI15aFSYM145,KI15aFSYM015,KI15aFSYM150,KI15aFSYM156,KI15aFSYM160,KI15aFSYM163,KI15aFSYM002,KI15aFSYM032,KI15aFSYM037,KI15aFSYM041,KI15aFSYM043,KI15aFSYM046,KI15aFSYM057,KI15aFSYM067,KI15aFSYM072,KI15aFSYM077,KI15aFSYM081,KI15aFSYM096,KI15aFSYM097,KI15cWSYM010,KI15cWSYM009,KI15cSSYM026,KI15cSSYM028,KI15cSSYM029,KI15cSSYM033,KI15cSSYM034,KI15cSSYM036 --enforce-Q30-check config_KI_Platy_KI14FSYM099 --marker-gene-stringent --retain-only-overlap  --min-overlap-size 100 --compute-qual-dicts


#Check how many config files were created
ls | wc -l

