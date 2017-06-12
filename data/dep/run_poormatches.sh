R --vanilla < data/run_nw.R --args data/otus_97_bysample/all_rep_set_rep_set.fasta data/ITS2db_trimmed_derep.fasta

chmod u+x data/run_blast_poormatches.sh

bash data/run_blast_poormatches_quick.sh data/otus_97_bysample/poor_matches.txt data/otus_97_bysample/all_rep_set_rep_set.fasta
