# $1 is path to poor_matches.txt, $2 is path to representative sequences

dir=$(dirname $1)

# Get corresponding sequences
awk 'NR==FNR {a[">"$1]; next} $1 in a {print; getline; print}' $dir/poor_matches.txt $2 > $dir/poor_matches_seqs.fasta

# Blast sequences to NCBI nr databse
/Users/Danielle/ncbi-blast-2.5.0+/bin/blastn -db nr -remote -query $dir/poor_matches_seqs.fasta -outfmt '6 qseqid qcovs stitle sseqid' -max_target_seqs 1 | sort -u -k1,1 > $dir/poormatch_IDs1.txt

join -a 1 $dir/poor_matches.txt $dir/poormatch_IDs1.txt > $dir/poormatch_IDs.txt

# Clean up
#rm $dir/poor_matches.txt
# rm $dir/poor_matches_seqs.fasta
# rm $dir/poormatch_IDs1.txt
