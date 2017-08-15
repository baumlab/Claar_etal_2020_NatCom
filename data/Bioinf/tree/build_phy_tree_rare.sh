#working from KI_Platy
#Originally from Moorea_Sym pipeline

#data/Bioinf/clust/all_rep_set_rep_set.fasta = seqs
#data/tax_table_rare.txt = ISDs
#column 9 is clade ID
#awk '{print $1}' data/tax_table_rare.txt = denovo name
#awk '{print $8}' data/tax_table_rare.txt = clade name

#subset ID lists by clade
awk '$9 ~ /^A/{ print $1; }' data/tax_table_rare.txt > data/Bioinf/tree/rare/A_tree_seq_rare.ids
awk '$9 ~ /^B/{ print $1; }' data/tax_table_rare.txt > data/Bioinf/tree/rare/B_tree_seq_rare.ids
awk '$9 ~ /^C/{ print $1; }' data/tax_table_rare.txt > data/Bioinf/tree/rare/C_tree_seq_rare.ids
awk '$9 ~ /^D/{ print $1; }' data/tax_table_rare.txt > data/Bioinf/tree/rare/D_tree_seq_rare.ids
awk '$9 ~ /^E/{ print $1; }' data/tax_table_rare.txt > data/Bioinf/tree/rare/E_tree_seq_rare.ids
awk '$9 ~ /^F/{ print $1; }' data/tax_table_rare.txt > data/Bioinf/tree/rare/F_tree_seq_rare.ids
awk '$9 ~ /^G/{ print $1; }' data/tax_table_rare.txt > data/Bioinf/tree/rare/G_tree_seq_rare.ids
awk '$9 ~ /^H/{ print $1; }' data/tax_table_rare.txt > data/Bioinf/tree/rare/H_tree_seq_rare.ids
awk '$9 ~ /^I/{ print $1; }' data/tax_table_rare.txt > data/Bioinf/tree/rare/I_tree_seq_rare.ids

#filter seqs by id list
filter_fasta.py -f data/Bioinf/clust/all_rep_set_rep_set.fasta -s data/Bioinf/tree/rare/A_tree_seq_rare.ids -o data/Bioinf/tree/rare/A_tree_seqs_rare.fasta
filter_fasta.py -f data/Bioinf/clust/all_rep_set_rep_set.fasta -s data/Bioinf/tree/rare/B_tree_seq_rare.ids -o data/Bioinf/tree/rare/B_tree_seqs_rare.fasta
filter_fasta.py -f data/Bioinf/clust/all_rep_set_rep_set.fasta -s data/Bioinf/tree/rare/C_tree_seq_rare.ids -o data/Bioinf/tree/rare/C_tree_seqs_rare.fasta
filter_fasta.py -f data/Bioinf/clust/all_rep_set_rep_set.fasta -s data/Bioinf/tree/rare/D_tree_seq_rare.ids -o data/Bioinf/tree/rare/D_tree_seqs_rare.fasta
filter_fasta.py -f data/Bioinf/clust/all_rep_set_rep_set.fasta -s data/Bioinf/tree/rare/E_tree_seq_rare.ids -o data/Bioinf/tree/rare/E_tree_seqs_rare.fasta
filter_fasta.py -f data/Bioinf/clust/all_rep_set_rep_set.fasta -s data/Bioinf/tree/rare/F_tree_seq_rare.ids -o data/Bioinf/tree/rare/F_tree_seqs_rare.fasta
filter_fasta.py -f data/Bioinf/clust/all_rep_set_rep_set.fasta -s data/Bioinf/tree/rare/G_tree_seq_rare.ids -o data/Bioinf/tree/rare/G_tree_seqs_rare.fasta
filter_fasta.py -f data/Bioinf/clust/all_rep_set_rep_set.fasta -s data/Bioinf/tree/rare/H_tree_seq_rare.ids -o data/Bioinf/tree/rare/H_tree_seqs_rare.fasta
filter_fasta.py -f data/Bioinf/clust/all_rep_set_rep_set.fasta -s data/Bioinf/tree/rare/I_tree_seq_rare.ids -o data/Bioinf/tree/rare/I_tree_seqs_rare.fasta

#check seq number
grep -c ">" data/Bioinf/tree/rare/A_tree_seqs_rare.fasta
grep -c ">" data/Bioinf/tree/rare/B_tree_seqs_rare.fasta
grep -c ">" data/Bioinf/tree/rare/C_tree_seqs_rare.fasta
grep -c ">" data/Bioinf/tree/rare/D_tree_seqs_rare.fasta
grep -c ">" data/Bioinf/tree/rare/E_tree_seqs_rare.fasta
grep -c ">" data/Bioinf/tree/rare/F_tree_seqs_rare.fasta
grep -c ">" data/Bioinf/tree/rare/G_tree_seqs_rare.fasta
grep -c ">" data/Bioinf/tree/rare/H_tree_seqs_rare.fasta
grep -c ">" data/Bioinf/tree/rare/I_tree_seqs_rare.fasta

#align fasta files for each clade
align_seqs.py -i data/Bioinf/tree/rare/A_tree_seqs_rare.fasta -m muscle -o data/Bioinf/tree/rare/
# align_seqs.py -i data/Bioinf/tree/rare/B_tree_seqs_rare.fasta -m muscle -o data/Bioinf/tree/rare/
align_seqs.py -i data/Bioinf/tree/rare/C_tree_seqs_rare.fasta -m muscle -o data/Bioinf/tree/rare/
align_seqs.py -i data/Bioinf/tree/rare/D_tree_seqs_rare.fasta -m muscle -o data/Bioinf/tree/rare/
# align_seqs.py -i data/Bioinf/tree/rare/E_tree_seqs_rare.fasta -m muscle -o data/Bioinf/tree/rare/
# align_seqs.py -i data/Bioinf/tree/rare/F_tree_seqs_rare.fasta -m muscle -o data/Bioinf/tree/rare/
align_seqs.py -i data/Bioinf/tree/rare/G_tree_seqs_rare.fasta -m muscle -o data/Bioinf/tree/rare/
# align_seqs.py -i data/Bioinf/tree/rare/H_tree_seqs_rare.fasta -m muscle -o data/Bioinf/tree/rare/
# align_seqs.py -i data/Bioinf/tree/rare/I_tree_seqs_rare.fasta -m muscle -o data/Bioinf/tree/rare/

#remove extra header info from alignments
sed 's/ .*//' data/Bioinf/tree/rare/A_tree_seqs_rare_aligned.fasta > data/Bioinf/tree/rare/A_tree_seqs_rare_aligned_clean.fasta
# sed 's/ .*//' data/Bioinf/tree/rare/B_tree_seqs_rare_aligned.fasta > data/Bioinf/tree/rare/B_tree_seqs_rare_aligned_clean.fasta
sed 's/ .*//' data/Bioinf/tree/rare/C_tree_seqs_rare_aligned.fasta > data/Bioinf/tree/rare/C_tree_seqs_rare_aligned_clean.fasta
sed 's/ .*//' data/Bioinf/tree/rare/D_tree_seqs_rare_aligned.fasta > data/Bioinf/tree/rare/D_tree_seqs_rare_aligned_clean.fasta
# sed 's/ .*//' data/Bioinf/tree/rare/E_tree_seqs_rare_aligned.fasta > data/Bioinf/tree/rare/E_tree_seqs_rare_aligned_clean.fasta
# sed 's/ .*//' data/Bioinf/tree/rare/F_tree_seqs_rare_aligned.fasta > data/Bioinf/tree/rare/F_tree_seqs_rare_aligned_clean.fasta
sed 's/ .*//' data/Bioinf/tree/rare/G_tree_seqs_rare_aligned.fasta > data/Bioinf/tree/rare/G_tree_seqs_rare_aligned_clean.fasta
# sed 's/ .*//' data/Bioinf/tree/rare/H_tree_seqs_rare_aligned.fasta > data/Bioinf/tree/rare/H_tree_seqs_rare_aligned_clean.fasta
# sed 's/ .*//' data/Bioinf/tree/rare/I_tree_seqs_rare_aligned.fasta > data/Bioinf/tree/rare/I_tree_seqs_rare_aligned_clean.fasta
