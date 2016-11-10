all: data/otus_97_bysample/97_otus_bysample.tsv data/otus_97_bysample/nw_tophits.tsv #this is the output at the end

data/otus_97_bysample/97_otus_bysample.tsv: data/fasta/combined_seqs_trimmed.fasta data/parallel_cluster.sh
	bash data/parallel_cluster.sh

# data/fasta/combined_seqs_trimmed.fasta: data/fastq_list.txt parallel_merge.sh