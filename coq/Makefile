all: Makefile _CoqProject
	$(COQBIN)coq_makefile -f _CoqProject -o CoqMakefile
	$(MAKE) --no-print-directory -f CoqMakefile

clean:
	rm -rf CoqMakefile CoqMakefile.conf .*.aux *.vo* *.glob */*.vo* */*.glob */*/*/*.vo* */*/*/*.glob */*/*/*/*.vo* */*/*/*/*.glob

