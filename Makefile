RSYNC=/usr/bin/rsync

sync: get put

put:
	$(RSYNC) -avuz --exclude 'Makefile*' --exclude '*~' . hu@136.187.45.248:public_html

# for backup purpose
get:
	$(RSYNC) -avuz hu@136.187.45.248:public_html/ .
