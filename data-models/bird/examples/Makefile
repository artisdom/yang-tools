HELLO = hello-server.xml
CONFFILES = bird4.conf bird6.conf
SUBMOD = device
YMODS = ../bird.yang $(addprefix ../bird-,$(addsuffix .yang,$(SUBMOD)))
TARGET ?= get-config-reply
BASE ?= test
SCHEMAS = $(BASE)-$(TARGET).rng $(BASE)-$(TARGET).sch $(BASE)-$(TARGET).dsrl
INSTANCE = $(BASE)-$(TARGET).xml
Y2DOPTS = -t $(TARGET) -b $(BASE)
DSRL2XSL = $(PYANG_XSLT_DIR)/dsrl2xslt.xsl
BIRD_XSL = ../../xslt/bird/bird.xsl
DFLTSHEET = $(BASE)-$(TARGET)-defaults.xsl
.PHONY: all clean rnc validate

all: $(SCHEMAS) $(CONFFILES) $(BASE)-$(TARGET).rnc

%-defaults.xsl: %.dsrl
	xsltproc --output $@ $(DSRL2XSL) $<

bird%.conf: $(INSTANCE) $(DFLTSHEET)
	xsltproc $(DFLTSHEET) $< | \
	    xsltproc --output $@ --stringparam ip-version $* $(BIRD_XSL) -

$(SCHEMAS): $(HELLO) $(YMODS)
	yang2dsdl -L $(Y2DOPTS) $<

validate: $(INSTANCE) $(SCHEMAS)
	yang2dsdl -j -s $(Y2DOPTS) -v $<

model.dsdl: $(HELLO) $(YMODS)
	pyang -L -f dsdl --dsdl-no-documentation $< \
	    | xmllint --format - > $@

model.tree: $(HELLO) $(YMODS)
	pyang -L -f tree -o $@ $<

%.rnc: %.rng
	trang -I rng -O rnc $< $@
 
clean:
	rm -f $(SCHEMAS) $(CONFFILES) $(DFLTSHEET) model.*\
	    *-gdefs*.rng *.rnc
