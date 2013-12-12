HELLO = hello-server.xml
BIRD = bird4.conf bird6.conf
TARGET ?= get-config-reply
BASE ?= test
SCHEMAS = $(BASE)-$(TARGET).rng $(BASE)-$(TARGET).sch $(BASE)-$(TARGET).dsrl
INSTANCE = $(BASE)-$(TARGET).xml
Y2DOPTS = -t $(TARGET) -b $(BASE)
DSRL2XSL = $(PYANG_XSLT_DIR)/dsrl2xslt.xsl
BIRD_XSL = ../bird.xsl
DFLTSHEET = $(BASE)-$(TARGET)-defaults.xsl
IOS = test-cisco-ios.cfg
.PHONY: all clean rnc validate

all: $(SCHEMAS) $(BIRD) $(BASE)-$(TARGET).rnc

%-defaults.xsl: %.dsrl
	xsltproc --output $@ $(DSRL2XSL) $<

bird%.conf: $(INSTANCE)
	xsltproc --output $@ --stringparam ip-version $* $(BIRD_XSL) $<

$(SCHEMAS): $(HELLO)
	yang2dsdl -L $(Y2DOPTS) $(HELLO)

validate: $(INSTANCE) $(SCHEMAS)
	yang2dsdl -j -s $(Y2DOPTS) -v $<

model.dsdl: $(YMODULES)
	pyang -L -f dsdl --dsdl-no-documentation $(HELLO) \
	    | xmllint --format - > $@

model.tree: $(YMODULES)
	pyang -L -f tree -o $@ $(HELLO)

%.rnc: %.rng
	trang -I rng -O rnc $< $@
 
clean:
	rm -f $(SCHEMAS) $(BIRD) $(DFLTSHEET) model.*\
	    *-gdefs*.rng *.rnc
