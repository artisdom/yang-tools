HELLO = hello-server.xml
SUBMODULES = bird-nest bird-features bird-kernel bird-rip bird-ospf
YSUB = $(addsuffix .yang, $(SUBMODULES)) 
IPV ?= 4
TARGET ?= config
BASE ?= sample
BN = $(BASE)-$(TARGET)
SCHEMAS = $(BN).rng $(BN).sch $(BN).dsrl
INSTANCE = $(BN).xml
Y2YXSL = $(PYANG_XSLT_DIR)/yin2yang.xsl
XSLPARS =
Y2DOPTS = -t $(TARGET) -b $(BASE)
.PHONY: all clean validate

all: $(SCHEMAS) $(BN).rnc bird$(IPV).conf

clean:
	rm -f *.yang $(SCHEMAS) *-gdefs.rng *-defaults.xsl \
	  *.rnc model.* bird[46].conf

validate: $(INSTANCE) $(SCHEMAS)
	yang2dsdl -j -s $(Y2DOPTS) -v $<

$(SCHEMAS): $(HELLO) bird.yang $(YSUB)
	yang2dsdl $(Y2DOPTS) -L $< 

%.rnc: %.rng
	trang -I rng -O rnc $< $@

%.yang: %.yinx
	xsltproc --output $@ $(XSLPARS) $(Y2YXSL) $<

%-defaults.xsl: %.dsrl
	xsltproc --output $@ $(PYANG_XSLT_DIR)/dsrl2xslt.xsl $<

bird$(IPV).conf: $(BN)-defaults.xsl $(INSTANCE)
	xsltproc $^ | xsltproc --stringparam ip-version $(IPV) --output $@ bird.xsl -

model.dsdl: $(HELLO) bird.yang $(YSUB)
	pyang -L -f dsdl --dsdl-no-documentation $< \
	| xmllint --format - > $@

model.tree: $(HELLO) bird.yang $(YSUB)
	pyang -L -f tree -o $@ $<

