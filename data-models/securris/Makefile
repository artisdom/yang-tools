MODULES = securris
ymod = $(addsuffix .yang, $(MODULES))
TARGET ?= config
BASE ?= sample
Y2Y = ~/Projects/YANG/yang-tools/xslt/yin2yang.xsl
PYANGDIR = /Users/lhotka/Projects/pyang
bn = $(BASE)-$(TARGET)
xsltdir = $(PYANGDIR)/xslt
schemas = $(bn).rng $(BASE)-gdefs-config.rng $(bn)-dsrl.xsl $(bn)-sch.xsl
tgt = --stringparam target $(TARGET)
rngparms =  $(tgt) --stringparam basename $(BASE) --stringparam schema-dir \
	$(PYANGDIR)/schema $(xsltdir)/gen-relaxng.xsl
Y2DOPTS = -t $(TARGET) -b $(BASE)
.PHONY = clean

all: $(schemas) $(bn).rnc model.tree model.xsl

%.yang: %.yinx
	@xsltproc --output $@ $(Y2Y) $<

$(bn).xml: model.jtox $(bn).json
	@json2xml -t $(TARGET) -o $@ $^

model.jtox: $(ymod)
	@pyang -o $@ -f jtox $^

model.xsl: $(ymod)
	@pyang -o $@ -f jsonxsl $^

model.tree: $(ymod)
	@pyang -o $@ -f tree $^

$(bn).dsdl: $(ymod)
	@pyang -f dsdl -o $@ --dsdl-no-documentation --dsdl-no-dublin-core $^

$(bn).rng: $(bn).dsdl
	@xsltproc --output $@ $(rngparms) $<

$(BASE)-gdefs-config.rng: $(bn).dsdl
	@xsltproc --output $@ --stringparam gdefs-only 1 $(rngparms) $<

$(bn).rnc: $(bn).rng $(BASE)-gdefs-config.rng
	@trang -I rng -O rnc $< $@

$(bn)-dsrl.xsl: $(bn).dsdl
	@xsltproc $(tgt) $(xsltdir)/gen-dsrl.xsl $< | \
	xsltproc --output $@ $(xsltdir)/dsrl2xslt.xsl -

$(bn)-sch.xsl: $(bn).dsdl
	@xsltproc $(tgt) $(xsltdir)/gen-schematron.xsl $< | \
	xsltproc $(xsltdir)/iso_abstract_expand.xsl - | \
        xsltproc -o $@ $(xsltdir)/iso_svrl_for_xslt1.xsl -

validate: $(bn).xml $(schemas)
	@jing $(bn).rng $<
	@xsltproc $(bn)-dsrl.xsl $< | xsltproc $(bn)-sch.xsl - | \
	xsltproc $(xsltdir)/svrl2text.xsl -

clean:
	@rm -f $(schemas) $(bn).xml *.dsdl model.* *.rnc
