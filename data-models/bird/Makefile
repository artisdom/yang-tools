SUBMOD = bird-global bird-device bird-kernel bird-nest bird-ospf
ALL = bird.yang $(addsuffix .yang, $(SUBMOD)) 
Y2YXSL = ../../xslt/yin2yang.xsl
XSLPARS = --stringparam date $(shell date +%F)
.PHONY: all clean

all:  $(ALL)
	pyang bird.yang

clean:
	rm -f *.yang

%.yang: %.yinx
	xsltproc --output $@ $(XSLPARS) $(Y2YXSL) $<

