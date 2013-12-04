<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:nc="urn:ietf:params:xml:ns:netconf:base:1.0"
		xmlns:bird="http://www.nic.cz/ns/bird"
		xmlns:v4ur="urn:ietf:params:xml:ns:yang:ietf-ipv4-unicast-routing"
		xmlns:v6ur="urn:ietf:params:xml:ns:yang:ietf-ipv6-unicast-routing"
		xmlns:if="urn:ietf:params:xml:ns:yang:ietf-interfaces"
		xmlns:ip="urn:ietf:params:xml:ns:yang:ietf-ip"
		xmlns:rt="urn:ietf:params:xml:ns:yang:ietf-routing"
		version="1.0">
  <xsl:output method="text"/>

  <xsl:template match="/">
    <xsl:apply-templates select=".//rt:router/bird:bird-config/bird:address-family"/>
  </xsl:template>

  <xsl:template match="bird:address-family">
    <xsl:value-of select="current()/../../rt:interfaces/rt:interface/rt:name"/>
    <xsl:text>
</xsl:text>
  </xsl:template>

</xsl:stylesheet>
