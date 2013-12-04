<?xml version="1.0"?>

<!-- Program name: cisco-ios.xsl

Copyright © 2012 by Ladislav Lhotka, CZ.NIC <lhotka@nic.cz>

Translates NETCONF "get-config" replies to Cisco IOS configuration.
Supported YANG modules: ietf-system, ietf-interfaces, ietf-ip,
ietf-routing, ietf-ipv4-unicast-routing, ietf-ipv6-unicast-routing.

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-->

<stylesheet xmlns="http://www.w3.org/1999/XSL/Transform"
		xmlns:nc="urn:ietf:params:xml:ns:netconf:base:1.0"
		xmlns:if="urn:ietf:params:xml:ns:yang:ietf-interfaces"
		xmlns:sys="urn:ietf:params:xml:ns:yang:ietf-system"
		xmlns:rt="urn:ietf:params:xml:ns:yang:ietf-routing"
		version="1.0">

  <output method="text"/>
  <strip-space elements="*"/>

  <param name="ios-version">12.4</param>
  <variable name="ipv4-rtbl"
	    select="//nc:data/rt:routing-tables/rt:routing-table
		    [rt:name = //nc:data/rt:routing/rt:router/
		    rt:main-routing-tables/rt:main-routing-table[
		    rt:address-family = 'ipv4' and rt:safi = 
		    'nlri-unicast']/rt:name"/>

  <variable name="ipv6-rtbl"
	    select="//nc:data/rt:routing-tables/rt:routing-table
		    [rt:name = //nc:data/rt:routing/rt:router/
		    rt:main-routing-tables/rt:main-routing-table[
		    rt:address-family = 'ipv6' and rt:safi = 
		    'nlri-unicast']/rt:name"/>

  <include href="../common-templates.xsl"/>
  <include href="cisco-ios-system.xsl"/>
  <include href="cisco-ios-interfaces.xsl"/>
  <include href="cisco-ios-ip.xsl"/>
  <include href="cisco-ios-routing.xsl"/>
  <include href="cisco-ios-ipv4-unicast-routing.xsl"/>
  <include href="cisco-ios-ipv6-unicast-routing.xsl"/>

  <template name="sanity-check">
    <if test="count(//nc:data/rt:routing/rt:router) &gt; 1">
      <message terminate="yes">
	<text>ERROR: Multiple logical routers are not supported.</text>
      </message>
    </if>
  </template>

  <template match="/">
    <call-template name="sanity-check"/>
    <value-of select="concat('!',$NL)"/>
    <text>! Created from NETCONF get-config reply</text>
    <value-of select="$NL"/>
    <value-of select="concat('!',$NL)"/>
    <value-of select="concat('version ', $ios-version, $NL)"/>
    <value-of select="concat('!',$NL)"/>
    <apply-templates select="descendant::nc:data"/>
    <value-of select="concat('!',$NL)"/>
    <text>end</text>
    <value-of select="$NL"/>
  </template>

  <template match="nc:data">
    <apply-templates select="sys:system/sys:name"/>
    <value-of select="concat('!',$NL)"/>
    <apply-templates select="sys:system/sys:clock"/>
    <value-of select="concat('!',$NL)"/>
    <value-of select="concat('aaa new-model',$NL)"/>
    <apply-templates select="sys:system/sys:authentication"/>
    <value-of select="concat('!',$NL)"/>
    <apply-templates select="sys:system/sys:dns"/>
    <value-of select="concat('!',$NL)"/>
    <if test="count($ipv6-rtbl) &gt; 0">
      <value-of select="concat('ipv6 unicast-routing', $NL)"/>
    </if>
    <value-of select="concat('!',$NL)"/>
    <apply-templates select="sys:system/sys:authentication/sys:user"/>
    <value-of select="concat('!',$NL)"/>
    <apply-templates select="if:interfaces"/>
    <apply-templates
	select="rt:routing/rt:router/
		rt:routing-protocols/rt:routing-protocol"/>
    <value-of select="concat('!',$NL)"/>
    <apply-templates select="sys:system/sys:location"/>
    <apply-templates select="sys:system/sys:contact"/>
    <apply-templates select="sys:system/sys:radius"/>
    <value-of select="concat('!',$NL)"/>
    <apply-templates select="sys:system/sys:ntp"/>
  </template>

  <template match="*">
    <message terminate="no">
      <value-of
	  select="concat('Node ', name(), ' not translated.')"/>
    </message>
  </template>

</stylesheet>
