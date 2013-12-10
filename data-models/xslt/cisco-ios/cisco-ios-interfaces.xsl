<?xml version="1.0"?>

<!-- Program name: cisco-ios-interfaces.xsl

Copyright © 2013 by Ladislav Lhotka, CZ.NIC <lhotka@nic.cz>

Translates NETCONF "get-config" replies to Cisco IOS configuration.
This stylesheet handles the "ietf-interfaces" YANG module.

==

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

<stylesheet
    xmlns="http://www.w3.org/1999/XSL/Transform"
    xmlns:if="urn:ietf:params:xml:ns:yang:ietf-interfaces"
    xmlns:ip="urn:ietf:params:xml:ns:yang:ietf-ip"
    xmlns:rt="urn:ietf:params:xml:ns:yang:ietf-routing"
    xmlns:v6ur="urn:ietf:params:xml:ns:yang:ietf-ipv6-unicast-routing"
    version="1.0">

  <template match="if:interfaces">
    <apply-templates select="if:interface"/>
  </template>

  <template match="if:interface">
    <apply-templates select="if:name"/>
    <apply-templates select="if:description"/>
    <apply-templates select="if:enabled"/>
    <if test="translate(if:name,'n','N') != 'Null0'">
      <apply-templates select="ip:ipv4"/>
      <apply-templates select="ip:ipv6"/>
      <apply-templates
	  select="//rt:routing//rt:interface[rt:name =
		  current()/if:name]/v6ur:ipv6-router-advertisements"/>
    </if>
    <value-of select="concat('!',$NL)"/>
  </template>

  <template match="if:description">
    <value-of select="concat(' description ',
			  normalize-space(.),$NL)"/>
  </template>

  <template match="if:name">
    <text>interface </text>
    <call-template name="VALUE-NL"/>
  </template>

  <template match="if:enabled">
    <if test=".='false'">
      <value-of select="concat(' shutdown',$NL)"/>
    </if>
  </template>

</stylesheet>
