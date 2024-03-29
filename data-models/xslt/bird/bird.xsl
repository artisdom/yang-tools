<?xml version="1.0"?>

<!-- Program name: bird.xsl

Copyright © 2014 by Ladislav Lhotka, CZ.NIC <lhotka@nic.cz>

Translates XML instance configuration to BIRD config file.

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
    xmlns:nc="urn:ietf:params:xml:ns:netconf:base:1.0"
    xmlns:v4ur="urn:ietf:params:xml:ns:yang:ietf-ipv4-unicast-routing"
    xmlns:v6ur="urn:ietf:params:xml:ns:yang:ietf-ipv6-unicast-routing"
    xmlns:if="urn:ietf:params:xml:ns:yang:ietf-interfaces"
    xmlns:ip="urn:ietf:params:xml:ns:yang:ietf-ip"
    xmlns:rt="urn:ietf:params:xml:ns:yang:ietf-routing"
    xmlns:bird="http://www.nic.cz/ns/bird"
    version="1.0">
  <output method="text"/>
  <strip-space elements="*"/>

  <!-- Address family of the configuration -->
  <param name="ip-version" select="4"/>
  <variable name="address-family"
	    select="concat('v', $ip-version, 'ur:ipv', $ip-version,
		    '-unicast')"/>
  <variable name="rt-root" select="//nc:*/rt:routing"/>
  <variable name="rt-root" select="//nc:*/if:interfaces"/>
  <!-- Name of the routing instance to process (first enabled entry of
       the routing-instance list is used by default) -->
  <param name="inst-name"
	 select="$rt-root/rt:routing-instance
		 [rt:type=concat('bird:bird-ipv',$ip-version)][1]/rt:name"/>

  <include href="../common-templates.xsl"/>
  <include href="bird-global.xsl"/>
  <include href="bird-nest.xsl"/>
  <include href="bird-radv.xsl"/>
  <include href="bird-static.xsl"/>
  <include href="bird-device.xsl"/>
  <include href="bird-pipe.xsl"/>

  <!-- Named templates -->
  
  <template name="close-block">
    <param name="level" select="0"/>
    <param name="semi"/>
    <call-template name="indent">
      <with-param name="level" select="$level"/>
    </call-template>
    <value-of select="concat('}', $semi, $NL)"/>
  </template>

  <template name="yes-no">
    <param name="bool"/>
    <choose>
      <when test="$bool = 'true'">yes</when>
      <otherwise>no</otherwise>
    </choose>
  </template>

  <template name="value-or-default">
    <param name="nodeset"/>
    <param name="dflt"/>
    <choose>
      <when test="$nodeset">
	<apply-templates select="$nodeset" mode="value"/>
      </when>
      <otherwise>
	<value-of select="$dflt"/>
      </otherwise>
    </choose>
  </template>

  <template name="statement">
    <param name="level"/>
    <param name="kw"/>
    <param name="arg"/>
    <param name="quoted"/>
    <call-template name="indent">
      <with-param name="level" select="$level"/>
    </call-template>
    <value-of select="$kw"/>
    <variable name="quote">
      <choose>
	<when test="$quoted">&quot;</when>
	<otherwise/>
      </choose>
    </variable>
    <if test="$arg">
      <value-of select="concat(' ', $quote, $arg, $quote)"/>
    </if>
  </template>

  <template name="stmt-leaf">
    <param name="level" select="0"/>
    <param name="kw"/>
    <param name="arg" select="."/>
    <param name="quoted" select="0"/>
    <param name="dflt">__NO_DEFAULT__</param>
    <if test="$arg != $dflt">
      <call-template name="statement">
	<with-param name="level" select="$level"/>
	<with-param name="kw" select="$kw"/>
	<with-param name="arg" select="$arg"/>
	<with-param name="quoted" select="$quoted"/>
      </call-template>
      <value-of select="concat(';', $NL)"/>
    </if>
  </template>

  <template name="stmt-block">
    <param name="level" select="0"/>
    <param name="kw"/>
    <param name="arg"/>
    <param name="quoted" select="0"/>
    <call-template name="statement">
      <with-param name="level" select="$level"/>
      <with-param name="kw" select="$kw"/>
      <with-param name="arg" select="$arg"/>
      <with-param name="quoted" select="$quoted"/>
    </call-template>
    <value-of select="concat(' {', $NL)"/>
  </template>

  <template name="switch">
    <param name="level" select="1"/>
    <param name="kw"/>
    <param name="dflt">yes</param>
    <call-template name="stmt-leaf">
      <with-param name="level" select="$level"/>
      <with-param name="kw" select="$kw"/>
      <with-param name="arg">
	<call-template name="yes-no">
	  <with-param name="bool" select="."/>
	</call-template>
      </with-param>
      <with-param name="dflt" select="$dflt"/>
    </call-template>
  </template>

  <template name="rt-name">
    <!-- Use "master" if the parameter is default table name. -->
    <param name="tname" select="rt:rib-name"/>
    <choose>
      <when test="$tname=$default-rib">master</when>
      <otherwise>
	<value-of select="$tname"/>
      </otherwise>
    </choose>
  </template>

  <template name="common-protocol-pars">
    <apply-templates
	select="rt:description|rt:enabled|
		rt:connected-ribs/rt:connected-rib"/>
  </template>

  <template name="process-instance">
    <param name="inst"/>
  </template>

  <!-- Root element -->

  <template match="/">
    <if test="$ip-version != 4 and $ip-version != 6">
      <message terminate="yes">
	<text>Bad 'ip-version' parameter: </text>
	<value-of select="$ip-version"/>
      </message>
    </if>
    <apply-templates select="$rt-root"/>
  </template>

  <template match="rt:routing">
    <variable name="inst" select="rt:routing-instance[rt:name=$inst-name]"/>
    <if test="count($inst) = 0">
      <message terminate="yes">
	<value-of
	    select="concat('No IPv', $ip-version, ' routing instance found.')"/>
      </message>
    </if>
    <text>/*
 *        Configuration for IPv</text>
    <value-of select="$ip-version"/>
    <text> BIRD daemon
 * (generated automatically from XML configuration)
 */
</text>
    <apply-templates select="$inst"/>
  </template>

  <template match="rt:routing-instance">
    <apply-templates select="bird:bird-config"/>
    <apply-templates select="rt:router-id"/>
    <if test="$ip-version = 6 and $if-root/if:interface[if:name = ">
      <apply-templates select="rt:interfaces" mode="radv"/>
    </if>
	      v6ur:ipv6-router-advertisements/
	      v6ur:send-advertisements = 'true'">
      <call-template name="stmt-block">
	<with-param name="kw">protocol</with-param>
	<with-param name="arg">radv</with-param>
      </call-template>
      <apply-templates
	  select="rt:interface[v6ur:ipv6-router-advertisements/
		  v6ur:send-advertisements = 'true']"
	  mode="radv"/>
    </if>
    <apply-templates select="rt:routing-protocols/rt:routing-protocol"/>
    <apply-templates
	select="rt:ribs/rt:rib[rt:address-family = $address-family]"/>
  </template>

  <template match="rt:interfaces">
    <call-template name="stmt-block">
      <with-param name="kw">protocol</with-param>
      <with-param name="kw">device</with-param>
    </call-template>
    <apply-templates select="bird:scan-time"/>
    <call-template name="close-block"/>
    <if test="$if-root/if:interface[if:name = rt:interface]/
	      ip:ipv6/v6ur:ipv6-router-advertisements">
      <call-template name="stmt-block">
	<with-param name="kw">protocol</with-param>
	<with-param name="kw">radv</with-param>
      </call-template>
      <apply-templates select="
    </if>
  </template>

  <template match="rt:interfaces">
    <if test="rt:interface">
    <call-template name="stmt-block">
      <with-param name="kw">protocol</with-param>
      <with-param name="arg">direct</with-param>
    </call-template>
    <call-template name="stmt-leaf">
      <with-param name="level" select="1"/>
      <with-param name="kw">interface</with-param>
      <with-param name="arg">
	<for-each select="rt:interface">
	  <value-of
	      select="concat('&quot;', rt:name, '&quot;')"/>
	  <if test="position() != last()">, </if>
	</for-each>
      </with-param>
    </call-template>
    <call-template name="close-block"/>
      
    </if>
  </template>
  
  <template match="rt:rib">
    <call-template name="stmt-leaf">
      <with-param name="kw">table</with-param>
      <with-param name="arg" select="rt:name"/>
    </call-template>
  </template>

  <template match="rt:router-id">
    <call-template name="stmt-leaf">
      <with-param name="kw">router id</with-param>
    </call-template>
  </template>

  <template match="bird:scan-time">
    <call-template name="stmt-leaf">
      <with-param name="level" select="1"/>
      <with-param name="kw">scan time</with-param>
      <with-param name="arg" select="."/>
    </call-template>
  </template>
  
  <template match="rt:interfaces" 

  <!-- Without a specific template, issue a warning. -->
  <template match="*">
    <message terminate="no">
      <value-of select="concat('Element ', name(), ' not handled.')"/>
    </message>
  </template>

</stylesheet>
