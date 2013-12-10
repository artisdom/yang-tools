<?xml version="1.0"?>

<!-- Program name: bird.xsl

Copyright © 2013 by Ladislav Lhotka, CZ.NIC <lhotka@nic.cz>

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
    version="1.0">
  <output method="text"/>
  <strip-space elements="*"/>

  <!-- Address family of the configuration -->
  <param name="ip-version" select="4"/>
  <variable name="address-family"
	    select="concat('v', $ip-version, 'ur:ipv', $ip-version, '-unicast')"/>
  <!-- Amount of indentation added for each BIRD hierarchy level. -->
  <param name="indent-step" select="2"/>
  <!-- Maximum line length -->
  <param name="line-length" select="70"/>

  <include href="../common-templates.xsl"/>
  <include href="bird-radv.xsl"/>
  <include href="bird-static.xsl"/>

  <template name="close-block">
    <param name="lev" select="0"/>
    <param name="semi"/>
    <call-template name="indent">
      <with-param name="lev" select="$lev"/>
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

  <template name="repeat-string">
    <!-- Insert `count` number of copies of `string`  -->
    <param name="count"/>
    <param name="string"/>
    <choose>
      <when test="not($count) or not($string)"/>
      <when test="$count = 1">
	<value-of select="$string"/>
      </when>
      <otherwise>
	<if test="$count mod 2">
	  <value-of select="$string"/>
	</if>
	<call-template name="repeat-string">
	  <with-param name="count" select="floor($count div 2)"/>
	  <with-param name="string" select="concat($string,$string)"/>
	</call-template> 
      </otherwise>
    </choose>
  </template>

  <template name="indent">
    <!-- Insert number of spaces corresponding to indentation at `lev` -->
    <param name="lev" select="1"/>
    <call-template name="repeat-string">
      <with-param name="count" select="$lev * $indent-step"/>
      <with-param name="string" select="' '"/>
    </call-template>
  </template>

  <template name="statement">
    <param name="lev"/>
    <param name="kw"/>
    <param name="arg"/>
    <param name="quoted"/>
    <call-template name="indent">
      <with-param name="lev" select="$lev"/>
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
    <param name="lev" select="0"/>
    <param name="kw"/>
    <param name="arg" select="."/>
    <param name="quoted" select="0"/>
    <param name="dflt">__NO_DEFAULT__</param>
    <if test="$arg != $dflt">
      <call-template name="statement">
	<with-param name="lev" select="$lev"/>
	<with-param name="kw" select="$kw"/>
	<with-param name="arg" select="$arg"/>
	<with-param name="quoted" select="$quoted"/>
      </call-template>
      <value-of select="concat(';', $NL)"/>
    </if>
  </template>

  <template name="stmt-block">
    <param name="lev" select="0"/>
    <param name="kw"/>
    <param name="arg"/>
    <param name="quoted" select="0"/>
    <call-template name="statement">
      <with-param name="lev" select="$lev"/>
      <with-param name="kw" select="$kw"/>
      <with-param name="arg" select="$arg"/>
      <with-param name="quoted" select="$quoted"/>
    </call-template>
    <value-of select="concat(' {', $NL)"/>
  </template>

  <template name="switch">
    <param name="lev" select="1"/>
    <param name="kw"/>
    <param name="dflt">yes</param>
    <call-template name="stmt-leaf">
      <with-param name="lev" select="$lev"/>
      <with-param name="kw" select="$kw"/>
      <with-param name="arg">
	<call-template name="yes-no">
	  <with-param name="bool" select="."/>
	</call-template>
      </with-param>
      <with-param name="dflt" select="$dflt"/>
    </call-template>
  </template>

  <template name="arg-check">
    <if test="$ip-version != 4 and $ip-version != 6">
      <message terminate="yes">
	<text>Bad 'ip-version' parameter: </text>
	<value-of select="$ip-version"/>
      </message>
    </if>
  </template>

  <template name="common-protocol-pars">
    <apply-templates
	select="rt:description|rt:enabled|
		rt:connected-ribs/rt:connected-rib"/>
  </template>

  <!-- Root element -->

  <template match="/">
    <call-template name="arg-check"/>
    <apply-templates select="//nc:*/rt:routing"/>
  </template>

  <template match="rt:routing">
    <variable name="rtr"
	      select="rt:routing-instance[rt:enabled = 'true' and
		      rt:default-ribs/rt:default-rib
		      [rt:address-family = $address-family]]"/>
    <if test="count($rtr) = 0">
      <message terminate="yes">
	<value-of
	    select="concat('No enabled routing instance found for IP version ',
		    $ip-version)"/>
      </message>
    </if>
    <if test="count($rtr) > 1">
      <message terminate="no">
	<value-of
	    select="concat('Multiple routing instances found, using &quot;',
		    $rtr[1]/rt:name, '&quot;')"/>
      </message>
    </if>
    <text>/*
 *        Configuration for IPv</text>
    <value-of select="$ip-version"/>
    <text> BIRD daemon
 * (generated automatically from XML configuration)
 */

</text>
    <apply-templates
	select="rt:ribs/rt:rib[rt:name !=
		$rtr[1]/rt:default-ribs/rt:default-rib
		[rt:address-family = $address-family]/rt:name]"/>
    <apply-templates select="$rtr[1]"/>
    <apply-templates select="rt:route-filters/rt:route-filter"/>
  </template>

  <template match="rt:rib">
    <call-template name="stmt-leaf">
      <with-param name="kw">table</with-param>
      <with-param name="arg" select="rt:name"/>
    </call-template>
  </template>

  <template match="rt:routing-instance">
    <apply-templates select="rt:router-id"/>
    <apply-templates select="rt:interfaces"/>
    <apply-templates select="rt:routing-protocols/rt:routing-protocol"/>
  </template>

  <template match="rt:router-id">
    <call-template name="stmt-leaf">
      <with-param name="kw">router id</with-param>
    </call-template>
  </template>

  <template match="rt:interfaces">
    <call-template name="stmt-block">
      <with-param name="kw">protocol</with-param>
      <with-param name="arg">direct</with-param>
    </call-template>
    <call-template name="stmt-leaf">
      <with-param name="lev" select="1"/>
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
    <if test="$ip-version = 6 and rt:interface/
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
  </template>

  <!-- Common protocol parameters -->

  <template match="rt:description">
    <call-template name="stmt-leaf">
      <with-param name="lev" select="1"/>
      <with-param name="kw">description</with-param>
      <with-param name="quoted" select="1"/>
    </call-template>
  </template>

  <template match="rt:enabled">
    <call-template name="stmt-leaf">
      <with-param name="lev" select="1"/>
      <with-param name="kw">disabled</with-param>
      <with-param name="arg">
	<choose>
	  <when test=". = 'false'">yes</when>
	  <otherwise>no</otherwise>
	</choose>
      </with-param>
      <with-param name="dflt">no</with-param>
    </call-template>
  </template>

  <template match="rt:connected-rib">
    <variable name="rtbl"
	      select="//nc:*/rt:routing/rt:ribs/
		      rt:rib[rt:name = current()/rt:name]"/>
    <if test="$rtbl/rt:address-family = $address-family">
      <call-template name="stmt-leaf">
	<with-param name="lev" select="1"/>
	<with-param name="kw">table</with-param>
	<with-param name="arg" select="rt:name"/>
	<with-param
	    name="dflt"
	    select="../../../../rt:default-ribs/rt:default-rib[
		    rt:address-family = $address-family]/rt:name"/>
      </call-template>
      <apply-templates select="rt:import-filter"/>
    </if>
  </template>

  <template match="rt:import-filter">
    <call-template name="stmt-leaf">
      <with-param name="lev" select="1"/>
      <with-param name="kw">import filter</with-param>
    </call-template>
  </template>

  <template match="rt:route-filter">
    <call-template name="stmt-block">
      <with-param name="kw">filter</with-param>
      <with-param name="arg" select="rt:name"/>
    </call-template>
    <apply-templates select="rt:type"/>
    <call-template name="close-block"/>
  </template>

  <template
      match="rt:route-filter/rt:type[. = 'rt:allow-all-route-filter']">
    <call-template name="stmt-leaf">
      <with-param name="lev" select="1"/>
      <with-param name="kw">accept</with-param>
      <with-param name="arg">allow all</with-param>
      <with-param name="quoted" select="1"/>
    </call-template>
  </template>

  <template
      match="rt:route-filter/rt:type[. = 'rt:deny-all-route-filter']">
    <call-template name="stmt-leaf">
      <with-param name="lev" select="1"/>
      <with-param name="kw">reject</with-param>
      <with-param name="arg">deny all</with-param>
      <with-param name="quoted" select="1"/>
    </call-template>
  </template>

  <!-- Without a specific template, issue a warning. -->
  <template match="*">
    <message terminate="no">
      <value-of select="concat('Element ', name(), ' not handled.')"/>
    </message>
  </template>

</stylesheet>
