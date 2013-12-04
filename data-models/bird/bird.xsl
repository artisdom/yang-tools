<?xml version="1.0"?>

<!-- Program name: bird.xsl

Copyright © 2012 by Ladislav Lhotka, CZ.NIC <lhotka@nic.cz>

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
    xmlns:bird="http://www.nic.cz/ns/bird"
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
  <variable name="afn" select="concat('ipv', $ip-version)"/>
  <variable name="safi">nlri-unicast</variable>
  <!-- Amount of indentation added for each BIRD hierarchy level. -->
  <param name="indent-step" select="2"/>
  <!-- Maximum line length -->
  <param name="line-length" select="70"/>

  <variable name="root" select="//nc:data"/>
  <variable name="nl" select="'&#xA;'"/>

  <key name="if-location" match="if:interface/if:location" use="../if:name"/>

  <template name="close-block">
    <param name="lev" select="0"/>
    <param name="semi"/>
    <call-template name="indent">
      <with-param name="lev" select="$lev"/>
    </call-template>
    <value-of select="concat('}', $semi, $nl)"/>
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
      <value-of select="concat(';', $nl)"/>
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
    <value-of select="concat(' {', $nl)"/>
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
		rt:connected-routing-tables/rt:connected-routing-table"/>
  </template>

  <!-- Root element -->

  <template match="/">
    <call-template name="arg-check"/>
    <apply-templates select="$root/rt:routing"/>
  </template>

  <template match="rt:routing">
    <variable name="rtr"
	      select="rt:router[not(rt:enabled = 'false') and
		      rt:main-routing-tables/rt:main-routing-table
		      [rt:address-family = $afn and rt:safi = $safi]]"/>
    <if test="count($rtr) = 0">
      <message terminate="yes">
	<value-of
	    select="concat('No enabled router instance found for IP version ',
		    $ip-version)"/>
      </message>
    </if>
    <if test="count($rtr) > 1">
      <message terminate="no">
	<value-of
	    select="concat('Multiple router instances founds, using &quot;',
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
	select="rt:routing-tables/rt:routing-table[rt:name !=
		$rtr[1]/rt:main-routing-tables/rt:main-routing-table
		[rt:address-family = $afn and rt:safi =
		$safi]/rt:name]"/>
    <apply-templates select="$rtr[1]"/>
    <apply-templates select="rt:route-filters/rt:route-filter"/>
  </template>

  <template match="rt:routing-table">
    <if test="rt:address-family = $afn and rt:safi = $safi">
      <call-template name="stmt-leaf">
	<with-param name="kw">table</with-param>
	<with-param name="arg" select="rt:name"/>
      </call-template>
    </if>
  </template>

  <template match="rt:router">
<!-- <apply-templates select="bird:bird-config"/> -->
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
<!-- 
    <call-template name="statement">
      <with-param name="kw">protocol</with-param>
      <with-param name="arg">device</with-param>
      <with-param name="sub" select="1"/>
    </call-template>
    <apply-templates select="bird:scan-time"/>
    <apply-templates select="rt:interface/rt:name"
		     mode="device"/>
    <call-template name="close-block"/>
-->
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
	      select="concat('&quot;', key('if-location', rt:name),
		      '&quot;')"/>
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

  <template match="rt:interface" mode="radv">
    <call-template name="stmt-block">
      <with-param name="lev" select="1"/>
      <with-param name="kw">interface</with-param>
      <with-param name="arg" select="key('if-location', rt:name)"/>
      <with-param name="quoted" select="1"/>
    </call-template>
    <apply-templates select="v6ur:ipv6-router-advertisements"/>
    <call-template name="close-block">
      <with-param name="lev" select="1"/>
    </call-template>
    <call-template name="close-block"/>
  </template>

  <template match="v6ur:ipv6-router-advertisements">
    <apply-templates
	select="v6ur:max-rtr-adv-interval|v6ur:managed-flag|
		v6ur:other-config-flag|v6ur:link-mtu|v6ur:reachable-time|
		v6ur:retrans-timer|v6ur:cur-hop-limit"/>
    <call-template name="stmt-leaf">
      <with-param name="lev" select="2"/>
      <with-param name="kw">min ra interval</with-param>
      <with-param name="arg">
	<call-template name="value-or-default">
	  <with-param name="nodeset"
		      select="v6ur:min-rtr-adv-interval"/>
	  <with-param name="dflt">
	    <choose>
	      <when test="v6ur:max-rtr-adv-interval &gt;= 9">
		<value-of select="0.33 * v6ur:max-rtr-adv-interval"/>
	      </when>
	      <otherwise>
		<value-of select="0.75 * v6ur:max-rtr-adv-interval"/>
	      </otherwise>
	    </choose>
	  </with-param>
	</call-template>
      </with-param>
      <with-param name="dflt" select="0.33 * v6ur:max-rtr-adv-interval"/>
    </call-template>
    <call-template name="stmt-leaf">
      <with-param name="lev" select="2"/>
      <with-param name="kw">default lifetime</with-param>
      <with-param name="arg">
	<call-template name="value-or-default">
	  <with-param name="nodeset" select="v6ur:default-lifetime"/>
	  <with-param name="dflt" select="3 * v6ur:max-rtr-adv-interval"/>
	</call-template>
      </with-param>
      <with-param name="dflt" select="3 * v6ur:max-rtr-adv-interval"/>
    </call-template>
    <apply-templates select="v6ur:prefix-list/v6ur:prefix"/>
  </template>

  <template match="v6ur:max-rtr-adv-interval">
    <call-template name="stmt-leaf">
      <with-param name="lev" select="2"/>
      <with-param name="kw">max ra interval</with-param>
      <with-param name="dflt" select="600"/>
    </call-template>
  </template>

  <template match="v6ur:managed-flag">
    <call-template name="switch">
      <with-param name="lev" select="2"/>
      <with-param name="kw">managed</with-param>
      <with-param name="dflt">no</with-param>
    </call-template>
  </template>

  <template match="v6ur:other-config-flag">
    <call-template name="switch">
      <with-param name="lev" select="2"/>
      <with-param name="kw">other config</with-param>
      <with-param name="dflt">no</with-param>
    </call-template>
  </template>

  <template match="v6ur:link-mtu">
    <call-template name="stmt-leaf">
      <with-param name="lev" select="2"/>
      <with-param name="kw">link mtu</with-param>
      <with-param name="dflt" select="0"/>
    </call-template>
  </template>

  <template match="v6ur:reachable-time">
    <call-template name="stmt-leaf">
      <with-param name="lev" select="2"/>
      <with-param name="kw">reachable time</with-param>
      <with-param name="dflt" select="0"/>
    </call-template>
  </template>

  <template match="v6ur:retrans-timer">
    <call-template name="stmt-leaf">
      <with-param name="lev" select="2"/>
      <with-param name="kw">retrans timer</with-param>
      <with-param name="dflt" select="0"/>
    </call-template>
  </template>

  <template match="v6ur:cur-hop-limit">
    <call-template name="stmt-leaf">
      <with-param name="lev" select="2"/>
      <with-param name="kw">current hop limit</with-param>
      <with-param name="dflt" select="64"/>
    </call-template>
  </template>

  <template match="v6ur:ipv6-router-advertisements/v6ur:prefix-list/v6ur:prefix">
    <call-template name="stmt-block">
      <with-param name="lev" select="2"/>
      <with-param name="kw">prefix</with-param>
      <with-param name="arg" select="v6ur:prefix-spec"/>
    </call-template>
    <apply-templates
	select="v6ur:no-advertise|v6ur:valid-lifetime|v6ur:on-link-flag|
		v6ur:preferred-lifetime|v6ur:autonomous-flag"/>
    <call-template name="close-block">
      <with-param name="lev" select="2"/>
    </call-template>
  </template>

  <template match="v6ur:no-advertise">
    <call-template name="stmt-leaf">
      <with-param name="lev" select="3"/>
      <with-param name="kw">skip</with-param>
      <with-param name="arg"/>
    </call-template>
  </template>

  <template match="v6ur:valid-lifetime">
    <call-template name="stmt-leaf">
      <with-param name="lev" select="3"/>
      <with-param name="kw">valid lifetime</with-param>
      <with-param name="dflt" select="86400"/>
    </call-template>
  </template>

  <template match="v6ur:on-link-flag">
    <call-template name="switch">
      <with-param name="lev" select="3"/>
      <with-param name="kw">onlink</with-param>
    </call-template>
  </template>

  <template match="v6ur:autonomous-flag">
    <call-template name="switch">
      <with-param name="lev" select="3"/>
      <with-param name="kw">autonomous</with-param>
    </call-template>
  </template>

  <template match="v6ur:preferred-lifetime">
    <call-template name="stmt-leaf">
      <with-param name="lev" select="3"/>
      <with-param name="kw">preferred lifetime</with-param>
      <with-param name="dflt" select="14400"/>
    </call-template>
  </template>

  <template match="rt:routing-protocol[rt:type = 'rt:static']">
    <call-template name="stmt-block">
      <with-param name="kw">protocol static</with-param>
      <with-param name="arg" select="rt:name"/>
    </call-template>
    <call-template name="common-protocol-pars"/>
    <apply-templates select="rt:static-routes"/>
    <call-template name="close-block"/>
  </template>

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

  <template match="rt:connected-routing-table">
    <variable name="rtbl"
	      select="$root/rt:routing/rt:routing-tables/
		      rt:routing-table[rt:name = current()/rt:name]"/>
    <if test="$rtbl/rt:address-family = $afn and
	      $rtbl/rt:safi = $safi">
      <call-template name="stmt-leaf">
	<with-param name="lev" select="1"/>
	<with-param name="kw">table</with-param>
	<with-param name="arg" select="rt:name"/>
	<with-param
	    name="dflt"
	    select="../../../../rt:main-routing-tables/
		    rt:main-routing-table[rt:address-family = $afn
		    and rt:safi = $safi]/rt:name"/>
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

  <template match="rt:static-routes">
    <choose>
      <when test="$ip-version = 4">
	<apply-templates select="v4ur:ipv4/v4ur:route"/>
      </when>
      <otherwise>
	<apply-templates select="v6ur:ipv6/v6ur:route"/>
      </otherwise>
    </choose>
  </template>

  <template match="v4ur:route">
    <call-template name="stmt-leaf">
      <with-param name="lev" select="1"/>
      <with-param name="kw"
		  select="concat('route ', v4ur:dest-prefix)"/>
      <with-param name="arg">
	<choose>
	  <when test="v4ur:next-hop">
	    <value-of select="concat('via ', v4ur:next-hop)"/>
	  </when>
	  <when test="v4ur:outgoing-interface">
	    <value-of
		select="concat('via &quot;',
			v4ur:outgoing-interface, '&quot;')"/>
	  </when>
	  <otherwise>prohibit</otherwise>
	</choose>
      </with-param>
    </call-template>
  </template>

  <template match="v6ur:route">
    <call-template name="stmt-leaf">
      <with-param name="lev" select="1"/>
      <with-param name="kw"
		  select="concat('route ', v6ur:dest-prefix)"/>
      <with-param name="arg">
	<choose>
	  <when test="v6ur:next-hop">
	    <value-of select="concat('via ', v6ur:next-hop)"/>
	  </when>
	  <when test="v6ur:outgoing-interface">
	    <value-of
		select="concat('via &quot;',
			v6ur:outgoing-interface, '&quot;')"/>
	  </when>
	  <otherwise>prohibit</otherwise>
	</choose>
      </with-param>
    </call-template>
  </template>

  <template match="bird:bird-config">
    <apply-templates select="bird:logging"/>
  </template>

  <template match="bird:logging">
    <apply-templates select="bird:log"/>
  </template>

  <template match="bird:log">
    <call-template name="statement">
      <with-param name="kw">log</with-param>
      <with-param name="arg">
	<call-template name="value-or-default">
	  <with-param name="nodeset" select="bird:destination"/>
	  <with-param name="dflt">stderr</with-param>
	</call-template>
	<text> </text>
	<call-template name="value-or-default">
	  <with-param name="nodeset" select="bird:log-classes"/>
	  <with-param name="dflt">all</with-param>
	</call-template>
      </with-param>
    </call-template>
  </template>

  <template match="bird:log/bird:destination" mode="value">
    <choose>
      <when test="bird:stderr or not(bird:*)">stderr</when>
      <when test="bird:file">
	<value-of select="concat('&quot;', bird:file, '&quot;')"/>
      </when>
      <otherwise>
	<apply-templates/>
      </otherwise>
    </choose>
  </template>

  <template match="bird:syslog">
    <text>syslog</text>
    <if test="bird:host != 'localhost'">
      <value-of select="concat(' name ', bird:host)"/>
    </if>
  </template>

  <template match="bird:log-classes" mode="value">
    <for-each select="bird:class">
      <value-of select="."/>
      <if test="position() != last()">
	<text> </text>
      </if>
    </for-each>
  </template>

  <template match="bird:scan-time">
    <call-template name="statement">
      <with-param name="lev" select="1"/>
      <with-param name="kw">scan time</with-param>
      <with-param name="arg" select="."/>
    </call-template>
  </template>

  <template match="rt:interface/rt:name" mode="device">
    <choose>
      <when test="$ip-version = '4'">
	<apply-templates
	    select="//if:interfaces/if:interface[if:name =
		    current()]/ip:ipv4/ip:address
		    [bird:primary = 'true']"/>
      </when>
      <otherwise>
	<apply-templates
	    select="//if:interfaces/if:interface[if:name =
		    current()]/ip:ipv6/ip:address
		    [bird:primary = 'true']"/>
      </otherwise>
    </choose>
  </template>

  <template match="ip:address">
    <call-template name="statement">
      <with-param name="lev" select="1"/>
      <with-param name="kw">primary</with-param>
      <with-param
	  name="arg"
	  select="concat('&quot;', ../../if:name, '&quot; ', ip:ip)"/>
    </call-template>
  </template>

  <!-- Without a specific template, issue a warning. -->
  <template match="*">
    <message terminate="no">
      <value-of select="concat('Element ', name(), ' not handled.')"/>
    </message>
  </template>

</stylesheet>
