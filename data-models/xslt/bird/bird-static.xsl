<?xml version="1.0"?>

<!-- Program name: bird-static.xsl

Copyright © 2013 by Ladislav Lhotka, CZ.NIC <lhotka@nic.cz>

Translates NETCONF "get-config" replies to BIRD configuration.
This stylesheet handles the static protocol.

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
    xmlns:rt="urn:ietf:params:xml:ns:yang:ietf-routing"
    xmlns:v4ur="urn:ietf:params:xml:ns:yang:ietf-ipv4-unicast-routing"
    xmlns:v6ur="urn:ietf:params:xml:ns:yang:ietf-ipv6-unicast-routing"
    version="1.0">

  <template match="rt:routing-protocol[rt:type = 'rt:static']">
    <call-template name="stmt-block">
      <with-param name="kw">protocol static</with-param>
      <with-param name="arg" select="rt:name"/>
    </call-template>
    <call-template name="common-protocol-pars"/>
    <apply-templates select="rt:static-routes"/>
    <call-template name="close-block"/>
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
	<apply-templates
	    select="v4ur:special-nexthop|v4ur:outgoing-interface|
		    v4ur:gateway[not(../v4ur:outgoing-interface)]|
		    v4ur:nexthop"/>
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
	<apply-templates
	    select="v6ur:special-nexthop|v6ur:outgoing-interface|
		    v6ur:gateway[not(../v6ur:outgoing-interface)]|
		    v6ur:nexthop"/>
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

  <template match="v4ur:special-nexthop|v6ur:special-nexthop">
    <value-of select="."/>
  </template>

  <template
      match="v4ur:gateway|v6ur:gateway|
	     v4ur:outgoing-interface|v6ur:outgoing-interface">
    <value-of select="concat('via ', .)"/>
  </template>

  <template name="multipath">
    <if test="position()=1">
      <text>multipath&#xA;</text>
    </if>
  </template>

  <template match="v4ur:nexthop">
    <call-template name="multipath"/>
    <apply-templates
	select="v4ur:address[not(../v4ur:outgoing-interface)]|
		v4ur:outgoing-interface"/>
    <apply-templates select="v4ur:weight"/>
    <value-of select="$NL"/>
  </template>

  <template match="v6ur:nexthop">
    <call-template name="multipath"/>
    <apply-templates
	select="v6ur:address[not(../v6ur:outgoing-interface)]|
		v6ur:outgoing-interface"/>
    <apply-templates select="v6ur:weight"/>
    <value-of select="$NL"/>
  </template>

  <template
      match="v4ur:nexthop/v4ur:address|v6ur:nexthop/v6ur:address">
    <call-template name="statement">
      <with-param name="lev" select="2"/>
      <with-param name="kw">via</with-param>
      <with-param name="arg" select="value-of(.)"/>
      <with-param name="quoted" select="0"/>
    </call-template>
  </template>

  <template
      match="v4ur:nexthop/v4ur:outgoing-interface|
	     v6ur:nexthop/v6ur:outgoing-interface">
    <call-template name="statement">
      <with-param name="lev" select="2"/>
      <with-param name="kw">via</with-param>
      <with-param name="arg" select="value-of(.)"/>
      <with-param name="quoted" select="1"/>
    </call-template>
  </template>

  <template match="v4ur:weight|v6ur:weight">
    <value-of select="concat(' weight ',.)"/>
  </template>

</stylesheet>