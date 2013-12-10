<?xml version="1.0"?>

<!-- Program name: cisco-ios-system.xsl

Copyright © 2013 by Ladislav Lhotka, CZ.NIC <lhotka@nic.cz>

Translates NETCONF "get-config" replies to Cisco IOS configuration.
This stylesheet handles the "ietf-system" YANG module.

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

<stylesheet xmlns="http://www.w3.org/1999/XSL/Transform"
		xmlns:yin="urn:ietf:params:xml:ns:yang:yin:1"
		xmlns:sys="urn:ietf:params:xml:ns:yang:ietf-system"
		version="1.0">

  <param name="system-yin">ietf-system.yin</param>

  <template name="get-tz-data">
    <param name="tz">UTC</param>
    <variable
	name="tz-enum"
	select="document($system-yin)/yin:module/yin:typedef
		[@name='timezone-name']/yin:type/yin:enum[@name=$tz]"/>
    <if test="not($tz-enum)">
      <message terminate="yes">
	<value-of select="concat('Timezone acronym ',$tz,
			      ' not found')"/>
      </message>
    </if>
    <value-of select="concat($tz, ' ')"/>
    <variable
	name="offset"
	select="substring-after($tz-enum/yin:description/yin:text,'UTC')"/>
    <choose>
      <when test="string-length($offset)=0">0</when>
      <when test="contains($offset,':')">
	<value-of select="concat(substring-before($offset,':'),
			      ' ', substring-after($offset, ':'))"/>
      </when>
      <otherwise>
	<value-of select="$offset"/>
      </otherwise>
    </choose>
  </template>

  <template match="sys:name">
    <if test=". != ''">
      <text>hostname </text>
      <call-template name="VALUE-NL"/>
    </if>
  </template>

  <template match="sys:authentication">
    <if test="sys:user-authentication-order">
      <text>aaa authentication default </text>
      <for-each select="sys:user-authentication-order">
	<choose>
	  <when test=". = 'radius' or . = 'sys:radius'">
	    <text>group radius</text>
	  </when>
	  <when test=". = 'local-users' or . = 'sys:local-users'">
	    <text>local</text>
	  </when>
	</choose>
	<if test="position()!=last()">
	  <text> </text>
	</if>
      </for-each>
      <value-of select="$NL"/>
    </if>
  </template>

  <template match="sys:authentication/sys:user">
    <value-of select="concat('username ', sys:name)"/>
    <apply-templates select="sys:password"/>
  </template>

  <template match="sys:authentication//sys:password">
    <variable name="cdr" select="substring(.,2)"/>
    <variable name="typ" select="substring-before($cdr,'$')"/>
    <if test="$typ != 0">
      <message terminate="yes">
	<value-of select="concat('Cisco IOS ', $ios-version,
			      ' does not support password encryption ',
			      'type #', $typ,'.')"/>
      </message>
    </if>
    <value-of
	select="concat(' password 0 ',substring-after($cdr,'$'),$NL)"/>
  </template>

  <template match="sys:location">
    <if test=". != ''">
      <text>snmp-server location </text>
      <call-template name="VALUE-NL"/>
    </if>
  </template>

  <template match="sys:contact">
    <if test=". != ''">
      <text>snmp-server contact </text>
      <call-template name="VALUE-NL"/>
    </if>
  </template>

  <template match="sys:clock">
    <apply-templates select="sys:timezone-name"/>
  </template>

  <template match="sys:timezone-name">
    <text>clock timezone </text>
    <call-template name="get-tz-data">
      <with-param name="tz" select="."/>
    </call-template>
    <value-of select="$NL"/>
  </template>

  <template match="sys:dns">
    <apply-templates select="sys:server"/>
    <if test="sys:search">
      <text>ip domain list </text>
      <for-each select="sys:search">
	<value-of select="."/>
	<if test="position()!=last()">
	  <text> </text>
	</if>
      </for-each>
    </if>
    <value-of select="$NL"/>
    <apply-templates select="sys:options"/>
  </template>

  <template match="sys:dns/sys:server">
    <text>ip name-server </text>
    <call-template name="VALUE-NL"/>
  </template>

  <template match="sys:dns/sys:options">
    <apply-templates select="sys:timeout|sys:attempts"/>
  </template>

  <template match="sys:dns//sys:timeout">
    <if test=". != 3">
      <text>ip domain timeout </text>
      <call-template name="VALUE-NL"/>
    </if>
  </template>

  <template match="sys:dns//sys:attempts">
    <if test=". != 2">
      <text>ip domain retry </text>
      <call-template name="VALUE-NL"/>
    </if>
  </template>

  <template match="sys:ntp">
    <if test="sys:use-ntp = 'true'">
      <apply-templates select="sys:ntp-server"/>
    </if>
  </template>

  <template match="sys:ntp-server">
    <if test="sys:enabled = 'true'">
      <apply-templates select="sys:address"/>
    </if>
  </template>

  <template match="sys:ntp-server/sys:address">
    <text>ntp server </text>
    <call-template name="VALUE-NL"/>
  </template>

  <template match="sys:radius">
    <apply-templates/>
  </template>

  <template match="sys:radius/sys:server">
    <text>radius-server host </text>
    <value-of select="sys:address"/>
    <apply-templates select="sys:authentication-port"/>
    <apply-templates select="sys:shared-secret"/>
    <value-of select="$NL"/>
  </template>

  <template match="sys:radius//sys:authentication-port">
    <if test=". != 1645">
      <value-of select="concat(' auth-port ',.)"/>
    </if>
  </template>

  <template match="sys:radius//sys:shared-secret">
    <value-of select="concat(' key ',.)"/>
  </template>

  <template match="sys:radius/sys:options">
    <apply-templates/>
  </template>

  <template match="sys:radius//sys:timeout">
    <if test=". != 5">
      <value-of select="concat('radius-server timeout ',.,$NL)"/>
    </if>
  </template>

  <template match="sys:radius//sys:attempts">
    <if test=". != 3">
      <value-of select="concat('radius-server retransmits ',.,$NL)"/>
    </if>
  </template>

</stylesheet>