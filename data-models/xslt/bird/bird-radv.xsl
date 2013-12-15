<?xml version="1.0"?>

<!-- Program name: bird-radv.xsl

Copyright © 2013 by Ladislav Lhotka, CZ.NIC <lhotka@nic.cz>

Translates NETCONF "get-config" replies to BIRD configuration.
This stylesheet handles the RAdv protocol (IPv6 router advertisements).

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
    xmlns:v6ur="urn:ietf:params:xml:ns:yang:ietf-ipv6-unicast-routing"
    version="1.0">

  <template match="rt:interface" mode="radv">
    <call-template name="stmt-block">
      <with-param name="level" select="1"/>
      <with-param name="kw">interface</with-param>
      <with-param name="arg" select="rt:name"/>
      <with-param name="quoted" select="1"/>
    </call-template>
    <apply-templates select="v6ur:ipv6-router-advertisements"/>
    <call-template name="close-block">
      <with-param name="level" select="1"/>
    </call-template>
    <call-template name="close-block"/>
  </template>

  <template match="v6ur:ipv6-router-advertisements">
    <apply-templates
	select="v6ur:max-rtr-adv-interval|v6ur:managed-flag|
		v6ur:other-config-flag|v6ur:link-mtu|v6ur:reachable-time|
		v6ur:retrans-timer|v6ur:cur-hop-limit"/>
    <call-template name="stmt-leaf">
      <with-param name="level" select="2"/>
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
      <with-param name="level" select="2"/>
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
      <with-param name="level" select="2"/>
      <with-param name="kw">max ra interval</with-param>
      <with-param name="dflt" select="600"/>
    </call-template>
  </template>

  <template match="v6ur:managed-flag">
    <call-template name="switch">
      <with-param name="level" select="2"/>
      <with-param name="kw">managed</with-param>
      <with-param name="dflt">no</with-param>
    </call-template>
  </template>

  <template match="v6ur:other-config-flag">
    <call-template name="switch">
      <with-param name="level" select="2"/>
      <with-param name="kw">other config</with-param>
      <with-param name="dflt">no</with-param>
    </call-template>
  </template>

  <template match="v6ur:link-mtu">
    <call-template name="stmt-leaf">
      <with-param name="level" select="2"/>
      <with-param name="kw">link mtu</with-param>
      <with-param name="dflt" select="0"/>
    </call-template>
  </template>

  <template match="v6ur:reachable-time">
    <call-template name="stmt-leaf">
      <with-param name="level" select="2"/>
      <with-param name="kw">reachable time</with-param>
      <with-param name="dflt" select="0"/>
    </call-template>
  </template>

  <template match="v6ur:retrans-timer">
    <call-template name="stmt-leaf">
      <with-param name="level" select="2"/>
      <with-param name="kw">retrans timer</with-param>
      <with-param name="dflt" select="0"/>
    </call-template>
  </template>

  <template match="v6ur:cur-hop-limit">
    <call-template name="stmt-leaf">
      <with-param name="level" select="2"/>
      <with-param name="kw">current hop limit</with-param>
      <with-param name="dflt" select="64"/>
    </call-template>
  </template>

  <template match="v6ur:ipv6-router-advertisements/v6ur:prefix-list/v6ur:prefix">
    <call-template name="stmt-block">
      <with-param name="level" select="2"/>
      <with-param name="kw">prefix</with-param>
      <with-param name="arg" select="v6ur:prefix-spec"/>
    </call-template>
    <apply-templates
	select="v6ur:no-advertise|v6ur:valid-lifetime|v6ur:on-link-flag|
		v6ur:preferred-lifetime|v6ur:autonomous-flag"/>
    <call-template name="close-block">
      <with-param name="level" select="2"/>
    </call-template>
  </template>

  <template match="v6ur:no-advertise">
    <call-template name="stmt-leaf">
      <with-param name="level" select="3"/>
      <with-param name="kw">skip</with-param>
      <with-param name="arg"/>
    </call-template>
  </template>

  <template match="v6ur:valid-lifetime">
    <call-template name="stmt-leaf">
      <with-param name="level" select="3"/>
      <with-param name="kw">valid lifetime</with-param>
      <with-param name="dflt" select="86400"/>
    </call-template>
  </template>

  <template match="v6ur:on-link-flag">
    <call-template name="switch">
      <with-param name="level" select="3"/>
      <with-param name="kw">onlink</with-param>
    </call-template>
  </template>

  <template match="v6ur:autonomous-flag">
    <call-template name="switch">
      <with-param name="level" select="3"/>
      <with-param name="kw">autonomous</with-param>
    </call-template>
  </template>

  <template match="v6ur:preferred-lifetime">
    <call-template name="stmt-leaf">
      <with-param name="level" select="3"/>
      <with-param name="kw">preferred lifetime</with-param>
      <with-param name="dflt" select="14400"/>
    </call-template>
  </template>
</stylesheet>