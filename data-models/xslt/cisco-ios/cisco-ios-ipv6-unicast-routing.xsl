<?xml version="1.0"?>

<!-- Program name: cisco-ios-ipv6-unicast-routing.xsl

Copyright © 2013 by Ladislav Lhotka, CZ.NIC <lhotka@nic.cz>

Translates NETCONF "get-config" replies to Cisco IOS configuration.
This stylesheet handles the "ietf-ipv6-unicast-routing" YANG module.

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

  <template match="v6ur:ipv6-router-advertisements
		       [v6ur:send-advertisements = 'false']">
    <value-of select="concat(' ipv6 nd ra suppress ', $NL)"/>
  </template>

  <template match="v6ur:ipv6-router-advertisements">
    <if test="v6ur:link-mtu != 0">
      <value-of select="concat(' ipv6 mtu ', v6ur:link-mtu, $NL)"/>
    </if>
    <if test="v6ur:max-rtr-adv-interval != 200 or
		  v6ur:min-rtr-adv-interval != 150">
      <value-of
	  select="concat(' ipv6 nd ra interval ',v6ur:max-rtr-adv-interval)"/>
      <if test="v6ur:min-rtr-adv-interval !=
		    0.75*v6ur:max-rtr-adv-interval">
	<value-of select="concat(' ',v6ur:min-rtr-adv-interval)"/>
      </if>
      <value-of select="$NL"/>
    </if>
    <if test="v6ur:managed-flag = 'true'">
      <value-of select="concat(' ipv6 nd managed-config-flag', $NL)"/>
    </if>
    <if test="v6ur:other-config-flag = 'true'">
      <value-of select="concat(' ipv6 nd other-config-flag', $NL)"/>
    </if>
    <if test="v6ur:reachable-time != 0">
      <value-of select="concat(' ipv6 nd reachable-time ',
			    v6ur:reachable-time, $NL)"/>
    </if>
    <if test="v6ur:retrans-timer != 0">
      <value-of select="concat(' ipv6 nd ns-interval ',
			    v6ur:retrans-timer, $NL)"/>
    </if>
    <if test="not(v6ur:default-lifetime) and
		  v6ur:max-rtr-adv-interval != 600">
      <value-of select="concat(' ipv6 nd ra lifetime ',
			    3 * v6ur:max-rtr-adv-interval, $NL)"/>
    </if>
    <apply-templates select="v6ur:default-lifetime[. != 1800]|
				 v6ur:prefix-list/v6ur:prefix"/>
  </template>

  <template match="v6ur:default-lifetime">
    <value-of select="concat(' ipv6 nd ra lifetime ', ., $NL)"/>
  </template>

  <template
      match="v6ur:ipv6-router-advertisements/
	     v6ur:prefix-list/v6ur:prefix">
    <value-of select="concat(' ipv6 nd prefix ',
			  v6ur:prefix-spec)"/>
    <choose>
      <when test="v6ur:no-advertise">
	<text>no-advertise</text>
      </when>
      <otherwise>
	<if test="v6ur:valid-lifetime != 2592000 or
		      v6ur:preferred-lifetime != 604800 or
		      v6ur:onlink-flag = 'false' or
		      v6ur:autonomous-flag = 'false'">
	  <value-of select="concat(' ', v6ur:valid-lifetime,
				' ', v6ur:preferred-lifetime)"/>
	  <if test="v6ur:onlink-flag = 'false'">
	    <text> no-onlink</text>
	  </if>
	  <if test="v6ur:autonomous-flag = 'false'">
	    <text> no-autoconfig</text>
	  </if>
	</if>
      </otherwise>
    </choose>
    <value-of select="$NL"/>
  </template>

  <template match="v6ur:route">
    <value-of select="concat('ipv6 route ', v6ur:dest-prefix)"/>
    <apply-templates select="v6ur:outgoing-interface"/>
    <apply-templates select="v6ur:next-hop"/>
    <value-of select="$NL"/>
  </template>

</stylesheet>