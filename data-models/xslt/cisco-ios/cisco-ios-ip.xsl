<?xml version="1.0"?>

<!-- Program name: cisco-ios-ip.xsl

Copyright © 2013 by Ladislav Lhotka, CZ.NIC <lhotka@nic.cz>

Translates NETCONF "get-config" replies to Cisco IOS configuration.
This stylesheet handles the "ietf-routing" YANG module.

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
		xmlns:ip="urn:ietf:params:xml:ns:yang:ietf-ip"
		version="1.0">

  <template match="ip:ipv4|ip:ipv6">
    <if test="ip:enabled = 'true'">
      <apply-templates/>
    </if>
  </template>

  <template match="ip:ipv4/ip:enabled"/>

  <template match="ip:ipv6/ip:enabled">
    <if test="not(../ip:address or
		  ../ip:autoconf/ip:create-global-addresses = 'true')">
      <value-of select="concat(' ipv6 enable',$NL)"/>
    </if>
  </template>

  <template match="ip:ipv4/ip:address">
    <value-of select="concat(' ip address ', ip:ip, ' ')"/>
    <call-template name="QMASK">
      <with-param name="mlen">
	<value-of select="ip:prefix-length"/>
      </with-param>
    </call-template>
    <value-of select="$NL"/>
  </template>

  <template match="ip:ipv4/ip:mtu">
    <text> mtu </text>
    <call-template name="VALUE-NL"/>
  </template>

  <template match="ip:ipv6/ip:address">
    <text> ipv6 address </text>
    <value-of select="concat(ip:ip,'/',ip:prefix-length,$NL)"/>
  </template>

  <template match="ip:autoconf">
    <apply-templates/>
  </template>

  <template match="ip:create-global-addresses">
    <if test=". = 'true'">
      <text> ipv6 address autoconfig </text>
      <value-of select="$NL"/>
    </if>
  </template>

  <template match="ip:dup-addr-detect-transmits">
    <if test=". != 1">
      <text> ipv6 nd dad attempts </text>
      <call-template name="VALUE-NL"/>
    </if>
  </template>

</stylesheet>