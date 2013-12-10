<?xml version="1.0"?>

<!-- Program name: cisco-ios-ipv4-unicast-routing.xsl

Copyright © 2013 by Ladislav Lhotka, CZ.NIC <lhotka@nic.cz>

Translates NETCONF "get-config" replies to Cisco IOS configuration.
This stylesheet handles the "ietf-ipv4-unicast-routing" YANG module.

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
    version="1.0">

  <template match="v4ur:route">
    <value-of select="concat('ip route ',
			  substring-before(v4ur:dest-prefix,'/'),
			  ' ')"/>
    <call-template name="QMASK">
      <with-param
	  name="mlen" select="substring-after(v4ur:dest-prefix,'/')"/>
    </call-template>
    <apply-templates select="v4ur:outgoing-interface"/>
    <apply-templates select="v4ur:next-hop"/>
    <value-of select="$NL"/>
  </template>

</stylesheet>