<?xml version="1.0"?>

<!-- Program name: cisco-ios-routing.xsl

Copyright © 2012 by Ladislav Lhotka, CZ.NIC <lhotka@nic.cz>

Translates NETCONF "get-config" replies to Cisco IOS configuration.
This stylesheet handles the "ietf-routing" YANG module.

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

  <template match="rt:routing-protocol[rt:type='rt:static']">
    <apply-templates
	select="rt:static-routes/v4ur:ipv4/v4ur:route"/>
    <value-of select="concat('!', $NL)"/>
    <apply-templates
	select="rt:static-routes/v6ur:ipv4/v6ur:route"/>
  </template>

  <template match="v4ur:outgoing-interface|v6ur:outgoing-interface
		       |v4ur:next-hop|v6ur:next-hop">
    <value-of select="concat(' ',.)"/>
  </template>

</stylesheet>