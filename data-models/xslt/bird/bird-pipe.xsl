<?xml version="1.0"?>

<!-- Program name: bird-pipe.xsl

Copyright © 2013 by Ladislav Lhotka, CZ.NIC <lhotka@nic.cz>

Translates NETCONF "get-config" replies to BIRD configuration.
This stylesheet handles the Pipe protocol (recipient ribs).

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
    version="1.0">

  <template match="rt:rib" mode="pipe">
    <apply-templates select="rt:recipient-ribs/rt:recipient-rib"/>
  </template>

  <template match="rt:recipient-rib">
    <variable name="rib" select="ancestor::rt:rib/rt:name"/>
    <call-template name="stmt-block">
      <with-param name="kw">protocol pipe</with-param>
      <with-param name="arg"
		  select="concat($rib,'-',rt:rib-name)"/>
    </call-template>
    <call-template name="stmt-leaf">
      <with-param name="level" select="1"/>
      <with-param name="kw">table</with-param>
      <with-param name="arg" select="rt:rib-name"/>
      <with-param name="dflt" select="$default-rib/rt:name"/>
    </call-template>
    <call-template name="stmt-leaf">
      <with-param name="level" select="1"/>
      <with-param name="kw">peer table</with-param>
      <with-param name="arg" select="$rib"/>
    </call-template>
    <apply-templates select="rt:filter" mode="pipe"/>
    <call-template name="close-block"/>
  </template>

  <template match="rt:filter" mode="pipe">
    <call-template name="stmt-leaf">
      <with-param name="level" select="1"/>
      <with-param name="kw">import filter</with-param>
    </call-template>
  </template>

</stylesheet>