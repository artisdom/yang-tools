<?xml version="1.0"?>

<!-- Program name: bird-device.xsl

Copyright © 2013 by Ladislav Lhotka, CZ.NIC <lhotka@nic.cz>

Translates NETCONF "get-config" replies to BIRD configuration.
This stylesheet handles the Device protocol (interface parameters).

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
    xmlns:if="urn:ietf:params:xml:ns:yang:ietf-interfaces"
    xmlns:ip="urn:ietf:params:xml:ns:yang:ietf-ip"
    xmlns:bird="http://www.nic.cz/ns/bird"
    version="1.0">

  <template name="prdev">
    <if test="position()=1">
      <call-template name="stmt-block">
	<with-param name="kw">protocol</with-param>
	<with-param name="arg">device</with-param>
      </call-template>
    </if>
  </template>

  <template match="rt:interfaces" mode="device">
    <choose>
      <when test="$ip-version=4">
	<apply-templates
	    select="bird:scan-time[.!=0]|//if:interfaces/if:interface[
		    if:name=current()/rt:interface/rt:name]/ip:ipv4/
		    ip:address[bird:primary='true']"
	    mode="device"/>
      </when>
      <otherwise>
	<apply-templates
	    select="bird:scan-time[.!=0]|//if:interfaces/if:interface[
		    if:name=current()/rt:interface/rt:name]/ip:ipv6/
		    ip:address[bird:primary='true']"
	    mode="device"/>
      </otherwise>
    </choose>
  </template>

  <template match="bird:scan-time" mode="device">
    <call-template name="prdev"/>
    <call-template name="stmt-leaf">
      <with-param name="level" select="1"/>
      <with-param name="kw">scan time</with-param>
    </call-template>
    <if test="position()=last()">
      <call-template name="close-block"/>
    </if>
  </template>

  <template match="ip:address" mode="device">
    <call-template name="prdev"/>
    <call-template name="stmt-leaf">
      <with-param name="level" select="1"/>
      <with-param name="kw">primary</with-param>
      <with-param name="arg">
	<text>&quot;</text>
	<value-of select="ancestor::if:interface/if:name"/>
	<text>&quot; </text>
	<value-of select="ip:ip"/>
      </with-param>
    </call-template>
    <if test="position()=last()">
      <call-template name="close-block"/>
    </if>
  </template>

</stylesheet>