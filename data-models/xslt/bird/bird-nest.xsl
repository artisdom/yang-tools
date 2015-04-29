<?xml version="1.0"?>

<!-- Program name: bird-nest.xsl

Copyright © 2014 by Ladislav Lhotka, CZ.NIC <lhotka@nic.cz>

Translates NETCONF "get-config" replies to BIRD configuration.
This stylesheet handles common protocol parameters.

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
    xmlns:bird="http://www.nic.cz/ns/bird"
    version="1.0">

  <template match="bird:global-options">
    <choose>
      <when test="bird:off">off</when>
      <when test="bird:all">all</when>
      <when test="bird:type">
	<text>{ </text>
	<for-each select="bird:type">
	  <value-of select="."/>
	  <if test="position() != last()">, </if>
	</for-each>
	<text> }</text>
      </when>
    </choose>
  </template>

  <template match="rt:description">
    <call-template name="indent"/>
    <text>description&#xA;</text>
    <variable name="qchar">"</variable>
    <variable name="prf">
      <call-template name="indent">
	<with-param name="level" select="2"/>
      </call-template>
    </variable>
    <value-of select="concat($prf,$qchar)"/>
    <call-template name="fill-text">
      <with-param name="text">
	<value-of select="normalize-space(.)"/>
	<value-of select="concat($qchar,';&#xA;')"/>
      </with-param>
      <with-param
	  name="length"
	  select="$line-length - string-length($prf) - 1"/>
      <with-param name="prefix" select="concat($prf,' ')"/>
      <with-param name="at-start" select="true()"/>
    </call-template>
  </template>

  <template match="rt:enabled">
    <call-template name="stmt-leaf">
      <with-param name="level" select="1"/>
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

  <template match="rt:rib">
    <variable name="rtbl"
	      select="$rt-root/rt:ribs/rt:rib[rt:name = current()]"/>
    <if test="$rtbl/rt:address-family = $address-family">
      <call-template name="stmt-leaf">
	<with-param name="level" select="1"/>
	<with-param name="kw">table</with-param>
	<with-param name="arg" select="."/>
      </call-template>
    </if>
  </template>

</stylesheet>
