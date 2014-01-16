<?xml version="1.0"?>

<!-- Program name: bird-global.xsl

Copyright © 2014 by Ladislav Lhotka, CZ.NIC <lhotka@nic.cz>

Translates NETCONF "get-config" replies to BIRD configuration.
This stylesheet handles global BIRD configuration.

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

  <template match="bird:bird-config">
    <apply-templates/>
  </template>

  <template match="bird:logging">
    <apply-templates select="bird:log"/>
  </template>

  <template match="bird:log">
    <call-template name="stmt-leaf">
      <with-param name="kw">log</with-param>
      <with-param name="arg">
	<call-template name="value-or-default">
	  <with-param name="nodeset" select="bird:destination"/>
	  <with-param name="dflt">stderr</with-param>
	</call-template>
	<text> </text>
	<call-template name="value-or-default">
	  <with-param name="nodeset" select="bird:log-classes"/>
	  <with-param name="dflt">all</with-param>
	</call-template>
      </with-param>
    </call-template>
  </template>

  <template match="bird:log/bird:destination" mode="value">
    <call-template name="value-or-default">
      <with-param name="nodeset"
		  select="bird:stderr|bird:file|bird:syslog"/>
      <with-param name="dflt">stderr</with-param>
    </call-template>
  </template>

  <template match="bird:stderr" mode="value">
    <text>stderr</text>
  </template>

  <template match="bird:syslog" mode="value">
    <text>syslog</text>
    <if test="bird:host != 'localhost'">
      <value-of select="concat(' name ', bird:host)"/>
    </if>
  </template>

  <template match="bird:file" mode="value">
    <text>"</text>
    <value-of select="."/>
    <text>"</text>
  </template>

  <template match="bird:log-classes" mode="value">
    <choose>
      <when test="bird:all">all</when>
      <when test="bird:class">
	<text>{ </text>
	<for-each select="bird:class">
	  <value-of select="."/>
	  <if test="position() != last()">, </if>
	</for-each>
	<text> }</text>
      </when>
    </choose>
  </template>

  <template match="bird:debugging">
    <apply-templates/>
  </template>

  <template match="bird:commands">
    <call-template name="stmt-leaf">
      <with-param name="kw">debug commands</with-param>
      <with-param name="arg">
	<choose>
	  <when test=". = 'off'">0</when>
	  <when test=". = 'connect'">1</when>
	  <when test=". = 'all'">2</when>
	</choose>
      </with-param>
      <with-param name="dflt" select="0"/>
    </call-template>
  </template>

  <template match="bird:protocols">
    <call-template name="stmt-leaf">
      <with-param name="kw">debug protocols</with-param>
      <with-param name="arg">
	<apply-templates select="bird:global-options"/>
      </with-param>
    </call-template>
  </template>

</stylesheet>
